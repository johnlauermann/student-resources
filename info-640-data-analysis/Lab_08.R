
# Lab 8: spatial data -----------------------------------------------------

library(dplyr)        # for data management
library(stringr)      # for string data management
library(tidycensus)   # for Census API
library(tidyverse)    # dependency for tidycensus
library(tigris)       # for metro region boundaries
library(ggplot2)      # for data visualization
library(sf)           # spatial analysis, for querying data
library(spdep)        # autocorrelation tests
library(spatialreg)   # spatial regression



# Using the Census API ----------------------------------------------------

##sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

## find a list of variables here
## it includes code, description, sampling population, and finest geographic scale of availability
variablelist <- load_variables(2024, "acs5", cache = TRUE)

## define my variables of interest
variables <- c(
  ConRent_agg = "B25060_001E",
  LaborForce_sum = "B23025_003E",
  HHIncome_agg = "B19025A_001E",   
  Households_sum = "B11001_001E",
  HouseUnits_sum = "B25001_001E",
  HouseValue_agg = "B25082_001E",   
  HouseValue_Mortgaged_agg = "B25082_002E",   
  HUNew_under5yrs_sum = "B25034_002E",
  HUNew_6to10yrs_sum = "B25034_003E",
  HUSingleFam_detached_sum = "B25024_002E",
  HUSingleFam_attached_sum = "B25024_003E",
  HUOccupied_sum = "B25002_002E",
  HUOwner_sum = "B25003_002E",
  HURenter_sum = "B25003_003E", 
  HUVacant_sum = "B25002_003E",
  Owner_MovedIn_under1yr_sum = "B25026_003E",
  Owner_MovedIn_2to5yrs_sum = "B25026_004E",
  Owner_MovedIn_5to10yrs_sum = "B25026_005E",
  Poverty_pct = "S1701_C03_046E",
  Renter_MovedIn_under1yr_sum = "B25026_010E",
  Renter_MovedIn_2to5yrs_sum = "B25026_011E",
  Renter_MovedIn_5to10yrs_sum = "B25026_012E",
  Unemployed_sum = "B23025_005E"
)

## get the data
data <- get_acs(geography = "tract",
                state = c("NY", "NJ", "CT", "PA"),
                variables = variables,
                output = "wide", 
                survey = "acs5",
                year = 2024, 
                geometry = TRUE)

## view the geographic data
### well-known text format
data$geometry

### coordinate reference system
st_crs(data)
data <- st_transform(x = data, 
                     crs = 4269)   # a relevant map projection for the region, see https://epsg.io/ 

### basic map
ggplot(data) +
  geom_sf(fill = "gray", color = "black", size = .05) + 
  theme_minimal() +
  labs(title = "Census tracts in the CBSA region")


## now use a bit of GIS to filter only tracts in the New York CBSA
### get the boundaries
cbsa_boundary <- core_based_statistical_areas(cb = TRUE, year = 2024) %>%
  filter(CBSAFP == "35620") 
st_crs(cbsa_boundary)


### clip only those tracts that spatially intersect with CBSA boundaries
data <- st_intersection(data, cbsa_boundary)

### map it just to be sure
ggplot(data) +
  geom_sf(fill = "gray", color = "black", size = .05) + 
  theme_minimal() +
  labs(title = "Census tracts in the CBSA region")

## clean up the data
data_clean <- data %>%
  mutate(
    ConRent_mean = ConRent_agg / HURenter_sum,
    HHIncome_mean = HHIncome_agg / Households_sum,
    HouseValue_mean = HouseValue_agg / HURenter_sum,
    HouseValue_Mortgaged_mean = HouseValue_Mortgaged_agg / HUOwner_sum,
    HUMultiFam_sum = HouseUnits_sum - HUSingleFam_detached_sum + HUSingleFam_attached_sum,
    HUMultiFam_pct = (HUMultiFam_sum / HouseUnits_sum) * 100,
    HUSingleFam_sum = HUSingleFam_detached_sum + HUSingleFam_attached_sum,
    HUSingleFam_pct = (HUSingleFam_sum / HouseUnits_sum) * 100,
    HUNewConstruction_sum = HUNew_under5yrs_sum + HUNew_6to10yrs_sum,
    HUNewConstruction_pct = (HUNewConstruction_sum / HouseUnits_sum) * 100,
    HUOwner_pct = (HUOwner_sum / HouseUnits_sum) * 100,
    HURenter_pct = (HURenter_sum / HouseUnits_sum) * 100,
    HUVacant_pct = (HUVacant_sum / HouseUnits_sum) * 100,
    MovedIn_under10yrs_sum = Owner_MovedIn_under1yr_sum + Owner_MovedIn_2to5yrs_sum +
      Owner_MovedIn_5to10yrs_sum + Renter_MovedIn_under1yr_sum +
      Renter_MovedIn_2to5yrs_sum + Renter_MovedIn_5to10yrs_sum,
    MovedIn_under10yrs_pct = (MovedIn_under10yrs_sum / HUOccupied_sum) * 100,
    Unemployed_pct = (Unemployed_sum / LaborForce_sum) * 100) %>%
  select(GEOID, NAME, ConRent_mean, HHIncome_mean, HouseValue_mean,
    HouseValue_Mortgaged_mean, HUMultiFam_sum, HUMultiFam_pct,
    HUSingleFam_sum, HUSingleFam_pct, HUNewConstruction_sum, HUNewConstruction_pct,
    HUOwner_pct, HURenter_pct, HUVacant_pct, MovedIn_under10yrs_sum,
    MovedIn_under10yrs_pct, Unemployed_pct)



# Q1: interpret choropleth maps -------------------------------------------

# map a variable of interest using ggplot
ggplot(data = data_clean) +  # defines the plot space
  geom_sf(aes(fill = ConRent_mean), color = NA) +  # viz type = map
  scale_fill_distiller(palette = "Reds", # define color fill
                       direction = 1,  # ramp from light to dark
                       name = "Rent ($)",  # label the legend
                       na.value = "gray90") + # color for nulls 
  labs(          
    title = "Median Contract Rent by Census Tract", # add text
    subtitle = "New York Core-Based Statistical Area, 2023",
    caption = "Source: ACS 5 Year Estimates"
  ) + 
  theme_minimal()  # choose a theme


# or map at a different scale
## query NYC tracts, and erase shoreline overlaps
nyc <- data_clean %>%
  filter(str_detect(NAME, "Bronx County|Kings County|New York County|Queens County|Richmond County"))

ggplot(data = nyc) +  
  geom_sf(aes(fill = ConRent_mean), color = NA) +  
  scale_fill_distiller(palette = "Reds", 
                       direction = 1,  
                       name = "Rent ($)",  
                       na.value = "gray90") + 
  labs(title = "Median Contract Rent by Census Tract",
       subtitle = "New York City, 2023",
       caption = "Source: ACS 5 Year Estimates") + 
  theme_minimal()



# Q2: spatial autocorrelation ---------------------------------------------

## drop nulls in variable of interest
rent_data <- data_clean %>% 
  filter(!is.na(ConRent_mean))


## create spatial weights
neighborlist <- poly2nb(pl = rent_data, # polygons
                        queen = TRUE)   # concept for matrix

listweights <- nb2listw(neighbours = neighborlist, # neighborhoods
                        style = "W",
                        zero.policy = TRUE)

## global moran's I
global_moran <- moran.test(x = rent_data$ConRent_mean, 
                           listw = listweights, 
                           randomisation = TRUE,
                           alternative = "greater",
                           na.action = na.omit,
                           zero.policy = TRUE)
print(global_moran)

moran.plot(x = rent_data$ConRent_mean, 
           listw = listweights,
           zero.policy = FALSE)



# Q3: spatial lag regression ----------------------------------------------
## define a formula
formula <- as.formula(ConRent_mean ~ HHIncome_mean + HouseValue_mean + 
                        HouseValue_Mortgaged_mean + HUMultiFam_pct + HUNewConstruction_pct + HURenter_pct + 
                        HUVacant_pct + MovedIn_under10yrs_pct + Unemployed_pct)
formula

## use in OLS
ols_model <- lm(formula = formula, data = rent_data)
summary(ols_model)


## fit the model
spatial_model <- lagsarlm(formula = formula, 
                          data = rent_data, 
                          listw = listweights, 
                          zero.policy = TRUE, 
                          na.action = na.omit)
names(spatial_model)
summary(spatial_model)

