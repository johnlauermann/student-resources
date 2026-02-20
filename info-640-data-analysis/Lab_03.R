
# Lab 3: cluster analysis -------------------------------------------------

library(dplyr)        # for data management
library(tidycensus)   # for Census API
library(tidyverse)    # dependency for tidycensus
library(tigris)       # for metro region boundaries
library(ggplot2)      # for data visualization
library(sf)           # spatial analysis, for querying data
library(factoextra)   # for cluster visualizations


# Using the Census API ----------------------------------------------------

##sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

## find a list of variables here
## it includes code, description, sampling population, and finest geographic scale of availability
variablelist <- load_variables(2023, "acs5", cache = TRUE)

## define my variables of interest (same that I used for Lab 2)
variables <- c(
  Adults_sum = "B15003_001E",
  ConRent_agg = "B25060_001E",
  Ed_Bach_sum = "B15003_022E",
  Ed_Masters_sum = "B15003_023E",
  Ed_ProfDegree_sum = "B15003_024E",
  Ed_Doctorate_sum = "B15003_025E",
  FamilyHH_sum = "B11001_002E",
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
  Unemployed_sum = "B23025_005E",
  Population_sum = "B02001_001E",
  Asian_sum = "B02001_005E",
  Black_sum = "B02001_003E",
  Hispanic_sum = "B03001_003E",
  White_sum = "B02001_002E"
)

## query the Census API
data <- get_acs(geography = "tract",
                state = c("NY", "NJ", "CT", "PA"),
                variables = variables,
                output = "wide", 
                survey = "acs5",
                year = 2023, 
                geometry = TRUE)

## now use a bit of GIS to filter only tracts in the New York CBSA
### map the raw data to verify
ggplot(data) +
  geom_sf(fill = "gray", color = "black", size = .05) + 
  coord_sf(crs = 26918) +
  theme_minimal() +
  labs(title = "Census tracts in the original sample")

### get the CBSA boundaries
cbsa_boundary <- core_based_statistical_areas(cb = TRUE, year = 2023) %>%
  filter(CBSAFP == "35620")

### clip only those tracts that spatially intersect with CBSA boundaries
data <- st_intersection(data, cbsa_boundary)

### map it just to be sure
ggplot(data) +
  geom_sf(fill = "gray", color = "black", size = .05) + 
  coord_sf(crs = 26918) +
  theme_minimal() +
  labs(title = "Census tracts in the CBSA region")

## finally, clean up the data
data_clean <- data %>%
  mutate(
    Bach_pct = ((Ed_Bach_sum + Ed_Masters_sum + Ed_ProfDegree_sum + Ed_Doctorate_sum) / Adults_sum) * 100,
    ConRent_mean = ConRent_agg / HURenter_sum,
    FamilyHH_pct = (FamilyHH_sum / Households_sum) * 100,
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
    Unemployed_pct = (Unemployed_sum / LaborForce_sum) * 100,
    Asian_pct = (Asian_sum / Population_sum) * 100,
    Black_pct = (Black_sum / Population_sum) * 100,
    Hispanic_pct = (Hispanic_sum / Population_sum) * 100,
    White_pct = (White_sum / Population_sum) * 100) %>%
  select(
    Bach_pct, ConRent_mean, FamilyHH_pct, HHIncome_mean, HouseValue_mean,
    HouseValue_Mortgaged_mean, HUMultiFam_sum, HUMultiFam_pct,
    HUSingleFam_sum, HUSingleFam_pct, HUNewConstruction_sum, HUNewConstruction_pct,
    HUOwner_pct, HURenter_pct, HUVacant_pct, MovedIn_under10yrs_sum,
    MovedIn_under10yrs_pct, Unemployed_pct, Asian_pct, Black_pct,
    Hispanic_pct, White_pct) %>%
  st_drop_geometry()



# Q1: Elbow and cluster plots ---------------------------------------------

## elbow plot
fviz_nbclust(x = na.omit(data_clean), kmeans, method = "wss")

## create a loop to generate clusters
kvalues <- 2:10
for(k in kvalues){
  kmodel <- paste0("k", k)
  kmodel <- assign(kmodel, kmeans(na.omit(data_clean), centers = k, nstart = 25))
  print(kmodel)
}

## and see the plots
for(k in kvalues){
  model_name <- paste0("k", k)
  kmodel <- get(model_name)
  
  print(fviz_cluster(object = kmodel, 
                     data = na.omit(data_clean),
                     centers = k, 
                     nstart = 25,
                     geom = "point",
                     main = paste0("Cluster plot for k=", k)
                     )
  )
}


# Q2: Dendrograms -------------------------------------------------------------

## create a matrix
matrix <- dist(x = na.omit(data_clean), method = "euclidean")


## explore linkage options
clust.single <- hclust(matrix, method = "single")
clust.average <- hclust(matrix, method = "average")
clust.complete <- hclust(matrix, method = "complete")
clust.ward <- hclust(matrix, method = "ward.D")

## plot dendrograms
plot(clust.single)
plot(clust.average)
plot(clust.complete)
plot(clust.ward)



# Q3: Interpret cluster metrics ---------------------------------------------
## I'll use k=4 given my data structure. You'll need to choose your own. 

## see available components
names(k4)

## see entire report
k4

## see the invidual component options
names(k4)

## call individual components
k4$size
k4$centers
k4$betweenss
k4$withinss
k4$totss
