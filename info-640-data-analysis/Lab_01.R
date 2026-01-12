
# INFO 640 Lab 1: EDA with inferential tests ------------------------------


library(dplyr)        # for data management
library(tidyverse)    # dependency for tidycensus
library(tidycensus)   # for Census API
library(tigris)       # for metro region boundaries
library(ggplot2)      # for data visualization
library(sf)           # spatial analysis, for querying data
library(kim)          # for skewness & kurtosis metrics
library(lsr)          # for Cohen's D


# Using the Census API ----------------------------------------------------

##sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

## find a list of variables here
## it includes code, description, sampling population, and finest geographic scale of availability
variablelist <- load_variables(2023, "acs5", cache = TRUE)

## define my variables of interest
variables <- c(
  ConRent_agg                = "B25060_001E",   
  HouseUnits_sum             = "B25001_001E",
  HURenter_sum               = "B25003_003E",
  HUVacant_sum               = "B25002_003E",
  )

## get the data
data <- get_acs(geography = "tract",
                state = c("NY", "NJ", "CT", "PA"),
                variables = variables,
                output = "wide", 
                survey = "acs5",
                year = 2023, 
                geometry = TRUE)

## now use a bit of GIS to filter only tracts in the New York CBSA
### get the boundaries
cbsa_boundary <- core_based_statistical_areas(cb = TRUE, year = 2023) %>%
  filter(CBSAFP == "35620")

### clip only those tracts that spatially intersect with CBSA boundaries
data <- st_intersection(x = cbsa_boundary, y = data)

### map it just to be sure
ggplot(data) +
  geom_sf(fill = "gray", color = "black", size = .05) + 
  coord_sf(crs = 26918) +
  theme_minimal() +
  labs(title = "Census tracts in the CBSA region")

## finally, clean up the data
data <- within(data, {
  ConRent_mean <- ConRent_agg / HURenter_sum
  HUVacant_pct <- (HUVacant_sum / HouseUnits_sum) * 100
  PlaceType <- ifelse(grepl("Bronx County|Kings County|New York County|
                             Queens County|Richmond County", NAME), "City", "Suburb")
  })



# Q1: Histograms and scatterplots -----------------------------------------

## my variable of interest (contract rent)
ggplot(data = data, aes(x = ConRent_mean)) +
  geom_histogram(bins = 100, fill = "darkgreen", color = "lightgray") +
  theme_minimal() +
  labs(title = "Mean Contract Rent", 
       subtitle = "In the New York City Metro Region, 2023",
       x = "$/month", 
       y = "# Census Tracts")

## and its relation to vacancy rates
ggplot(data = data, aes(x = HUVacant_pct, y = ConRent_mean)) +
  geom_point(color = "blue", alpha = .5, na.rm = TRUE) + 
  theme_minimal() +
  labs(title = "Rent vs. Vacancy",
       subtitle = "In the NYC Metro Region",
       y = "Rent ($)",
       x = "% Vacant Units")

## now assess their descriptive stats
variable = "ConRent_mean"
descriptives <- data %>%
  summarize(
    mean = mean(ConRent_mean, na.rm = TRUE), 
    median = median(ConRent_mean, na.rm = TRUE),
    sd = sd(ConRent_mean, na.rm = TRUE),
    skewness = skewness(ConRent_mean, type = "pearson_2"),
    kurtosis = kurtosis(ConRent_mean)
    )



# Correlation analysis ----------------------------------------------------

cor.test(data$ConRent_mean, data$HUVacant_pct, method = "pearson")



# T-tests -----------------------------------------------------------------

# do groups have equal variance?
var.test(x = data$ConRent_mean[data$PlaceType == "City"],
         y = data$ConRent_mean[data$PlaceType == "Suburb"])

## if yes
t.test(x = data$ConRent_mean[data$PlaceType == "City"],
       y = data$ConRent_mean[data$PlaceType == "Suburb"], 
       var.equal = TRUE)

## if not
t.test(x = data$ConRent_mean[data$PlaceType == "City"],
       y = data$ConRent_mean[data$PlaceType == "Suburb"], 
       var.equal = FALSE)

## and cohen's d for effect sizes
cohensD(x = data$ConRent_mean[data$PlaceType == "City"],
        y = data$ConRent_mean[data$PlaceType == "Suburb"])
