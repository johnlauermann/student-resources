#install and import relevant libraries
library(tidyverse)
library(tidycensus)
library(dplyr)

#set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

#sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

#query metadata
metadata <- load_variables(2023, "acs5")
view(metadata)

#define variables
variables <- c(Population = "B01003_001E",
               PovertyRate  = "S1701_C03_046E",
               UnemploymentRate = "S2301_C04_001E")

#pull by state (API defaults to state level queries)
NYdata <- get_acs(geography = "tract",
                  state = "New York",
                  variables = variables,
                  output = "wide",
                  year = 2023) 

#filter out the variables you want
NYdata <- NYdata %>%
  select(GEOID, Population, PovertyRate, UnemploymentRate)


#Other customizations
##pull by county query (within a state)
counties <- c("Bronx", "Kings", "New York", "Queens", "Richmond")
NYCdata <- get_acs(geography = "tract",
                  state = "New York",
                  county = counties,
                  variables = variables,
                  output = "wide",
                  year = 2023)

##pull multiple states
states <- c("Connecticut", "New Jersey", "New York")
tristatedata <- get_acs(geography = "tract",
                   state = states,
                   variables = variables,
                   output = "wide",
                   year = 2023)


#save to CSV
write.csv(NYdata, "NYdata.csv")
