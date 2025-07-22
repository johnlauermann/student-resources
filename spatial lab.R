#set up, with census API and relevant libraries
library(tidyverse)
library(tidycensus)
library(dplyr)
library(sf)
library(tigris)
library(moments)
library(tmap)
library(ggplot2)

#sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

#configure tidycensus to work with spatial data
options(tigris_use_cache = TRUE)


#1)a) create choropleth maps of one borough

#basic option using ggplot
#see also https://walker-data.com/tidycensus/articles/spatial-data.html 
manhattan <- get_acs(geography = "tract",
                state = "New York",
                county = "New York",
                variables = "S1701_C03_046E",
                geometry = TRUE,
                year = 2022)
colnames(manhattan)[colnames(manhattan) == "estimate"] = "PovertyRate"

manhattan %>%
  ggplot(aes(fill = PovertyRate)) + 
  geom_sf(color = NA) +
  theme_classic() +
  scale_fill_viridis_c(option = "magma") +  # other options include inferno, plasma, viridis, rocket, mako, turbo; https://ggplot2.tidyverse.org/reference/scale_viridis.html
  labs(title = "Poverty Rate") 

# another option, using the theme_void() parameter to emphasize shorelines
ggplot(manhattan, aes(fill = PovertyRate)) +
  geom_sf() +
  theme_bw() +
  scale_fill_viridis_c(option = "mako") +
  labs(title = "Poverty Rate, 2022")

#third option, erasing water bodies
manhattan_land <- get_acs(geography = "tract",
                          state = "New York",
                          county = "New York",
                          variables = "S1701_C03_046E",
                          year = 2022,
                          geometry = TRUE,
                          cb = FALSE
                         ) %>%
                            st_transform(26918) %>%
                            erase_water(year = 2020)
colnames(manhattan_land)[colnames(manhattan_land) == "estimate"] = "PovertyRate"

ggplot(manhattan_land, aes(fill = PovertyRate)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c(option = "turbo", labels = scales::percent)


## 1)b)  classifying the map
#explore distribution of the data
hist(manhattan$PovertyRate, breaks = 100) 
nrow(manhattan)
quantile(na.omit(manhattan$PovertyRate))
max(na.omit(manhattan$PovertyRate))
min(na.omit(manhattan$PovertyRate))
mean(na.omit(manhattan$PovertyRate))
median(na.omit(manhattan$PovertyRate))
skewness(na.omit(manhattan$PovertyRate))

#example using tmap to classify
#see also https://rpubs.com/erikaaldisa/choroplethmapping
qtm(manhattan, fill = "PovertyRate")
tm_shape(manhattan) + tm_fill("PovertyRate",style = "jenks") 
tm_shape(manhattan) + tm_fill("PovertyRate",style = "equal") 
tm_shape(manhattan) + tm_fill("PovertyRate",style = "quantile") 
tm_shape(manhattan) + tm_fill("PovertyRate",style = "sd") 

#with more parameters
tm_shape(manhattan_land, projection = 26918) + tm_fill("PovertyRate", style = "quantile")


#Question 2 create maps of all NYC
#pulling data for all boroughs
counties <- c("Bronx", "Kings", "Queens", "New York", "Richmond")
fiveboros <- get_acs(geography = "tract",
                state = "New York",
                county = counties,
                variables = "S1701_C03_046E",
                output = "wide",
                geometry = TRUE,
                year = 2022)

#create a Borough name variable
fiveboros$Borough <- ifelse(grepl("Bronx", fiveboros$NAME), "Bronx", NA)
fiveboros$Borough <- ifelse(grepl("Kings", fiveboros$NAME), "Brooklyn", fiveboros$Borough)
fiveboros$Borough <- ifelse(grepl("New York County", fiveboros$NAME), "Manhattan", fiveboros$Borough)
fiveboros$Borough <- ifelse(grepl("Queens", fiveboros$NAME), "Queens", fiveboros$Borough)
fiveboros$Borough <- ifelse(grepl("Richmond", fiveboros$NAME), "Staten Island", fiveboros$Borough)

#map the variable
ggplot(fiveboros, aes(fill = S1701_C03_046E)) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c(option = "inferno", labels = scales::percent) +
  labs(title = "Poverty in New York, 2022", fill = "Poverty Rate")

# facet mapping to show multiple panels
fiveboros %>% 
  ggplot(aes(fill = S1701_C03_046E)) +
  facet_wrap(~Borough) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c(option = "turbo", labels = scales::percent) +
  labs(title = "Poverty in New York, 2022", fill = "Poverty Rate")



# Question 3) Map the same variable for the US
lower48 <- c("Alabama",	"Arizona",	"Arkansas",	"California",	"Colorado",	"Connecticut",	"Delaware",	"Florida",	"Georgia", "Idaho",	"Illinois",	"Indiana",	"Iowa",	"Kansas",	"Kentucky",	"Louisiana",	"Maine",	"Maryland",	"Massachusetts",	"Michigan",	"Minnesota",	"Mississippi",	"Missouri",	"Montana",	"Nebraska",	"Nevada",	"New Hampshire",	"New Jersey",	"New Mexico",	"New York",	"North Carolina",	"North Dakota",	"Ohio",	"Oklahoma",	"Oregon",	"Pennsylvania",	"Rhode Island",	"South Carolina",	"South Dakota",	"Tennessee",	"Texas",	"Utah",	"Vermont",	"Virginia",	"Washington",	"West Virginia",	"Wisconsin",	"Wyoming")
usmap <- get_acs(geography = "county",
                 state = lower48,
                 variables = "S1701_C03_046E", 
                 geometry = TRUE,
                 year = 2022) %>%
                  st_transform(5070)

#add a state overlap to remove Alaska, Hawaii and Puerto Rico
state_overlay <- states(
  cb = TRUE,
  resolution = "20m"
) %>% 
  filter(GEOID != "02" & GEOID != "15" & GEOID != "72" ) 

#map the data
ggplot() +
  geom_sf(data = usmap, aes(fill = estimate)) +
  geom_sf(data = state_overlay, fill = NA, color = "black", size = 0.1) +
  theme_void() +
  scale_fill_viridis_c(option = "turbo", labels = scales::percent) +
  labs(title = "Poverty Rate by County", subtitle = "American Community Survey, 2022 5 year estimates", fill = "Poverty Rate")


#another option using classification for choropleths
tm_shape(usmap, projection = 102008) + tm_fill("estimate", style = "quantile", palette = "magma")
tm_shape(usmap, projection = 3857) + tm_fill("estimate", style = "quantile")
