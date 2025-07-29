# Lab 8: logistic regression

library(dplyr)
library(ggplot2)
library(leaflet)
library(MASS)
library(RSocrata)


# load data
## for this, we'll use the Socrata API to load data from NYC Open Data using RSocrata 
## we'll then pull data from the NYC Street Tree Census: https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh/about_data

## the library we need is deprecated, so we first have to install it from the developer's GitHub profile (https://github.com/Chicago/RSocrata)
library(devtools)
install_github("Chicago/RSocrata")
library(RSocrata)

## now you need a Socrata API token
## To get one: https://support.socrata.com/hc/en-us/articles/210138558-Generating-App-Tokens-and-API-Keys 
## To query NYC Open Data: https://hwangnyc.medium.com/using-r-to-access-311-service-request-from-nyc-open-data-using-socrata-open-data-api-and-the-83de00327a8c 
token <- "your token here"

## I'm going to pull data for a single species
data <- read.socrata("https://data.cityofnewyork.us/resource/uvpi-gqnh.json?spc_common=sweetgum", app_token = token)

## clean data and calculate outcome variables
data <- na.omit(data)
data <- data %>%
  mutate(
    goodhealth = if_else(health == "Good", 1, 0),
    badhealth = if_else(health == "Good", 0, 1),
    sidewalkdamage = if_else(sidewalk == "Damage", 1, 0),
    guardhelpful = if_else(guards == "Helpful", 1, 0),
    stonedamage = if_else(root_stone == "Yes", 1, 0),
    wiredamage = if_else(trunk_wire == "Yes", 1, 0),
    tree_dbh = as.numeric(tree_dbh), 
    borocode = as.numeric(borocode),
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
    )


# Q1: Define and justify your research question
## In my case, what are predictors of bad health among trees in my chosen species? 

## basic data visualizations
ggplot(data, aes(x= health)) +
  geom_bar(stat = "count")
ggplot(data, aes(x= tree_dbh, fill = health)) +
  geom_bar()
ggplot(data, aes(x= boroname, fill = health)) +
  geom_bar(stat = "count")
ggplot(data, aes(x= sidewalk, fill = health)) +
  geom_bar(stat = "count")
ggplot(data, aes(x= root_stone, fill = health)) +
  geom_bar(stat = "count")
ggplot(data, aes(x= trunk_wire, fill = health)) +
  geom_bar(stat = "count")
ggplot(data, aes(x= trnk_light, fill = health)) +
  geom_bar(stat = "count")

## testing correlations
cor.test(data$badhealth, data$tree_dbh, method = "kendall")
cor.test(data$badhealth, data$sidewalkdamage, method = "kendall")
cor.test(data$badhealth, data$guardhelpful, method = "kendall")
cor.test(data$badhealth, data$stonedamage, method = "kendall")
cor.test(data$badhealth, data$wiredamage, method = "kendall")
cor.test(data$badhealth, data$borocode, method = "kendall")

## now just for fun, a map...
options(viewer = NULL)
map <- leaflet()
map <- addTiles(map)
map <- addProviderTiles(map, "Stadia.StamenToner")

cols <- c("green", "gray")
map <- addCircleMarkers(map,
                      lng = data$longitude,
                      lat = data$latitude,
                      radius = 2.5,
                      color = cols[data$badhealth + 1],
                      )
map <- addLegend(map,
               "topright",
               colors = cols,
               labels = c("Good", "Fair/Poor"),
               title = "Tree Health",
               opacity = 1
)
map



# Q2: define and interpret a logistic model
## define the model
log_model <- glm(badhealth ~ tree_dbh + boroname + sidewalk + guards + root_stone + trunk_wire, 
                 data = data, family = "binomial")

## print the results
summary(log_model)

## convert coefficients and confidence intervals to odds-ratio terms
exp(coefficients(log_model))
exp(confint.default(log_model))



# Q3: verify with stepwise modeling
## See this for more details: https://rstudiodatalab.medium.com/stepwise-logistic-regression-in-r-a-complete-guide-82fcd9e2d389 

## first we have to set up a base model, against which the full  model will be compared
base_model <- glm(badhealth ~ 1, data = data, family = "binomial")

## then we can run forward, backward, and/or stepwise selections
forward_model <- stepAIC(base_model, direction = "forward", scope = log_model)
summary(forward_model)

backward_model <- stepAIC(base_model, direction = "backward", scope = log_model)
summary(backward_model)

both_model <- stepAIC(base_model, direction = "both", scope = log_model)
summary(both_model)
