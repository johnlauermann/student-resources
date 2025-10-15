# Lab 6: Multiple linear regression

library(corrplot)     # for a correlation matrix
library(dplyr)        # for data management
library(olsrr)        # for stepwise model building
library(tidyverse)    # dependency for tidycensus
library(tidycensus)   # for Census API

# set up with census API
## I'll once again use the poverty variable that I used prior assignments, plus a few others

##sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

## find a list of variables here
## it includes code, description, sampling population, and finest geographic scale of availability
variablelist <- load_variables(2023, "acs5", cache = TRUE)

## define a vector with variables of interest
variables <- c(
  PovertyRate  = "S1701_C03_046E",
  Bachpct = "S1501_C02_012E",
  Postgradpct = "S1501_C02_013E",
  UnemploymentRate = "S2301_C04_001E",
  MedConRent = "B25058_001E",
  MedHHIncome = "S0501_C01_101E")

## pull data from the Census API
data <- get_acs(geography = "county",    # all counties in the US
                variables = variables,   # all variables defined in the list above
                output = "wide",         # outputs a table where each row is a county and each column a variable
                year = 2023)             # or whatever is the most recent year available 


## data cleaning to remove unnecessary variables and omit null values
ls(data) # lists the variables
data <- data %>% 
  select(-c(B25058_001M, S0501_C01_101M, S1501_C02_012M, 
            S1501_C02_013M,S1701_C03_046M, S2301_C04_001M))  # select() uses - to remove columns


# Q1: plot & define a research question
## In my case: What predicts higher poverty rates in US counties?

## generate scatterplots
### create a matrix for analysis
matrix <- data %>% select(-c(GEOID, NAME))   # drops the variable we don't want to correlate
matrix <- na.omit(matrix)                # removes rows with null values

### use pairs() to create the visualization
pairs(matrix, main = "Potential Predictors of Poverty")
pairs(matrix, main = "Potential Predictors of Poverty", upper.panel = NULL) ## this removes the duplication

### correlation tests
cor.test(data$PovertyRate, data$Bachpct)
cor.test(data$PovertyRate, data$Postgradpct)
cor.test(data$PovertyRate, data$UnemploymentRate)
cor.test(data$PovertyRate, data$MedHHIncome)



# Q2: define a regression model, interpret coefficients
## define the basic mode
model_1 <- lm(PovertyRate ~ Bachpct + Postgradpct + UnemploymentRate, data = data)  # define the model
summary(model_1)  # print results

## add an interaction effect
model_2 <- lm(PovertyRate ~ Bachpct + Postgradpct + UnemploymentRate + (Bachpct * UnemploymentRate), data = data)
summary(model_2)



# Q3 Use forward, backward, or stepwise modeling
## see this for documentation https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html

## a forward selection adds predictor variables one at a time 
## this version evaluates each iteration based on how the new variables change R2
ols_step_forward_r2(model_2)

## a backward selection starts with all variables, then removes them one by one
ols_step_backward_r2(model_2)

## a both selection (stepwise) adds and removes in all possible combinations
ols_step_both_r2(model_2)

## or this function will run the possibible indicators, in addition to R2
ols_step_all_possible(model_2)

