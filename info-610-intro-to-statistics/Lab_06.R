# Lab 6: Multiple linear regression

library(corrplot)
library(dplyr)
library(olsrr)
library(tidyverse)
library(tidycensus)

# set up with census API
## I'll once again use the poverty variable that I used prior assignments, plus a few others

##sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

## define a vector with variables
variables <- c(
  PovertyRate  = "S1701_C03_046E",
  Bachpct = "S1501_C02_012E",
  Postgradpct = "S1501_C02_013E",
  UnemploymentRate = "S2301_C04_001E",
  MedConRent = "B25058_001E",
  MedHHIncome = "S0501_C01_101E")

## pull data from API
data <- get_acs(geography = "county",
                variables = variables,
                output = "wide",
                year = 2023)


## data cleaning to remove unnecessary variables and omit null values
ls(data)
data <- data %>% select(-c(B25058_001M, S0501_C01_101M, S1501_C02_012M,
                           S1501_C02_013M,S1701_C03_046M, S2301_C04_001M))


# Q1: plot & define a research question
## In my case: What predicts higher poverty rates in US counties?

## generate scatterplots
### create a matrix for analysis
matrix <- data %>% select(-c(GEOID, NAME))
matrix <- na.omit(matrixdata)

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
model1 <- lm(PovertyRate ~ Bachpct + Postgradpct + UnemploymentRate, data = data)
summary(model1)

## add an interaction effect
model2 <- lm(PovertyRate ~ Bachpct + Postgradpct + UnemploymentRate + (Bachpct * UnemploymentRate), data = data)
summary(model2)



# Q3 Use forward, backward, or stepwise modeling
## see this for documentation https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html

## a forward model adds predictor variables one at a time
ols_step_forward_r2(model2)

## a backward model starts with all variables, then removes them one by one
ols_step_backward_r2(model2)

## a both model (stepwise) adds and removes in all possible combinations
ols_step_both_r2(model2)

##or this function will run the possibilities for you
ols_step_all_possible(model2)

