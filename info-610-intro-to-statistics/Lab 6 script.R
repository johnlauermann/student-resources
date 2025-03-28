#set up, with census API
# I'll use the same poverty and race variables that I used prior assignments

library(tidyverse)
library(tidycensus)
library(dplyr)
library(corrplot)

#sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

#define a vector with variables
variables <- c(
  PovertyRate  = "S1701_C03_046E",
  Bachpct = "S1501_C02_012E",
  Postgradpct = "S1501_C02_013E",
  UnemploymentRate = "S2301_C04_001E",
  MedConRent = "B25058_001E",
  MedHHIncome = "S0501_C01_101E")

#pulling my variables
data <- get_acs(geography = "county",
                variables = variables,
                output = "wide",
                year = 2022)


#data cleaning to remove unnecessary variables and omit null values
ls(data)
data <- data %>% select(-c(B25058_001M, S0501_C01_101M, S1501_C02_012M,
                           S1501_C02_013M,S1701_C03_046M, S2301_C04_001M))
matrixdata <- data %>% select(-c(GEOID, NAME))
matrixdata <- na.omit(matrixdata)


#Q1: plot & define a research question
# In my case: What predicts higher poverty rates in US counties?

# generate scatterplots
pairs(matrixdata, main = "Scatterplots of Poverty Variables")
pairs(matrixdata, main = "Scatterplots of Poverty Variables", upper.panel = NULL)

#correlation tests
cor.test(data$PovertyRate, data$Bachpct)
cor.test(data$PovertyRate, data$Postgradpct)
cor.test(data$PovertyRate, data$UnemploymentRate)
cor.test(data$PovertyRate, data$MedHHIncome)

#correlation matrix
cor(matrixdata)
cormatrix <- cor(matrixdata)
corrplot(cormatrix)


#Q2: define a regression model, interpret coefficients
model1 <- lm(PovertyRate ~ Bachpct + Postgradpct + UnemploymentRate, data = data)
summary(model1)

# adding an interaction effect
model2 <- lm(PovertyRate ~ Bachpct + Postgradpct + UnemploymentRate + (Bachpct * UnemploymentRate), data = data)
summary(model2)

#3 Use forward, backward, or stepwise modeling
## see this for documentation https://cran.r-project.org/web/packages/olsrr/vignettes/variable_selection.html
library(olsrr)
ols_step_forward_r2(model2)
ols_step_backward_r2(model2)
ols_step_both_r2(model2)
ols_step_all_possible(model2)

