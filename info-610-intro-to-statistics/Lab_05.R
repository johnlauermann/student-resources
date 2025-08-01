# Lab 5: Simple linear regression

# set up, with census API
## I'll use the same poverty and race variables that I used in Lab 4 (S1701 and B02001, respectively)
library(tidyverse)
library(tidycensus)


## sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

# pull my variables
## find a list of variables here
variablelist <- load_variables(2023, "acs5", cache = TRUE)


## define a vector with variables
variables <- c(
  PovertyRate  = "S1701_C03_046E",
  Pop = "B02001_001E",
  AIAN = "B02001_004E",
  Asian = "B02001_005E",
  Black = "B02001_003E",
  NHPI = "B02001_006E",
  Other = "B02001_007E",
  TwoOrMore = "B02001_008E",
  White = "B02001_002E")

## query the API
data <- get_acs(geography = "county",
                variables = variables,
                output = "wide",
                year = 2022)

## calculate percentages
## you could do it this way, or use within() or the dplyr mutate() functions
data$AIANpct <- (data$AIAN / data$Pop) * 100
data$Asianpct <- (data$Asian / data$Pop) * 100
data$Blackpct <- (data$Black / data$Pop) * 100
data$NHPIpct <- (data$NHPI / data$Pop) * 100
data$Otherpct <- (data$Other / data$Pop) * 100
data$TwoOrMorepct <- (data$TwoOrMore / data$Pop) * 100
data$Whitepct <- (data$White / data$Pop) * 100



# Q1: plot & define a research question
## In my case: Does a lower/higher share of ___ racial group predict lower/higher rates of poverty?
## first create a scatterplot to explore
plot(x = data$Whitepct, 
     y = data$PovertyRate,
     main = "Poverty by Race", 
     xlab = "% White", 
     ylab = "Poverty Rate", 
     col = 'dark red')

## then test for correlation
cor.test(data$Whitepct, data$PovertyRate)


# Q2: define a regression model, interpret coefficients
## first define a linear model
model <- lm(data$PovertyRate ~ data$Whitepct)
model
abline(model)  #add regression line to scatterplot



# Q3: evaluate the model metrics
## summary() will often generate all summary metrics for statistical tests
summary(model)

# this will also work if you want to write it long form
summary(lm(data$PovertyRate ~ data$Whitepct))

