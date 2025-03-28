#set up, with census API
# I'll use the same poverty and race variables that I used in Assignment 4

library(tidyverse)
library(tidycensus)

#sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

#pulling my variables

test <- get_acs(geography = "county", 
                variables = "S1701_C03_046E",
                year = 2022)

variablelist <- load_variables(2022, "acs5", cache = TRUE)
variablelist

#define a vector with variables
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

data <- get_acs(geography = "county",
                variables = variables,
                output = "wide",
                year = 2022)


data$AIANpct <- (data$AIAN / data$Pop) * 100
data$Asianpct <- (data$Asian / data$Pop) * 100
data$Blackpct <- (data$Black / data$Pop) * 100
data$NHPIpct <- (data$NHPI / data$Pop) * 100
data$Otherpct <- (data$Other / data$Pop) * 100
data$TwoOrMorepct <- (data$TwoOrMore / data$Pop) * 100
data$Whitepct <- (data$White / data$Pop) * 100


#Q1: plot & define a research question
# In my case: Does a lower/higher share of ___ racial group predict lower/higher rates of poverty?
plot(data$Whitepct, data$PovertyRate,
     main = "Poverty by Race", 
     xlab = "% White", 
     ylab = "Poverty Rate", 
     col = 'dark red')
cor.test(data$Whitepct, data$PovertyRate)

#Q2: define a regression model, interpret coefficients
lm(data$PovertyRate ~ data$Whitepct)
model <- lm(data$PovertyRate ~ data$Whitepct)
abline(model)  #add regression line to scatterplot

#Q3: evaluate the model metrics
summary(model)  #goodness of fit metrics
summary(lm(data$PovertyRate ~ data$Whitepct))

