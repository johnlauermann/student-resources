# Lab 8: logistic regression

library(dplyr)
library(ggplot2)
library(leaflet)


# load data
## for this, we'll use the most recent URL to load Furman Center's Subsidized Housing Database
url <- "https://furmancenter.org/files/CoreData/FC_SHD_bbl_analysis_2025-05-13.csv"
data <- read.csv(url) %>%
  filter(prog_j51 == 1 | prog_421a == 1)  # in my case, I only want to compare these kinds of properties

## add one new variable
data$assessed_value_perunit <- data$assessed_value / data$res_units



# Q1: Define and justify your research question
## In my case, what differentiates a 421a vs J51 building? 

## basic data visualizations
boxplot(year_built ~ prog_421a, 
        data = data,
        xlab = "421a Building",
        ylab = "Year Built")

boxplot(res_units ~ prog_421a, 
        data = data,
        xlab = "421a Building",
        ylab = "Residential Units")

boxplot(assessed_value_perunit ~ prog_421a, 
        data = data,
        xlab = "421a Building",
        ylab = "Assessed Value ($ per unit)")

boxplot(net_inc_sqft ~ prog_421a, 
        data = data,
        xlab = "421a Building",
        ylab = "Net Rental Income ($ per sq.ft.)")


## testing correlations, using Kendall's tau b/c data is categorical
cor.test(data$prog_421a, data$year_built, method = "kendall")
cor.test(data$prog_421a, data$res_units, method = "kendall")
cor.test(data$prog_421a, data$assessed_value_perunit, method = "kendall")
cor.test(data$prog_421a, data$net_inc_sqft, method = "kendall")

## now just for fun, a map...
options(viewer = NULL)
map <- leaflet()
map <- addTiles(map)
map <- addProviderTiles(map, "Stadia.StamenToner")

cols <- c("blue", "gray")
map <- addCircleMarkers(map,
                      lng = data$longitude,
                      lat = data$latitude,
                      radius = 1.5,
                      color = cols[data$prog_421a + 1],
                      )
map <- addLegend(map,
               "topright",
               colors = cols,
               labels = c("421a", "J51"),
               title = "Construction-Related Subsidies",
               opacity = 1
)
map


# Q2: define and interpret logistic models with only one variable at a time
## define my predictors
predictors <- c("year_built", "res_units", "net_inc_sqft", "assessed_value_perunit")

## write a loop to test each predictor
for (variable in predictors){
  
  ## build the formula
  formula <- as.formula(paste("prog_421a ~", variable))
  
  ## fit the model
  model <- glm(formula, data = data, family = "binomial")
  
  ## print results
  ### model summary
  print(summary(model))
  
  ### odds ratios
  print(exp(coef(model)))
  
  ### confidence intervals
  print(exp(confint.default(model)))
  
  # analysis of deviance
  print(anova(model, test = "Chisq"))
  
}


# Q3: define and interpret a logistic model with all variables
## define the model
log_model <- glm(prog_421a ~ year_built + res_units + net_inc_sqft + assessed_value_perunit, 
                 data = data, family = "binomial")

## print the results
summary(log_model)

## convert coefficients and confidence intervals to odds-ratio terms
exp(coefficients(log_model))
exp(confint.default(log_model))


## Interpret analysis of deviance table
anova(log_model, "Chisquare")

