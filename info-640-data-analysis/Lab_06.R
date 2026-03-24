
# Lab 6: generalized linear models ----------------------------------------

# first, install library from GitHub
library(devtools)
install_github(repo = "https://github.com/pewresearch/pewmethods")

library(haven)      # for reading SPSS data
library(pewmethods) # for pew data
library(dplyr)      # for data management
library(survey)     # for weighting survey data
library(broom)      # for incident rate ratios
library(ggplot2)    # for data visualization


# Create an account at Pew Research. Download data, then load.
# I'll use the 23-24 Religious Landscape Study 
# https://www.pewresearch.org/dataset/2023-24-religious-landscape-study-rls-dataset/

## load data
data_raw <- read_spss("2023-24 RLS Public Use File Feb 19.sav")

## define variables of interest
variables <- c(
  "HAPPY", "SATIS_A", "SATIS_B", "RELPER", "SPIRPER",
  "ATTNDPERRLS", "RELIMP", "PRAY", "SECMEMB", "MARITAL", "CHILDREN",
  "BIRTHDECADE", "GENDER", "RACECMB", "HISP", "EDUCREC", "EMPLSIT",
  "INC_SDT1", "INTFREQ", "YEARLOCREC", "REG", "WEIGHT")

## common codes used to designate null values
NA_codes <- c("99", "999", "9999", "900000")

## filter data 
data <- data_raw %>%
  select(all_of(variables)) %>%
  mutate(across(
    all_of(variables),
    ~ifelse(as.character(.x) %in% NA_codes, NA, .x)
  ))

## and clean up to include variable labels
### define labels
variable_labels <- sapply(X = data_raw, 
                          FUN = function(x) attr(x, "label"))

### function to label data frames
apply_labels <- function(data, labels) {
  for (column in intersect(names(data), names(labels))) {
    attr(data[[column]], "label") <- labels[[column]]
  }
  data
}

### apply the labeling function
data <- apply_labels(data, variable_labels)


## test just to see
ls(data)
get_totals(var = "HAPPY", df = data, wt = "WEIGHT")



# Q1: define and justify model --------------------------------------------

# This is mostly conceptual, but check assumptions and distribution of data.
# In my case, I'll test potential predictors of happiness (HAPPY) using logistic regression. 
# And potential predictors of internet usage frequency (INTFREQ) with Poisson regression. 



# Q2: Logistic regression ----------------------------------------

## create a binary variable
data$HAPPY_binary <- ifelse(data$HAPPY == 1 | data$HAPPY == 2, 1, 0)
data <- data %>%
  mutate(HAPPY_binary = haven ::as_factor(HAPPY_binary))
table(data$HAPPY_binary)

## and identify predictor variables
predictors <- setdiff(variables, c("HAPPY", "WEIGHT"))
predictors

## define a formula
logistic_formula <- as.formula(paste("as.factor(HAPPY_binary) ~", paste(predictors, collapse = " + ")))
logistic_formula 

## fit a basic model with raw data
logistic_model <- glm(formula = logistic_formula, data = data, family = "binomial")
summary(logistic_model)

## and compare with weighted survey data
design <- svydesign(ids = ~1, weights = ~WEIGHT, data = data)
logistic_model_weighted <- svyglm(
  formula = logistic_formula,
  design = design,
  family = quasibinomial())
summary(logistic_model_weighted)

## convert coefficients and confidence intervals to odds-ratio terms
exp(coefficients(logistic_model))
exp(confint.default(logistic_model))
exp(coefficients(logistic_model_weighted))
exp(confint.default(logistic_model_weighted))


## Interpret analysis of deviance table
anova(logistic_model, "Chisquare")




# Q3: Poisson regression ------------------------------------------------------

## create a count variable
data <- data %>%
  mutate(
    INTFREQ_count = case_when(
      INTFREQ == 1 ~ 21,   # 3x/day
      INTFREQ == 2 ~ 7,    # 1x/day
      INTFREQ == 3 ~ 3,    # few times/week
      INTFREQ == 4 ~ 1,    # few times/month
      INTFREQ == 5 ~ 0,    # less often
      TRUE ~ NA_real_
    )
  )
table(data$INTFREQ_count)


## and identify predictor variables
predictors <- setdiff(variables, c("INTFREQ", "INTFREQ_count", "WEIGHT"))
predictors

## define formula
poisson_formula <- as.formula(paste("INTFREQ_count ~", paste(predictors, collapse = " + ")))
poisson_formula

## fit a basic model with raw data
poisson_model <- glm(formula = poisson_formula, data = data, family = "poisson")
summary(poisson_model)

## and compare with weighted survey data
design <- svydesign(ids = ~1, weights = ~WEIGHT, data = data)
poisson_model_weighted <- svyglm(
  formula = poisson_formula,
  design = design,
  family = quasipoisson())
summary(poisson_model_weighted)

## convert coefficients to incident ratios
tidy(x = poisson_model, conf.int = TRUE, exponentiate = TRUE)
tidy(x = poisson_model_weighted, conf.int = TRUE, exponentiate = TRUE)

## Interpret analysis of deviance table
anova(poisson_model, "Chisquare")

