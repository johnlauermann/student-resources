
# Lab 5: Linear regression ------------------------------------------------

library(ipumsr)   # for IPUMS API
library(dplyr)    # for data management
library(car)      # for evaluating model assumptions
library(olsrr)    # for stepwise modeling


# Get data from IPUMS -----------------------------------------------------

### register for an account on IPUMS (https://uma.pop.umn.edu/cps/registration/new)
### request an API key from (https://account.ipums.org/api_keys)

## set API key
my_key <- "your key here"
set_ipums_api_key(my_key)


## see list of data
sample_list <- get_sample_info("usa")

## define an extract
extract <- define_extract_micro(
  collection = "usa",
  description = "ACS PUMS Data, 2024",
  samples = c("us2024a"),
  variables = c("STATEFIP", "COUNTYFIP", "PUMA", "METPOP20","DENSITY",
                "SEX", "AGE", "MARST", "RACE", "CITIZEN", "ANCESTR1",
                "INCTOT", "POVERTY", "EMPSTAT", "TRANWORK", "TRANTIME", "EDUC"), 
  data_quality_flags = TRUE)

## submit the API and download results
extract <- submit_extract(extract)
wait_for_extract(cps_extract)
filepath <- download_extract(extract)

## read data
ddi <- read_ipums_ddi(filepath)
data <- read_ipums_micro(ddi)
ls(data)
table(data$ANCESTR1)

# Q1: Define your model ---------------------------------------------------

## This is mostly conceptual. But check for correlations and eventually test assumptions. 

## variables for modeling interest
variables_model <- c("INCTOT", "POVERTY", "EMPSTAT", "TRANWORK", "TRANTIME", 
               "SEX", "AGE", "CITIZEN", "METPOP20", "DENSITY")

## simplify to a matrix
matrix <- data %>%
  select(all_of(variables_model)) %>%
  filter(INCTOT > 0 & 
         ANCESTR1 == 32  # German ancestry, just to cut down the data somehow...
  ) %>%
  mutate(across(
    all_of(variables),
    ~ na_if(.x, 9999999)
  ))

data <- data_raw %>%
  select(all_of(variables)) %>%
  mutate(across(
    all_of(variables),
    ~ifelse(as.character(.x) %in% NA_codes, NA, .x)
  ))
ls(matrix)


# Q2: Interpret regression results ----------------------------------------
## define model
model <- lm(INCTOT ~ ., data = matrix)
summary(model)

##  normality of errors
hist(resid(model))

# homoscedasticity of errors
plot(fitted(model), resid(model))

# absence of outliers
plot(hatvalues(model))
plot(cooks.distance(model))



# Q3: stepwise modeling ---------------------------------------------------
## a forward selection adds predictor variables one at a time 
## this version evaluates each iteration based on how the new variables change R2
forward <- ols_step_forward_r2(model) ## generate and save the modeling
forward  ## print the report
plot(forward)  ## visualize the change in R2 from one step to the next

## a backward selection starts with all variables, then removes them one by one
backward <- ols_step_backward_r2(model)
backward
plot(backward)

## a both selection (stepwise) adds and removes in all possible combinations
both <- ols_step_both_r2(model)
both
plot(both)

## or this function will run the possible indicators, in addition to R2
ols_step_all_possible(model)



# Save your data. We'll use it later in the semester for the Bayesian lab. 
write.csv(x = matrix, file = "lab_05_data.csv")