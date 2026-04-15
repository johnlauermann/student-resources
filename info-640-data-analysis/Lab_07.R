
# Lab 7: bayesian methods -------------------------------------------------

library(dplyr)      # for data management
library(ggplot2)    # for data visualization
library(rstanarm)   # for Bayesian regression
library(BayesFactor) # for Bayesian factors
library(olsrr)      # for stepwise modeling

# load data used in linear regression lab
data <- read.csv("lab_05_data.csv") %>%
  filter(INCTOT > 1000) %>%
  mutate(across(
    everything(),
      ~ na_if(.x, 9999999)
      ))

# plot some just to check
ggplot(data = data, aes(x = INCTOT)) +
  geom_histogram(binwidth = 5000,
                 na.rm = TRUE) +
  labs(title = "Total Personal Income",
       subtitle = "PUMS 2024 Data",
       x = "$",
       y = "Individuals") + 
  theme_minimal() 



# Q1: correlation with Bayes factors --------------------------------------

## scale the predictors, data to simplify
data <- data %>%
  mutate(across(c(AGE, DENSITY), scale))

## standard Pearson's R
cor.test(x = data$INCTOT, y = data$AGE)
cor.test(x = data$INCTOT, y = data$SEX, method = "spearman")
cor.test(x = data$INCTOT, y = data$CITIZEN)
cor.test(x = data$INCTOT, y = data$DENSITY)

## Bayes factor
correlationBF(x = data$INCTOT, y = data$AGE)
correlationBF(x = data$INCTOT, y = data$CITIZEN)
correlationBF(x = data$INCTOT, y = data$DENSITY)



# Q2: bayesian linear regression ------------------------------------------

## conventional model for comparison
ols_model <- lm(INCTOT ~ AGE + SEX + CITIZEN + DENSITY, data = data)
summary(ols_model)


## define bayseian model
bayes_model <- stan_glm(INCTOT ~ AGE + SEX + CITIZEN + DENSITY, # formula
                        data = data,  # data frame
                        family = gaussian(), # linear regression
                        chains = 3,   # how many Markov chains? more = better diagnostics
                        iter = 2000,  # how many samples per chain?
                        seed = 123)   # sets the random seed for reproducibility

## view the model
summary(bayes_model)

## view the priors
prior_summary(bayes_model)

## plot coefficient estimates
parameters <- names(coef(bayes_model))

for (coefficient in parameters){
  plot <- plot(bayes_model,
       plotfun = "areas",
       pars = coefficient,
       include = TRUE, 
       prob = .95, 
       border = "black") +
    theme_minimal() +
    labs(title = paste("Posterior Distribustion of", coefficient, "with 95% Credible Interval"),
         x = "Parameter Value",
         y = "Density")
  print(plot)
}


# Q3: baysian model selection ---------------------------------------------

## forward selection
bic_forward <- ols_step_forward_sbic(ols_model)
bic_forward
plot(bic_forward)

## both directions
bic_both <- ols_step_both_sbic(ols_model)
bic_both
plot(bic_both)
