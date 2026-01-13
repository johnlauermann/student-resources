
# Lab 4: ANOVA ------------------------------------------------------------

library(socratadata)  # for Socrata API
library(dplyr)        # for data management
library(stringr)      # for string data management
library(ggplot2)      # for data visualization
library(effectsize)   # for eta squared


# Query from NYC Open Data ------------------------------------------------

## define your API key
soc_api_key_id = "your key here"

## review list of data
dataset_list <- soc_discover(domains = "data.cityofnewyork.us")

## or query the list
dataset_query <- soc_discover(domains = "data.cityofnewyork.us", 
                              query = "zoning")

## read the data, including a query
data <- soc_read(
  "https://data.cityofnewyork.us/City-Government/Primary-Land-Use-Tax-Lot-Output-PLUTO-/64uk-42ks/about_data",
  query = soc_query(where = "borough LIKE 'BK'")
)

## view the metadata
soc_metadata(data)



# Q1: Define and justify your model ---------------------------------------

## In my case, I'll use `assessland` as outcome and zoning type and LLC ownership as treatment. 

# data prep
residential <- data %>%
  filter(str_starts(zonedist1, "R") &
           (assessland > 0) &
           (assesstot > 0) &
           (lotarea > 0) &
           (unitsres > 0) &
           (resarea > 0) & 
           (numfloors > 0)) %>%
  mutate(assessland_persqft = assessland / as.numeric(lotarea),
         assesstot_persqft = assesstot / as.numeric(resarea),
         assessland_perunit = assessland / as.numeric(unitsres),
         assesstot_perunit = assesstot / as.numeric(unitsres),
         LLC = ifelse(str_detect(ownername, "LLC"), 1, 0))


## and just verify it worked
table(residential$LLC)
table(residential$zonedist1)

## and visualize it
ggplot(data = residential, aes(x = zonedist1, y = assessland_persqft)) +
  geom_boxplot() + 
  labs(
    title = "Residential land values in Brooklyn",
    subtitle = "Price per sq.ft., 2025", 
    y = "$/sq.ft",
    x  = "Zoning Type") + 
  theme_minimal()

ggplot(data = residential, aes(x = zonedist1, y = assessland_perunit)) +
  geom_boxplot() + 
  labs(
    title = "Residential land values in Brooklyn",
    subtitle = "Price per unit, 2025", 
    y = "$",
    x  = "Zoning Type") + 
  theme_minimal()

## now test assumptions
### data normally distributed?
ggplot(data = residential, aes(x = assessland_perunit)) +
  geom_histogram(
    binwidth = 10000,
    fill = "red",       
    color = "gray50",  
    na.rm = TRUE) +
  labs(
    title = "Residential land values in Brooklyn",
    x = "$")

### shapiro test (maxes out at 5000 samples)
set.seed(123)
sample <- sample(residential$assessland_perunit, 5000)
shapiro.test(sample)

### fligner test of equal variances
fligner.test(assessland_perunit ~ zonedist1, data = residential)
fligner.test(assessland_perunit ~ LLC, data = residential)




# Q2: Interpret the ANOVA -------------------------------------------------
model1 <- aov(assessland_perunit ~ zonedist1, data = residential)
summary(model1)

model2 <- aov(assessland_perunit ~ zonedist1 * LLC, data = residential)
summary(model2)


# Q3: Post-hoc tests ------------------------------------------------------

## effect sizes
eta_squared(model1)
eta_squared(model2)

## post hoc tests
TukeyHSD(model1)
plot(TukeyHSD(model1))

