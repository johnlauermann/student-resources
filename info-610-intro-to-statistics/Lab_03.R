# Lab 3: Histograms & distributions

# we'll use these packages
library(dplyr)  ## for data management 
library(here)  ## for creating relative file paths. It's a great resource for project-oriented workflows and reproducibility. 
library(moments)  ## for calculating skewness and kurtosis metrics

# set up your working directory
## to use here, save your R script wherever you want, then open it. The following will map to whatever directory in which your script is saved. 
here::i_am("Lab_03.r") 


# add your data
## Get data from the most recent year of the National Household Travel Survey (https://nhts.ornl.gov/).
## I'm going to use the trips data, a record of individual trips taken by respondents. 
trips <- read.csv("tripv2pub.csv") %>%
  mutate(across(everything(), ~ ifelse(.x < 0, NA, .x)))  # this replaces negative values with nulls, since that's how the survey designates non-reponses. 


# Part 1: Present tables that summarize trip time or distance
trips$MinuteCategory <- ifelse(trips$TRVLCMIN >= 15, 1, 0) ## I created a new variable that classifies trips as more or less than 15 minutes
table(trips$TDWKND, trips$MinuteCategory) ## crosstabulated frequency table
prop.table(table(trips$TDWKND, trips$TRVLCMIN)) ## crosstabulated density table


# Part 2: Design histograms that descriptive trip time or distance by a category (I used the weekend/workweek variable)
## one option is to use the hist() funciton in base R
## partition the plot space into one row and two columns
par(mfrow = c(1,2))  

## then create the histograms 
hist(trips$TRVLCMIN[trips$TDWKND == 2], 
     breaks = 250,  ## this parameter sets the bin sizes. More breaks = more bins, each covering a smaller range of the y axis. 
     xlim = range(0,120),
     main = "Work Week", xlab = "Minutes", ylab = "Trips",
     col = "yellow")
hist(trips$TRVLCMIN[trips$TDWKND == 1], 
     breaks = 250,
     xlim = range(0,120),
     main = "Weekend", xlab = "Minutes", ylab = "Trips", 
     col = "purple")


# Part 3: Interpret distributions using mean, median, standard deviation, and variance
## basic descriptives
mean(trips$TRVLCMIN[trips$TDWKND == 1], na.rm = TRUE)
median(trips$TRVLCMIN[trips$TDWKND == 1], na.rm = TRUE)
sd(trips$TRVLCMIN[trips$TDWKND == 1], na.rm = TRUE)
var(trips$TRVLCMIN[trips$TDWKND == 1], na.rm = TRUE)  

mean(trips$TRVLCMIN[trips$TDWKND == 2], na.rm = TRUE)
median(trips$TRVLCMIN[trips$TDWKND == 2], na.rm = TRUE)
sd(trips$TRVLCMIN[trips$TDWKND == 2], na.rm = TRUE)
var(trips$TRVLCMIN[trips$TDWKND == 2], na.rm = TRUE)

# or use dplyr to simplify this
summary <- trips %>%
  group_by(TDWKND) %>%
  summarize(mean = mean(TRVLCMIN, na.rm = TRUE),
            median = median(TRVLCMIN, na.rm = TRUE),
            sd = sd(TRVLCMIN, na.rm = TRUE),
            var = var(TRVLCMIN, na.rm = TRUE)) %>%
  mutate(
    TDWKND = case_when(
      TDWKND == 1 ~ "Weekend",
      TDWKND == 2 ~ "Work Week"))
print(summary)

## add mean lines to the histograms
## then create the histograms 
hist(trips$TRVLCMIN[trips$TDWKND == 2], 
     breaks = 250,
     xlim = range(0,120),
     main = "Work Week", xlab = "Minutes", ylab = "Trips",
     col = "yellow")
abline(v = mean(trips$TRVLCMIN[trips$TDWKND == 2], na.rm = TRUE), col = "blue")

hist(trips$TRVLCMIN[trips$TDWKND == 1], 
     breaks = 250,
     xlim = range(0,120),
     main = "Weekend", xlab = "Minutes", ylab = "Trips", 
     col = "purple")
abline(v = mean(trips$TRVLCMIN[trips$TDWKND == 2], na.rm = TRUE), col = "orange")


# Part 4: Calculate the skewness and kurtosis of the distributions
## This calculates Pearson's Coefficient of Skewness. It ranges from -3 to +3, with directionality indicating left/right skew and magnititude quantify the degree of the skew. 
skewness(trips$TRVLCMIN, na.rm = TRUE)
skewness(trips$TRVLCMIN[trips$TDWKND == 1], na.rm = TRUE)
skewness(trips$TRVLCMIN[trips$TDWKND == 2], na.rm = TRUE)

## This calculates Pearson's measure of kurtosis. It ranges around the value of 3, with 3 indicating a normal distribution, <3 indicating spikier distributions (less data in the tails), and >3 indicating flatter distributions (more data in the tails). 
kurtosis(trips$TRVLCMIN, na.rm = TRUE)
kurtosis(trips$TRVLCMIN[trips$TDWKND == 1], na.rm = TRUE)
kurtosis(trips$TRVLCMIN[trips$TDWKND == 2], na.rm = TRUE)

