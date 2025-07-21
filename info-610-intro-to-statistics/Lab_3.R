# Lab 3: Histograms & distributions

# we'll use these packages
## Note the addition of here (https://here.r-lib.org/), used for creating relative file paths. 
## It's a great resource for project-oriented workflows and reproducibility. 
library(dplyr)
library(ggplot2)
library(here)
library(moments)

# set up your working directory
## to use here, save your R script wherever you want, then open it. The following will map to whatever directory in which your script is saved. 
here::i_am("Lab_3.r") 


# add your data
## Get data from the most recent year of the National Household Travel Survey (https://nhts.ornl.gov/).
## I'm going to use the trips data, a record of individual trips taken by respondents. 
trips <- read.csv("tripv2pub.csv")


# Part 1: Present tables that summarize trip time or distance
trips$MinuteCategory <- ifelse(trips$TRVLCMIN >= 15, 1, 0) #I created a new variable that classifies trips as more or less than 15 minutes
table(trips$TDWKND, trips$MinuteCategory) #crosstabulated frequency table
prop.table(table(trips$TDWKND, trips$TRVLCMIN)) #crosstabulated density table


# Part 2: Design histograms that descriptive trip time or distance by a category (I used the weekend/workweek variable)
## one option is to use the hist() funciton in base R
## partition the plot space into one row and two columns
par(mfrow = c(1,2))  

## then create the histograms 
hist(trips$TRVLCMIN[trips$TDWKND == 2], 
     breaks = 250,  ## this parameter sets the bin sizes. More breaks = smaller bins. 
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
mean(trips$TRVLCMIN[trips$TDWKND == 1])
median(trips$TRVLCMIN[trips$TDWKND == 1])
sd(trips$TRVLCMIN[trips$TDWKND == 1])
var(trips$TRVLCMIN[trips$TDWKND == 1])  
mean(trips$TRVLCMIN[trips$TDWKND == 2])
median(trips$TRVLCMIN[trips$TDWKND == 2])
sd(trips$TRVLCMIN[trips$TDWKND == 2])
var(trips$TRVLCMIN[trips$TDWKND == 2])

# or use dplyr to simplify this
summary <- trips %>%
  group_by(TDWKND) %>%
  summarize(mean = mean(TRVLCMIN),
            median = median(TRVLCMIN),
            sd = sd(TRVLCMIN),
            var = var(TRVLCMIN)) %>%
  mutate(
    TDWKND = case_when(
      TDWKND == 1 ~ "Weekend",
      TDWKND == 2 ~ "Work Week"))
print(summary)

## add these lines to the histograms
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
skewness(trips$TRVLCMIN)
skewness(trips$TRVLCMIN[trips$TDWKND == 1])
skewness(trips$TRVLCMIN[trips$TDWKND == 2])
kurtosis(trips$TRVLCMIN)
kurtosis(trips$TRVLCMIN[trips$TDWKND == 1])
kurtosis(trips$TRVLCMIN[trips$TDWKND == 2])
