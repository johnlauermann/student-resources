# Lab 7: Chi-squared tests

## Explore the Stanford Open Policing Project database of police traffic stops (https://openpolicing.stanford.edu/data/). Then:
## Pick a location and download the relevant dataset. 
## Import to R, so we can run categorical tests.

library(here)

# set environment & load data
here::i_am("Lab_07.r")

## here's a method to download data files directly in the script
url <- "https://stacks.stanford.edu/file/druid:yg821jf8611/yg821jf8611_ri_statewide_2020_04_01.rds"
download.file(url = url, destfile = "RI_statewide_2020.rds")

## load data
data <- readRDS("RI_statewide_2020.rds")

## verify shape of data
dim(data)
ls(data)
str(data)


# Q2: chi-squared tests
## a basic chi-squared test will assess whether there are differences across groups
## start by exploring the data
table(data$frisk_performed, data$subject_race)

## then the test
friskbyrace <- chisq.test(data$frisk_performed, data$subject_race, correct = FALSE)
print(friskbyrace)
summary(friskbyrace)


# Q3: contingency tables
friskbyrace$observed #the actual observed counts by category pairs
friskbyrace$expected  #the expected counts by category pairs, based on probabilities
friskbyrace$residuals   #positive suggests that observed is greater than expected, large indicate larger deviations from expected
friskbyrace$stdres



# Q4: odds ratios and Fisher test

data$subject_iswhite <- ifelse(data$subject_race == "white", "yes", "no") #calculate a simpler race variable, since my machine was crashing 

fisher.test(data$frisk_performed, data$subject_iswhite) # fisher test will calculate the odds ratio


data$subject_ishispanic <- ifelse(data$subject_race == "hispanic", "yes", "no")

friskbyhispanic <- chisq.test(data$frisk_performed, data$subject_iswhite)

