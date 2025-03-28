#Explore the Stanford Open Policing Project database of police traffic stops (https://openpolicing.stanford.edu/data/). Then:
#Pick a location and download the relevant dataset. 
#Import to R, so we can run categorical tests.
# see also this tutorial for details: https://whitlockschluter3e.zoology.ubc.ca/RLabs/R_tutorial_Contingency_analysis.html#learning_outcomes

#load data
#set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac
CAdata <- read.csv("ca_los_angeles_2020_04_01.csv")
NYdata <- readRDS("yg821jf8611_ny_statewide_2020_04_01.rds")
RIdata<- readRDS("yg821jf8611_ri_statewide_2020_04_01.rds")

#verify shape of data
dim(RIdata)
ls(RIdata)
str(RIdata)


#Question 1) chi square tests
searchbyarrest <- chisq.test(RIdata$search_conducted, RIdata$arrest_made)
print(searchbyarrest)
searchbyarrest$observed  #the actual observed counts by category pairs
searchbyarrest$expected  #the expected counts by category pairs, assuming the categories are independent
searchbyarrest$residuals  #positive suggests that observed is greater than expected, large indicate larger deviations from expected
searchbyarrest$stdres 

#another example
table(RIdata$frisk_performed)
table(RIdata$subject_race)
table(RIdata$frisk_performed, RIdata$subject_race)
friskbyrace <- chisq.test(RIdata$frisk_performed, RIdata$subject_race, correct = FALSE)
print(friskbyrace)
summary(friskbyrace)
friskbyrace$observed
friskbyrace$expected
friskbyrace$residuals
friskbyrace$stdres

#other explorations:
RIdata$subject_iswhite <- ifelse(RIdata$subject_race == "white", "yes", "no") #calculate a simpler race variable, since my machine was crashing 
fisher.test(RIdata$frisk_performed, RIdata$subject_iswhite) # fisher test will calculate the odds ratio

RIdata$subject_ishispanic <- ifelse(RIdata$subject_race == "hispanic", "yes", "no")
friskbyhispanic <- chisq.test(RIdata$frisk_performed, RIdata$subject_iswhite)

