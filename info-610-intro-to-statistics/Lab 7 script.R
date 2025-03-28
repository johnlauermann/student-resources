#set up, with census API and relevant libraries
library(tidyverse)
library(tidycensus)
library(dplyr)
library(stringr)
library(lsr)

#sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("your key here")

#define a vector with variables
variables <- c(
  Population = "B01003_001E",
  PovertyRate  = "S1701_C03_046E",
  Bachpct = "S1501_C02_012E",
  UnemploymentRate = "S2301_C04_001E")

NYcounties <- c("Kings", "Queens", "New York", "Bronx", 
                "Richmond", "Westchester", 
              "Suffolk", "Nassau", "Rockland", "Putnam")
NJcounties <- c("Bergen", "Hudson", 
              "Passaic", "Middlesex", "Monmouth", "Ocean", 
              "Somerset", "Essex", 
              "Union", "Morris", "Sussex", "Hunterdon")
  
#pulling my data tables by state, then merging
NYdata <- get_acs(geography = "tract",
                state = "New York",
                county = NYcounties,
                variables = variables,
                output = "wide",
                year = 2022)
NJdata <- get_acs(geography = "tract",
                  state = "New Jersey",
                  county = NJcounties,
                  variables = variables,
                  output = "wide",
                  year = 2022)
data <- rbind(NYdata, NJdata)  #append on table to the other

#data cleaning to remove unnecessary variables and omit null values
ls(data)
str(data)
data <- data %>% select(-c(B01003_001M, S1501_C02_012M, 
                           S1701_C03_046M, S2301_C04_001M))  #remove unnecessary columns
mean(data$PovertyRate) #this won't work because of the NA value in the first tract of Bronx County (aka Rikers Island)
data <- na.omit(data) #remove NAs
mean(data$PovertyRate)  #now it works!



#Question 1) run t-tests to compare two groups
# first off, you need two groups. For example, you could use the NY and NJ data frames as separate groups. 
#I'm going to create a new grouping variable based on whether a tract is suburban or inner city. 

#create a county column using the NAME string
data$County <- str_match(data$NAME, ";\\s*(.*?)\\s*;")[,2]

#create a suburb/central city column
data$PlaceType <- "Suburb"
data$PlaceType <- ifelse(grepl("Bronx", data$NAME), "City", data$PlaceType)
data$PlaceType <- ifelse(grepl("Kings", data$NAME), "City", data$PlaceType)
data$PlaceType <- ifelse(grepl("New York County", data$NAME), "City", data$PlaceType)
data$PlaceType <- ifelse(grepl("Queens", data$NAME), "City", data$PlaceType)
data$PlaceType <- ifelse(grepl("Richmond", data$NAME), "City", data$PlaceType)

city <- data[data$PlaceType == "City", ]
suburb <- data[data$PlaceType == "Suburb", ]

#check whether the two groups have equal variance
var(city$PovertyRate)
var(suburb$PovertyRate)
var(city$UnemploymentRate)
var(suburb$UnemploymentRate)
var(city$Bachpct)
var(suburb$Bachpct)

#compare the distributions of each group
mean(city$PovertyRate)
sd(city$PovertyRate)
mean(suburb$PovertyRate)
sd(suburb$PovertyRate)
hist(city$PovertyRate, main = "New York Metro Poverty, 2022", 
     ylab = "Census Tracts", xlab = "Poverty Rate")
hist(suburb$PovertyRate, main = "Poverty in the New York Metro, 2022", 
         ylab = "Census Tracts", xlab = "Poverty Rate", add = TRUE, col = "red")

#run the t-tests to compare variable values in each group
t.test(city$PovertyRate, suburb$PovertyRate, 
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = .95)
t.test(city$UnemploymentRate, suburb$UnemploymentRate, 
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = .95)
t.test(city$Bachpct, suburb$Bachpct, 
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = .95)

#or alternatively, you can use the [] query to work directly with the original dataframe
t.test(data$PovertyRate[data$PlaceType == "City"], 
       data$PovertyRate[data$PlaceType == "Suburb"], 
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = .95)

#assess the effect sizes using cohens D (you'll need the lsr library)
#for more on Cohen's D, see https://statisticsbyjim.com/basics/cohens-d/ 
cohensD(city$PovertyRate, suburb$PovertyRate)
cohensD(city$UnemploymentRate, suburb$UnemploymentRate)
cohensD(city$Bachpct, suburb$Bachpct)



#Question 2) Do ANOVA to compare across three or more groups
#again, you'll need a grouping variable. 
#In my case, I'm going to subset the five boroughs of NYC, and use each borough as a group. 

city <- subset(data, PlaceType == "City") #just another way to subset out the relevant tracts

#configure the ANOVA, with formula main variable ~ group variable
one.way.poverty <- aov(PovertyRate ~ County, data = city)
summary(one.way.poverty)
one.way.unemployement <- aov(UnemploymentRate ~ County, data = city)
summary(one.way.unemployement)
one.way.bach <- aov(Bachpct ~ County, data = city)
summary(one.way.bach)


#the ANOVA will tell you whether there is a difference across all of the groups,
#without explaing where the differences occur. 
#for that, use a post-hoc test like Tukey's HSD
#see more at https://rpubs.com/aaronsc32/post-hoc-analysis-tukey 

#post-hoc tests to assess difference between group pairs
posthoctest.poverty <- TukeyHSD(one.way.poverty)
posthoctest.poverty
posthoctest.unemployment <- TukeyHSD(one.way.unemployement)
posthoctest.unemployment
posthoctest.bach <- TukeyHSD(one.way.bach)
posthoctest.bach

