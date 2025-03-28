#in my example, I'm testing whether there's a correlation between poverty and race

#set up and merge. 
#If using Excel for initial data cleaning, see details here: https://support.microsoft.com/en-us/office/merge-queries-and-join-tables-cbd17828-7a50-4dc6-9aac-20af4ef6d8a6
#If using R, you can use the merge() function
library(data.table)

#set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

#load poverty data, and rename columns using colnames()
poverty <- fread("Poverty-ACS20225yr-Data.csv", 
                 select = c('GEO_ID', 'S1701_C03_046E'))
names(poverty)[names(poverty) == 'S1701_C03_046E'] <-'PovertyRate'
colnames(poverty) <- c("GEO_ID", "PovertyRate")

#load race data, rename columns using names(), calculate percentages
race <- fread("Race-ACS20225yr-Data.csv",
              select = c('GEO_ID', 'B02001_001E', 'B02001_002E', 'B02001_003E',
                         'B02001_004E', 'B02001_005E', 'B02001_006E', 'B02001_007E',
                         'B02001_008E'))
names(race)[names(race) == 'B02001_001E'] <- 'Total'
names(race)[names(race) == 'B02001_002E'] <- 'White'
names(race)[names(race) == 'B02001_003E'] <- 'Black'
names(race)[names(race) == 'B02001_004E'] <- 'AIAN'
names(race)[names(race) == 'B02001_005E'] <- 'Asian'
names(race)[names(race) == 'B02001_006E'] <- 'NHPI'
names(race)[names(race) == 'B02001_007E'] <- 'Other'
names(race)[names(race) == 'B02001_008E'] <- 'TwoOrMore'
race$Whitepct <- (race$White / race$Total) * 100
race$Blackpct <- (race$Black / race$Total) * 100
race$AIANpct <- (race$AIAN / race$Total) * 100
race$Asianpct <- (race$Asian / race$Total) * 100
race$NHPIpct <- (race$NHPI / race$Total) * 100
race$Otherpct <- (race$Other / race$Total) * 100
race$TwoOrMorepct <- (race$TwoOrMore / race$Total) * 100


#merge, by default, this will merge based on a shared column name
joined <- merge(poverty, race)


# Q1: Create scatterplots
par(mfrow = c(2, 4))
plot(x=joined$Whitepct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% White", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$Blackpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Black or African American", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$AIANpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% American Indian or Alaska Native", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$Asianpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Asian", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$NHPIpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Native Hawiian or Pacific Islander", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$Otherpct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Other", 
     ylab = "Poverty Rate", 
     col = 'dark red')
plot(x=joined$TwoOrMorepct, 
     y=joined$PovertyRate, 
     main = "Poverty by Race", 
     xlab = "% Two or more races", 
     ylab = "Poverty Rate", 
     col = 'dark red')

#Q2: Calculate correlations and derivative metrics
cor.test(joined$Whitepct, joined$PovertyRate, method = "pearson")
cor.test(joined$Blackpct, joined$PovertyRate, method = "pearson")
cor.test(joined$AIANpct, joined$PovertyRate, method = "pearson")
cor.test(joined$Asianpct, joined$PovertyRate, method = "pearson")
cor.test(joined$NHPIpct, joined$PovertyRate, method = "pearson")
cor.test(joined$Otherpct, joined$PovertyRate, method = "pearson")
cor.test(joined$TwoOrMorepct, joined$PovertyRate, method = "pearson")


#Q3: Correlation matrix
library(dplyr)
library(corrplot)
percentages <- select(joined, PovertyRate, AIANpct, Asianpct, Blackpct,
                      NHPIpct, Otherpct, TwoOrMorepct, Whitepct)
matrix <- cor(percentages)
matrix
corrplot(matrix)
