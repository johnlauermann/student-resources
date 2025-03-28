library(moments)

#set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

#add your data
trips <- read.csv("tripv2pub.csv")


#Present tables that summarize trip time or distance
trips$MinuteCategory <- ifelse(trips$TRVLCMIN >= 15, 1, 0) #I created a new variable that classifies trips as more or less than 15 minutes
table(trips$TDWKND, trips$MinuteCategory) #crosstabulated frequency table
prop.table(table(trips$TDWKND, trips$TRVLCMIN)) #crosstabulated density table


# Design histograms that descriptive trip time or distance by a category (I used the weekend/workweek variable)
par(mfrow = c(1,2))  #partition the plot space into one row and two columns
hist(trips$TRVLCMIN[trips$TDWKND == 2], 
     breaks = 250,
     xlim = range(0,120),
     main = "Work Week", xlab = "Minutes", ylab = "Trips",
     col = "yellow")
hist(trips$TRVLCMIN[trips$TDWKND == 1], 
     breaks = 250,
     xlim = range(0,120),
     main = "Weekend", xlab = "Minutes", ylab = "Trips", 
     col = "purple")


# Interpret the distributions using mean, median, standard deviation, and variance
mean(trips$TRVLCMIN[trips$TDWKND == 1])
median(trips$TRVLCMIN[trips$TDWKND == 1])
sd(trips$TRVLCMIN[trips$TDWKND == 1])
var(trips$TRVLCMIN[trips$TDWKND == 1])  
mean(trips$TRVLCMIN[trips$TDWKND == 2])
median(trips$TRVLCMIN[trips$TDWKND == 2])
sd(trips$TRVLCMIN[trips$TDWKND == 2])
var(trips$TRVLCMIN[trips$TDWKND == 2])     


#Calculate the skewness and kurtosis of the distributions
skewness(trips$TRVLCMIN)
skewness(trips$TRVLCMIN[trips$TDWKND == 1])
skewness(trips$TRVLCMIN[trips$TDWKND == 2])
kurtosis(trips$TRVLCMIN)
kurtosis(trips$TRVLCMIN[trips$TDWKND == 1])
kurtosis(trips$TRVLCMIN[trips$TDWKND == 2])
