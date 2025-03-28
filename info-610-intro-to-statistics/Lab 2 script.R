# initial data cleaning included: 
#     1) compiling variable names into one row
#     2) removing US summary and state-level subtotal rows
#     3) removing rows with 0 employees

#set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

#add data
dataraw <- read.csv("ASPEPdata.csv")
data <- na.omit(dataraw)

#step 1: descriptive statistics
mean(data$Ftsalary)
median(data$Ftsalary)
sd(data$Ftsalary)
var(data$Ftsalary)
mean(data$ptsalary)
median(data$ptsalary)
sd(data$ptsalary)
var(data$ptsalary)

#step 2: query subsets
# option 1
mean(data$Ftsalary[data$Government.Function == "Libraries"])
median(data$Ftsalary[data$Government.Function == "Libraries"])
sd(data$Ftsalary[data$Government.Function == "Libraries"])
var(data$Ftsalary[data$Government.Function == "Libraries"])
mean(data$ptsalary[data$Government.Function == "Libraries"])
median(data$ptsalary[data$Government.Function == "Libraries"])
sd(data$ptsalary[data$Government.Function == "Libraries"])
var(data$ptsalary[data$Government.Function == "Libraries"])

#option 2
subset <- subset(data, data$Government.Function == "Libraries")
mean(subset$Ftsalary)
median(subset$Ftsalary)
sd(subset$Ftsalary)
var(subset$Ftsalary)
mean(subset$ptsalary)
median(subset$ptsalary)
sd(subset$ptsalary)
var(subset$ptsalary)

#step 3: using aggregate function
mean_by_state <- aggregate(x = data$Ftsalary,
                           by = list(data$State),
                           FUN = mean)
print(mean_by_state)

median_by_state <- aggregate(x = data$Ftsalary,
                           by = list(data$State),
                           FUN = median)
print(median_by_state)
