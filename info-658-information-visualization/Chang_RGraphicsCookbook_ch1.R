#This tutorial accompanies Chang (2025) R Graphics Cookbook https://r-graphics.org/

#Chapter 1: R Basics
##to install a package
install.packages("ggplot2", "dplyr", "readxl")

##to load a package
library(ggplot2)
library(dplyr)
library(readxl)

##top update packages
update.packages(ask=FALSE)

##set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

##loading data from CSV or excel
 #this will look for a file with that name in your workign directory
data <- read.csv("C:/Documents/ClassData/yourfile/csv") #this will pull a specific file
data <- read_excel("yourfile.xlsx", 1) #reads an excel file, identifies which tab to read

##use the pipe operator to query data when loading
data <- read.csv("yourfile.csv") %>%
  filter(Year >= 2010)

