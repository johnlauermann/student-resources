#This tutorial accompanies Chang (2025) R Graphics Cookbook https://r-graphics.org/

#Chapter 2: Data viz using base R functions
##to install a package
install.packages("ggplot2", "dplyr", "readxl")

##to load a package
library(ggplot2)

##set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

#2.1 Scatter plots 
mtcars  ##this is a sample dataset included with R

#using the base R plot function
plot(  
  mtcars$wt, #defines the x variable, by identifying dataframe and $variable
  mtcars$mpg)  #defines the y variable

#using ggplot
ggplot(   ##this defines the plot space
  data = mtcars, ## this calls the data
  aes(x = wt, y = mpg) ##this maps data to aesthetic properties
  ) + 
  geom_point()  ##this adds the geom_ layer type we want 


#2.2 Line graphs
pressure  ##another sample dataset included with R

#using base R
plot(pressure$temperature, pressure$pressure, type = "l") ##type defines a line chart. to see others, search for plot() in the help window

plot(pressure$temperature, pressure$pressure, type = "l") 
points(pressure$temperature, pressure$pressure) ##calling points immediately after will add them to the plot

lines(pressure$temperature, pressure$pressure/2, col = "red")  ##similarly, this will add lines to the same plot
points(pressure$temperature, pressure$pressure/2, col = "red")


##2.3 Bar graphs
BOD  ##another sample dataset

##using base R
barplot(BOD$demand, names.arg = BOD$Time)  ##first parameter defines data, second defines categories for each bar

##using ggplot
ggplot(BOD, aes(x = Time, y = demand)) +
  geom_col() ##bars are a geom_col type of layer

ggplot(BOD, aes(x = factor(Time), y = demand)) +  ## passing Time through the factor() function will treat it as a category
  geom_col()


#2.4 Histograms
mtcars  ##another sample dataset

##using base R
hist(mtcars$mpg)  ##default
hist(mtcars$mpg, breaks = 20)  ##breaks will change the number of bins in the graph

##using ggplot
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram()

ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = .5)
