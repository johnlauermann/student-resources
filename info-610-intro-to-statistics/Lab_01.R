# Lab 1: Data exploration
## In this lab, we'll explore NYC Open Data to learn the basics of data management in R. 


# Set your working directory
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac


# Pull data from the NYC Street Tree Census (https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh/about_data)
## Download as a CSV, then:
data <- read.csv("alltrees.csv")


#Part 1: Explore dimensions of the data
## see the first rows
head(data)
head(data, 10) # this specifies how many first rows

## count dimensions of the data
nrow(data)
ncol(data)

## list the variables
ls(data)

## identify the data types
class(data$bbl) ## for a single variable
sapply(data, class) ## for all variables, using sapply() to iterate through each column

## find null values using is.na()
sum(is.na(data$bbl))  ## for a single variable
colSums(is.na(data))  ## for the entire data frame


# Part 2: Summarize data for a single species
### for this, we'll import the dplyr library, which has more specialized data management tools
install.packages("dplyr") ## if you need to install it...
library(dplyr)

## query the data from the web, download as a csv, then:
linden <- data %<% 
  filter(spc_common = "Linden")  ## I'll use American Linden trees as an example

## create frequency and proportional tables
frequency <- table(data$borough)
print(frequency)

proportion <- prop.table(frequency)
print(proportion)

## create a pivot table
pivot <- linden %>%  ## The %>% is the 'pipe' operator. It means do this, then keep going.
  group_by(nta_name) %>%  ## group_by() to group by neighborhood
  summarize(count = n())  ## summarize() can be any descriptive statistic on the group. I want the total count. 
print(pivot)

## basic data visualizations
### histograms, which show distribution of a single variable
hist(linden$tree_dbh)

### bar charts, which summarize counts by attribute
health_summary <- linden %>%
  group_by(health) %>%
  summarize(mean_dbh = mean(tree_dbh))
barplot(health_summary$mean_dbh, names.arg = health_summary$health)


# Part 3: Summarize your chosen borough
## You're on your own for this, but here's one cool trick you could include: making a simple map. 

## load necessary libraries (install them first if needed)
library(sf)
library(ggplot2)

## create a spatial data frame
spatial_data <- st_as_sf(x = linden, ## your data frame
                         coords = c("longitude", "latitude"), ## the variables with location information
                         crs = 4326  ## a map projection to use...will vary for each data source, but this one is generic enough for most
                         ) 

## define a simple map
ggplot(data = spatial_data) +  ## in ggplot, the library uses + similar to how dplyr uses %>%
   geom_sf() +
  theme_light()

ggplot(data = spatial_data, 
       aes(size = tree_dbh, color = health)) +  ## the aes parameter connects aesthetic elements to columns of data
  geom_sf() +
  theme_light()
