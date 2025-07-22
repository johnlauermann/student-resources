# Lab 10: cluster analysis

## see also this tutorial: https://uc-r.github.io/kmeans_clustering

library(tidyverse)
library(tidycensus)
library(dplyr)
library(cluster)
library(factoextra)


# load data
## sign up for a Census API key at https://api.census.gov/data/key_signup.html
census_api_key("fd874e212346a3d3f3e27fce3fb3cc7dfce3c4e1")

## define a vector with variables
variables <- c(
  PovertyRate  = "S1701_C03_046E",
  Bachpct = "S1501_C02_012E",
  Postgradpct = "S1501_C02_013E",
  UnemploymentRate = "S2301_C04_001E",
  MedConRent = "B25058_001E",
  MedHomeValue = "B25077_001E", 
  Renterspct = "DP04_0046PE" 
  )

## pull from API
data <- get_acs(geography = "county",
                variables = variables,
                output = "wide",
                year = 2023)


##  remove unnecessary variables & missing values
data <- data %>% select(c(Bachpct, MedConRent, MedHomeValue, Postgradpct, 
                          PovertyRate, Renterspct, UnemploymentRate))
data <- na.omit(data)


# Question 1: what variables did you choose and why?
## visualizing data
## scatterplot pairs
pairs(data)

# heat maps
distance <- get_dist(na.omit(data))
fviz_dist(distance, gradient = list(low = "blue", mid = "white", high = "red"))



# Question 2: cluster analysis
## cluster analysis, moving through each value of k
k2 <- kmeans(data, centers = 2, nstart = 25)
k3 <- kmeans(data, centers = 3, nstart = 25)
k4 <- kmeans(data, centers = 4, nstart = 25)
k5 <- kmeans(data, centers = 5, nstart = 25)
k6 <- kmeans(data, centers = 6, nstart = 25)
k7 <- kmeans(data, centers = 7, nstart = 25)
k8 <- kmeans(data, centers = 8, nstart = 25)
k9 <- kmeans(data, centers = 9, nstart = 25)
k10 <- kmeans(data, centers = 10, nstart = 25)

## print the results
k2
k3
k4
k5
k6
k7
k8
k9
k10

## or use a for loop to automate the process
kvalues <- 2:10
for(k in kvalues){
  kmodel <- paste0("k", k)
  kmodel <- assign(kmodel, kmeans(data, centers = k, nstart = 25))
  print(kmodel)
}


# Question 3: elbow plots to assess optimal value of k
fviz_nbclust(data, kmeans, method = "wss")
fviz_nbclust(data, kmeans, method = "silhouette")
fviz_nbclust(data, kmeans, method = "gap_stat")



# Question 4: visualize the clusters
fviz_cluster(k2, data = data, geom = "point", main = "k = 2", ggtheme = theme_minimal())
fviz_cluster(k3, data = data, geom = "point", main = "k = 3", ggtheme = theme_minimal())
fviz_cluster(k4, data = data, geom = "point", main = "k = 4", ggtheme = theme_minimal())
fviz_cluster(k5, data = data, geom = "point", main = "k = 5", ggtheme = theme_minimal())
fviz_cluster(k6, data = data, geom = "point", main = "k = 6", ggtheme = theme_minimal())
fviz_cluster(k7, data = data, geom = "point", main = "k = 7", ggtheme = theme_minimal())
fviz_cluster(k8, data = data, geom = "point", main = "k = 8", ggtheme = theme_minimal())
fviz_cluster(k9, data = data, geom = "point", main = "k = 9", ggtheme = theme_minimal())
fviz_cluster(k9, data = data, geom = "point", main = "k = 9", ggtheme = theme_minimal())
fviz_cluster(k10, data = data, geom = "point", main = "k = 10", ggtheme = theme_minimal())
