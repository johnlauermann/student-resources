#This script accompanies Wickham et al's R for Data Science, Chapter 1 https://r4ds.hadley.nz/data-visualize.html
#This is an introduction to data visualization in R. It covers some basics of R's "grammar of graphics" and static data viz in ggplot2.

#set up packages 
##Note: if you don't have a library installed, you can do so in the Packages tab on RStudio or with the function install.packages("package name here")
library(tidyverse)
library(patchwork)

#set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

#create and view a data frame (in this case, from the tidyverse library)
data <- mpg
glimpse(data)
head(data, n=7)
view(data)


#define the aesthetics of plot spaces
##using colors and symbols to communicate data values
left <- ggplot(data, aes(x = displ, y = hwy, color = class)) +
  geom_point() 
right <- ggplot(data, aes(x = displ, y = hwy, shape = class)) +
  geom_point()
left + right  #use patchwork library to create a side-by-side view

##using size and alpha (transparency) to communicate data values
left <- ggplot(data, aes(x = displ, y = hwy, size = class)) +
  geom_point()
right <- ggplot(data, aes(x = displ, y = hwy, alpha = class)) +
  geom_point()
left + right


#geometric objects
##comparing two types of geom
left <- ggplot(data, aes(x = displ, y = hwy)) + 
  geom_point()
right <- ggplot(data, aes(x = displ, y = hwy)) + 
  geom_smooth()
left + right

##you can define parameters globally in the ggplot() layer and/or locally in the geom layer
ggplot(data, aes(x = displ, y = hwy, color = drv)) + 
  geom_point() +
  geom_smooth()
ggplot(data, aes(x = displ, y = hwy, color = drv)) + 
  geom_point() +
  geom_smooth(aes(linetype = drv))

##here's another global vs. local example
ggplot(data, aes(x = displ, y = hwy, color = class)) + 
  geom_point() + 
  geom_smooth()
ggplot(data, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth()

##by the same logic, you can use local arguments to override global settings for specific layers 
ggplot(data, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_point(
    data = mpg |> filter(class == "2seater"), 
    color = "red",
    shape = "circle",
    size = 3
  ) 



#Facets
##facet_wrap splits the plot into subplots based on a categorical variable
ggplot(data, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~cyl)

##if you want the grid to compare across two variables, use facet_grid()
ggplot(data, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl)  ##by default, all graphics will use the same axes

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl, scales = "free") ## you could modify this to use different axes


#Statistical transformations
##pull a new dataset
data <- diamonds

##bar/column charts
ggplot(data, aes(x = cut)) + 
  geom_bar() ##default uses the count from the dataframe

ggplot(data, aes(x = cut, y = after_stat(prop), group = 1)) + 
  geom_bar() ## this will use the proportion value from the Y variable instead

##customizing the outlines or fill values
left <- ggplot(mpg, aes(x = drv, color = drv)) + 
  geom_bar()
right <- ggplot(mpg, aes(x = drv, fill = drv)) + 
  geom_bar()
left + right

##customizing the position adjustments
left <- ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(alpha = 1/5, position = "identity")  ## identity places each object exactly where it falls in a chart
middle <- ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "fill") ## fill stacks bar segments, along a proportional scale
right < - ggplot(mpg, aes(x = drv, fill = class)) + 
  geom_bar(position = "dodge")   ## dodge splits bars into a side-by-side arrangement
left + middle + right

