#This script accompanies Hadely et al's R for Data Science, Chapter 1 https://r4ds.hadley.nz/data-visualize.html
#This is an introduction to data visualization in R. It covers some basics of R's "grammar of graphics" and static data viz in ggplot2.

#set up packages 
##Note: if you don't have a library installed, you can do so in the Packages tab on RStudio or with the function install.packages("package name here")
library(tidyverse)
library(ggthemes)
library(palmerpenguins)

#set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac

#create and view a dataframe (in this case, from the palemerpenguins library)
data <- penguins
glimpse(data)
view(data)
head(data, n=7)

#define a plot object, which we can then build visualization on
ggplot(data = data)

#define the aesthetic of the plot space
ggplot(
  data = data, 
  mapping = aes(x=flipper_length_mm, y=body_mass_g)
)

#now let's add a layer of geometric objects to the plot (in this case, a point cloud)
ggplot(
  data = data, 
  mapping = aes(x=flipper_length_mm, y=body_mass_g)
) + 
  geom_point()

#modify the aesthetic of the scatterplot
ggplot(
  data = data, 
  mapping = aes(x=flipper_length_mm, y=body_mass_g, color = species)
) + 
  geom_point()

#add a trendlines layer. More broadly, this is how you would add any additional visual layers.
ggplot(
  data = data,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")

#other customizations
ggplot(
  data = data,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body Mass versus Flipper Length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper Length (mm)", y = "Body Mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

#use facet_wrap() to subdivide the plot space
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

#options for simplifying the script
##you don't always need to spell out the entire parameter statement
ggplot(data, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()
##you can use the pipe symbol
data |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()


#Visualizing distributions
##bar charts
ggplot(data, aes(x = species)) +
  geom_bar()
ggplot(data, aes(x = fct_infreq(species))) +  ##organizes bars by frequency
  geom_bar()
ggplot(data, aes(x = island, fill = species)) + ##stacked bars
  geom_bar()
ggplot(data, aes(x = island, fill = species)) + ##stacked to 100 percent
  geom_bar(position = "fill")

#saving your output
##you can save from the Plots pane using the Export button, or,
##calling the ggsave() function. It will save to whatever working directory you defined earlier.
ggsave("filename.png") 

##histograms
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
ggplot(penguins, aes(x = body_mass_g)) +  ##experiment with the bin width (measured in units of x variable)
  geom_histogram(binwidth = 20)

##density histograms
ggplot(data, aes(x = body_mass_g)) +
  geom_density()
ggplot(data, aes(x = body_mass_g, color = species)) +  ## distributions by species
  geom_density(linewidth = 0.75)
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +  ##filled histograms
  geom_density(alpha = 0.5)

#Visualizing relationships
##box plots
ggplot(data, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
