#This tutorial accompanies Chang (2025) R Graphics Cookbook https://r-graphics.org/

#Chapter 4: Line charts
## load packages
library(ggplot2)
library(gcookbook)  #this includes sample datasets used below


#4.1) Basic line graph
## view the data, from gcookbook
BOD

##basic chart
ggplot(  ##call the plot space
  data = BOD,  ##define the data
  aes(x = Time, y = demand)  ##map data elements to aesthetic elements
) + 
  geom_line()  ##specify a line chart

##create a new data frame
new_data <- BOD

##treat time as a category, not a generic number
new_data$Time <- factor(new_data$Time)

ggplot(new_data, aes(x = Time, y = demand, group = 1)) +
  geom_line()



#4.2) Add points to a line graph
worldpop  ##this data is from the gcookbook library

ggplot(worldpop,   
       aes(x = Year, y = Population)) +
  geom_line() +  ##the first geometric layer
  geom_point()   ##another geometric layer


##convert the scale to logarithmic
ggplot(worldpop, aes(x = Year, y = Population)) +
  geom_line() +
  geom_point() +
  scale_y_log10() 


#4.3) Multiple lines on the graph
tg  ##again, data from gcookbook

## Two lines, split by the category in variable supp
ggplot(tg, aes(x = dose, y = length, colour = supp)) +
  geom_line()

## This will use line styles instead of colors
ggplot(tg, aes(x = dose, y = length, linetype = supp)) +
  geom_line()

##now treat the dose variable as a category, using the factor() function
ggplot(tg, aes(x = factor(dose), y = length, colour = supp, group = supp)) +
  geom_line()

##or try adding points, mapped to categories
ggplot(tg, aes(x = dose, y = length, shape = supp)) +
  geom_line() +
  geom_point(size = 4) 

ggplot(tg, aes(x = dose, y = length, fill = supp)) +
  geom_line() +
  geom_point(size = 4, shape = 23)  

##adding a position adjustment will ensure no overlaps
ggplot(tg, aes(x = dose, y = length, shape = supp)) +
  geom_line(position = position_dodge(0.2)) +   ##dodges the lines         
  geom_point(position = position_dodge(0.2), size = 4)  ##dodges the points


#4.4) Changing appearance of lines
BOD  ## view the data
tg

ggplot(BOD, aes(x = Time, y = demand)) +
  geom_line(
    linetype = "dashed",  ## other options include blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
    size = 1,  ##measured in millimeters
    colour = "blue"  ##can use default options, or add your own hex code
    )

##it's common to use pre-defined palettes. See this list for details: https://github.com/EmilHvitfeldt/r-color-palettes
ggplot(tg, aes(x = dose, y = length, color = supp)) +
  geom_line() + 
  scale_color_brewer(palette = "Set2")  ##this palette collection is from ColorBrewer (colorbrewer2.org)

##since line color is visualizing our primary categories, define it in aes
ggplot(tg, aes(x = dose, y = length, color = supp)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 25, size = 2.5, fill = "white")


#4.6) Shaded area charts
#view data from gcookbook, then load as a data frame
sunspot.year
sunspot_data <- data.frame(  ##data.frame() creates a new, empty frame
  Year = as.numeric(time(sunspot.year)),  ##create a variable called year, based on the time
  Sunspots = as.numeric(sunspot.year)  ##create a variable called sunspots, based on the counts per year
)

##geom_area() will create the area chart
ggplot(sunspot_data, aes(x = Year, y = Sunspots)) +
  geom_area()

##playing around with style of areas
ggplot(sunspot_data, aes(x = Year, y = Sunspots)) +
  geom_area(colour = "darkgray", fill = "purple", alpha = .25)

ggplot(sunspot_data, aes(x = Year, y = Sunspots)) +
  geom_area(colour = "darkgray", fill = "purple", alpha = .25) + 
  geom_line(color = "darkblue")  ## adds a line


#4.7 Stacked area charts
#view the data, load as a data frame
uspopage
data <- uspopage

##stack areas representing age groups
ggplot(data, aes(x = Year, y = Thousands, fill = AgeGroup)) +
  geom_area()

##playing around with style
ggplot(data, aes(x = Year, y = Thousands, fill = AgeGroup)) +
  geom_area(color = "darkred", size = .15, alpha = .5) +
  scale_fill_brewer(palette = "Reds")
