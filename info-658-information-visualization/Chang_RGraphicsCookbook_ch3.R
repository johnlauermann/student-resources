#This tutorial accompanies Chang (2025) R Graphics Cookbook https://r-graphics.org/

#Chapter 2: Data viz using base R functions
##to install a package
install.packages("ggplot2", "gcookbook")

##to load a package
library(ggplot2)
library(gcookbook)  #this includes sample datasets used below

##set up your working directory 
setwd("C:/Users/YourName/Documents/ClassData")  #example for a PC
setwd("~/Documents/ClassData")  #example for a Mac


#3.1 Basic bar graph
ggplot(  ##defines the plot
  data = pg_mean,   ##calls the data
  aes(x = group, y = weight)  ##maps data to aesthetic elements
  ) +
  geom_col()  #defines what kind of geom layer we want

ggplot(BOD, aes(x = Time, y = demand)) + ##different data, using a time variable
  geom_col()

ggplot(BOD, aes(x = factor(Time), y = demand)) +  ##factor() treats Time as a category
  geom_col()


#3.2 Grouped bar charts
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_col(position = "dodge")  ##position adjustment places bars along the dodge variable categories

ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(position = "dodge", colour = "black") +  ##now we've added a color to the bar boundaries
  scale_fill_brewer(palette = "Pastel1")  ##and added a color palette to the entire graphic


#3.3 Bar graphs by count
ggplot(diamonds, aes(x = cut)) + ##by removing the Y variable, we force a count
  geom_bar()

ggplot(diamonds, aes(x = carat)) + ##if you use a continuous variable for X
  geom_bar()

ggplot(diamonds, aes(x = carat)) + 
  geom_histogram()  ##or if you want a histogram


#3.4 Using color
##use dpylr and the pipe operator to query top ten states by population
pop <- uspopchange %>%
  arrange(desc(Change)) %>%
  slice(1:10)  

##now chart, with Region defining colors
ggplot(pop, aes(x = Abb, y = Change, fill = Region)) +
  geom_col()

ggplot(upc, aes(x = reorder(Abb, Change), y = Change, fill = Region)) +
  geom_col(colour = "darkgray") +  ##adding boundaries to bars
  scale_fill_brewer(palette = "Pastel2") +  ##using a palette
  xlab("State")  ##adding a label to the x axis


##3.6 Adjusting bar width & spacing
##to adjust the bars, use width parameter in geom_col()
ggplot(pg_mean, aes(x = group, y = weight)) +
  geom_col()  ##default
ggplot(pg_mean, aes(x = group, y = weight)) +
  geom_col(width = 0.5)  ##narrower
ggplot(pg_mean, aes(x = group, y = weight)) +
  geom_col(width = 1)  ##wider

##if you have grouped bars and want to space them
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(width = 0.5, position = "dodge")  ##default
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(width = 0.5, position = position_dodge(0.7))  ##adjust the gap


#3.7 Stacked bar charts
##adding a fill parameter will stack subcategories
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col()

ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(position = position_stack(reverse = TRUE)) +  ##adjusting the order of the stake
  guides(fill = guide_legend(reverse = TRUE))  ##and the legend too

#3.8 Proportional stacked bars
##position = "fill" will create a proportional stack
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(position = "fill")

ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) +
  geom_col(colour = "darkgray", position = "fill") +  ##adds boundaries
  scale_y_continuous(labels = scales::percent) +  ##change the label on the axis
  scale_fill_brewer(palette = "Pastel2")  ##applies a palette


##3.9 Adding labels to chart
##labeling the bars with data they represent
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) +
  geom_col() +
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white")  ##adds a text layer, use vjust to choose placement

##labeing the bars with counts
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")  ##define the labe as a stratstic

