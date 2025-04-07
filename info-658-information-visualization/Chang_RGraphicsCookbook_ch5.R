#This tutorial accompanies Chang (2025) R Graphics Cookbook https://r-graphics.org/

#Chapter 5: Scatter plots
## load packages
library(ggplot2)
library(gcookbook)  ##this includes sample datasets used below
library(dplyr)  ##used for managing/reshaping data
library(MASS)  ##more sample data
library(ggrepel)  ##for offsetting items in ggplots


#5.1) Basic scatter plot
##view the data from gcookbook
heightweight
ls(heightweight)  ##lists the variables in a data frame

##then query relevant variables
data <- heightweight %>%  ##pipe operator means add more commands...
  select(ageYear, heightIn)  ##select only these variables

##now graph it
ggplot(data, aes(x = ageYear, y = heightIn)) + 
  geom_point()

##the point layer can be customized with parameters
ggplot(data, aes(x = ageYear, y = heightIn)) + 
  geom_point(shape = 21, size = 1.5)


#5.2) Grouping points by shape or color
data <- heightweight ##now using all variables

##adding shape and color to aes
ggplot(data, aes(x = ageYear, 
                 y = heightIn,
                 shape = sex, 
                 color = sex)) + 
  geom_point()

##adding a color palette
ggplot(data, aes(x = ageYear, y = heightIn, shape = sex, color = sex)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set3")


#5.4) Using continuous variables
##load data from gcookbook
data <- heightweight %>%
  select(sex, ageYear, heightIn, weightLb)
head(data)  ##show the first few rows
ls(data)  ## lists the columns

##add continuous variable to color
ggplot(data, aes(x = ageYear, y = heightIn, color = weightLb)) +
  geom_point()

##add continuous variable to size
ggplot(data, aes(x = ageYear, y = heightIn, size = weightLb)) +
  geom_point()


#5.6) Adding trend lines based on simple regression
##create a plot object for future use
plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn))

##adding a trend line
plot +
  geom_point() + 
  stat_smooth(method = lm) ##adds a simple linear model as a trend line

##or you could right it the same as other charts
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + 
  geom_point() + 
  stat_smooth(method = lm)

##now add confidence bands
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + 
  geom_point() + 
  stat_smooth(method = lm, 
              level = .95   ##this is the 'alpha' value. In stats terms: we are 95% confident the trend is in this range. 
              )

##customizing the line and confidence bands
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + 
  geom_point(color = "darkblue") + 
  stat_smooth(method = lm, level = .95, color = "red")

##try out other trend models
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + 
  geom_point(color = "darkblue") + 
  stat_smooth(method = loess,   ##options include lm, glm, loess
              level = .95, color = "red")

##or add two trends
ggplot(heightweight, aes(x = ageYear, y = heightIn, color = sex)) +
  geom_point() +
  scale_colour_brewer(palette = "Set1") + 
  geom_smooth(method = lm, fullrange = TRUE)  ##with a linear model

ggplot(heightweight, aes(x = ageYear, y = heightIn, color = sex)) +
  geom_point() +
  scale_colour_brewer(palette = "Set1") +   ## with LOESS
  geom_smooth(method = loess, fullrange = TRUE)


##for more complex trends, you can add your own formulas
biopsy
data <- biopsy %>%
  mutate(classn = recode(class, benign = 0, malignant = 1)) ##renames variables while loading

##work on a plot when you have binomial data
ggplot(data, aes(x = V1, y = classn)) +
  geom_point(
    position = position_jitter(width = 0.3, height = 0.06),
    alpha = 0.4,
    shape = 21,
    size = 1.5
  ) +
  stat_smooth(method = glm, method.args = list(family = binomial))


#5.11) Adding labels
countries
data <- countries %>% 
  filter(Year == 2009 & healthexp > 2000) ##filter() pulls the rows that meet the query

##basic plot, defined as an object then displayed
plot <- ggplot(data, aes(x = healthexp, y = infmortality)) +
  geom_point()
plot

##adding annotations with positions on the x,y axes
plot +
  annotate("text", x = 4350, y = 5.4, label = "Canada") +
  annotate("text", x = 7400, y = 6.8, label = "USA")

##adding all labels based on a variable
plot +
  geom_text(aes(label = Name), size = 3.5, color = "blue")

##and use ggrepel to place conflicting labels
plot +
  geom_text_repel(aes(label = Name), size = 3.5, color = "blue")

