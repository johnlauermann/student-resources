#This tutorial accompanies Chang (2025) R Graphics Cookbook https://r-graphics.org/

#Chapter 13.17-13.20: Making maps 
## load packages
library(ggplot2)
library(gcookbook)  ##this includes sample datasets used below
library(dplyr)  ##used for managing/reshaping data
library(maps)  ##collection of map data
library(mapproj)  ##required for changing projection on maps


#13.17) Create a map
states <- map_data("state") ## pull sample data from maps library

##basic outline of states
ggplot(data = states, 
       aes(x = long, ##for maps, x is always longitude
           y = lat,  ##and y is always latitude
           group = group  ##this draws the state polygons
           )) + 
  geom_polygon(fill = "white", 
               color = "black")

##now add a projection
ggplot(states, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black") + 
  coord_map("mercator")  ##the list of projection options here: https://rdrr.io/cran/mapproj/man/mapproject.html

##outline of world countries
world <- map_data("world")
sort(unique(world$region))

##one way to query a region and map only that place
##define a smaller list of countries
east_asia <- map_data("world", region = c("Japan", "China", "North Korea", "South Korea"))

##map the region
ggplot(east_asia, aes(x = long, y = lat, group = group, fill = region)) +
  geom_polygon(colour = "darkgray") +
  scale_fill_brewer(palette = "Set1") +
  coord_map("mollweide")

##Now try New Zealand, with a spatial query to select data
nz <- map_data("world", region = "New Zealand") %>%
  filter(long > 0 & lat > -48)        ##sets a geographic filter

ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_path()


#13.18 Create a choropleth
##pull and clean data
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimes
states <- map_data("state")
states

##merge data together
crime_map <- merge(states, crimes, by.x = "region", by.y = "state")
crime_map

##create the map
ggplot(crime_map, aes(x = long, y = lat, group = group, fill = Assault)) +
  geom_polygon(color = "darkgray") +
  coord_map("polyconic")

##use gradients to visualize the polygon data
ggplot(crime_map, aes(x = long, y = lat, group = group, fill = Assault)) +
  geom_polygon(color = "darkgray") +
  coord_map("polyconic") +
  scale_fill_gradient2(low = "#559999", mid = "grey90", high = "#BB650B",
                     midpoint = median(crimes$Assault))  ##defining your own gradien

ggplot(crime_map, aes(x = long, y = lat, group = group, fill = Assault)) +
  geom_polygon(color = "darkgray") +
  coord_map("polyconic") +
  scale_fill_viridis_c()  ##or use viridis, which is optimized for visual accesibility


##creating a quantile classification
### Find quantiles
quantiles <- quantile(crimes$Assault, c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
quantiles

###add them to the data frame
crimes$Assault_quantile <- cut(crimes$Assault, quantiles,
                        labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                        include.lowest = TRUE)
crimes

###generate a color palette with 5 values
colors <- colorRampPalette(c("#559999", "grey80", "#BB650B"))(5)
colors

###create the map
ggplot(crimes, aes(map_id = state, fill = Assault_quantile)) +
  geom_map(map = states, colour = "black") +
  scale_fill_manual(values = colors) +
  expand_limits(x = states$long, y = states$lat) +
  coord_map("polyconic") +
  labs(fill = "Assault Rate\n Percentile")  ## the \n will create a new line in legend


#13.19) Using other themes
ggplot(crimes, aes(map_id = state, fill = Assault_quantile)) +
  geom_map(map = states, colour = "black") +
  scale_fill_manual(values = colors) +
  expand_limits(x = states$long, y = states$lat) +
  coord_map("polyconic") +
  labs(fill = "Assault Rate\n Percentile") +
  theme_void()  ##list of theme options: https://ggplot2.tidyverse.org/reference/ggtheme.html

