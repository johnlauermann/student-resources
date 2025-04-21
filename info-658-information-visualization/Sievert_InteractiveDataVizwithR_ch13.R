# This tutorial accompanies Sievert's Interactive Web-Based Data Viz in R (https://plotly-r.com/)
# It focuses on chapter 13, arranging plotly objects in a dashboard

library(plotly)

# Ch 13.1 Arrange plots on subplots
## Load some default data from the plotly library, then use subplot to chart
plot1 <- plot_ly(  ## this creates a plot space, similar to ggplot()
  data = economics, ## this defines the data
  x = ~date, y = ~unemploy  ## and this maps data to aesthetics. Note you do not need an aes paramter like in ggplot
  ) %>% 
  add_lines(name = "Unemployed")  ##this adds a geometric object, similar to geom paramter in ggplot.

plot2 <- plot_ly(economics, x = ~date, y = ~uempmed) %>% 
  add_lines(name = "Unemployment Rate")

subplot(plot1, plot2)


## Another option is to create a list of plots, to reduce redundancy
variables <- setdiff(names(economics), "date")  ## list includes all variables in the data, as dates

plots <- lapply(variables, function(var) {
  plot_ly(economics, x = ~date, y = as.formula(paste0("~", var))) %>%
    add_lines(name = var)
})

subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)


## subplots can be nested recursively, for example using this custom function
plotList <- function(nplots) {
  lapply(seq_len(nplots), function(x) plot_ly())
}

s1 <- subplot(plotList(6), nrows = 2, shareX = TRUE, shareY = TRUE)
s2 <- subplot(plotList(2), shareY = TRUE)

subplot(
  s1, s2, plot_ly(), nrows = 3, 
  margin = 0.04, heights = c(0.6, 0.3, 0.1)
)


## Here's an example using a map and bar charts
### specify map projection and ptions
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)
### create a map of population density
density <- state.x77[, "Population"] / state.x77[, "Area"]
map <- plot_geo(
  z = ~density, text = state.name, 
  locations = state.abb, locationmode = 'USA-states'
) %>%
  layout(geo = g)

### create a horizontal bar charts along a list of charts
vars <- colnames(state.x77)
barcharts <- lapply(vars, function(var) {
  plot_ly(x = state.x77[, var], y = state.name) %>%
    add_bars(orientation = "h", name = var) %>%
    layout(showlegend = FALSE, hovermode = "y",
           yaxis = list(showticklabels = FALSE))
})

### Plot it all together
subplot(barcharts, margin = 0.01) %>%
  subplot(map, nrows = 2, heights = c(0.3, 0.7), margin = 0.1) %>%
  layout(legend = list(y = 1)) %>%
  colorbar(y = 0.5)


##subplots can be paired using ggpairs
pm <- GGally::ggpairs(iris, aes(color = Species)) ### pulling data from library, with pairs
class(pm)
ggplotly(pm)


## An alternative is to use ggplot2's facet layers to pair, and then subplot to generate the graphic
gg1 <- ggplot(economics_long, aes(date, value)) + geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 1)
gg2 <- ggplot(economics_long, aes(factor(1), value)) +
  geom_violin() +
  facet_wrap(~variable, scales = "free_y", ncol = 1) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank())
subplot(gg1, gg2)


# Ch 13.2 Working with html widgets
## here's an option using shiny
library(shiny)
p <- plot_ly(x = rnorm(100))  ## generate some random data
fluidPage(  ##this will generate HTML
  fluidRow(p),
  fluidRow(
    column(6, p), column(6, p) 
  )
)

## another option is to use htmltools
library(htmltools)
p <- plot_ly(x = rnorm(100))  ## again, just random data
browsable(div(   ##browsable will give more control over objects
  style = "display: flex; flex-wrap: wrap; justify-content: center",
  div(p, style = "width: 40%; border: solid;"),
  div(p, style = "width: 40%; border: solid;"),
  div(p, style = "width: 100%; border: solid;")
))

## and here's an example using trellisscope, with sample data from gapminder
library(trelliscopejs)
library(gapminder)
data(gapminder, package = "gapminder")

qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ country + continent,
                    nrow = 2, ncol = 6, width = 300, 
                    as_plotly = TRUE, 
                    plotly_args = list(dynamicTicks = T),
                    plotly_cfg = list(displayModeBar = F)
  )

