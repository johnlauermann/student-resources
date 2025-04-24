# This tutorial creates a simple shiny dashboard using random data
# It's based on "Shiny R Dashboards" (0204), https://www.geeksforgeeks.org/shiny-r-dashboard/ 
# For additional discussion, check out: https://tilburgsciencehub.com/topics/visualization/data-visualization/dashboarding/shinydashboard/

# Load the necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(plotly)

# Create sample data using random seed
set.seed(123)
sample_data <- data.frame(
  category = sample(letters[1:5], 100, replace = TRUE),
  x = rnorm(100),
  y = rnorm(100),
  lat = runif(100, 35, 45),
  lng = runif(100, -100, -90)
)

# Configure the user interface
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Dashboard"),  ## This creates a header. See here for parameters: https://rstudio.github.io/shinydashboard/structure.html#header 
  dashboardSidebar(  ## You can customize the sidebar menu too. Parameters here: https://rstudio.github.io/shinydashboard/structure.html#sidebar 
    sidebarMenu(   ## See menu options here: https://rstudio.github.io/shinydashboard/structure.html#sidebar 
      menuItem("Controls", tabName = "controls", icon = icon("sliders")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-line"))
    )
  ),
  dashboardBody(  ## Details on how to customize boxes and/or tabs: https://rstudio.github.io/shinydashboard/structure.html#body 
    tabItems(
      tabItem(tabName = "controls",
              fluidRow(
                box(
                  title = "Inputs", width = 4,
                  sliderInput("slider", "Number of Points:", min = 10, max = 100, 
                              value = 50),
                  checkboxInput("show_plotly", "Show Plotly Plot", value = TRUE),
                  checkboxInput("show_leaflet", "Show Leaflet Map", value = TRUE),
                  selectInput("category", "Select Category:",
                              choices = unique(sample_data$category)),
                  actionButton("update", "Update")
                )
              )),
      tabItem(tabName = "plots",
              fluidRow(
                box(title = "ggplot2 Scatter Plot", width = 6, plotOutput("ggplot")),
                box(title = "Plotly Interactive Plot", width = 6, plotlyOutput("plotly")),
                box(title = "Leaflet Map", width = 12, leafletOutput("leaflet"))
              ))
    )
  )
)

# Configure the server function. For this example, we'll just run everything locally. 
server <- function(input, output, session) {
  
  ## This creates an interaction filter base on our random category
  filtered_data <- reactive({
    sample_data[sample_data$category == input$category, ]
  })
  
  ## With data filtered, this will render a (static) ggplot 
  output$ggplot <- renderPlot({  
    ggplot(filtered_data(), aes(x = x, y = y)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Scatter Plot (ggplot2)", x = "X-Axis", y = "Y-Axis")
  })
  
  ## With data filtered, this will render a plotly object
  output$plotly <- renderPlotly({  ## This will add a plotly plot
    if (input$show_plotly) {
      plot_ly(filtered_data(), x = ~x, y = ~y, type = 'scatter', mode = 'markers') %>%
        layout(title = "Interactive Scatter Plot (Plotly)", 
               xaxis = list(title = "X-Axis"), yaxis = list(title = "Y-Axis"))
    }
  })
  
  ## Ditto here, rendering a map with leaflet, an open source basemap tool
  output$leaflet <- renderLeaflet({
    if (input$show_leaflet) {
      leaflet(filtered_data()) %>%
        addTiles() %>%
        addMarkers(~lng, ~lat, popup = ~paste("X:", x, "<br>Y:", y)) %>%
        setView(lng = mean(filtered_data()$lng), lat = mean(filtered_data()$lat), 
                zoom = 4)
    }
  })
}

# Run the app
shinyApp(ui, server)
