# Load required libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(leaflet)

# read-in dataset
business_tbl <- readRDS("business_tbl.rds")
day_of_week <- wday(today(), label = TRUE, abbr = FALSE) %>%
  as.character()

# Define the UI
ui <- fluidPage(theme = shinytheme('lumen'), 
    titlePanel("Fancy a coffee now?"),
    sidebarLayout(
    sidebarPanel(
      selectInput("state", "Which state?",
                  choices = unique(business_tbl$state),
                  selected = "CA"),
      br(),
      sliderInput("hour", "Select Hour:", 
                  min = 14,
                  max = 24,
                  step = 1,
                  ticks = FALSE,
                  value = 16),
      br(),
      actionButton("showMap", "Show Map", 
                       class = "btn-success"),
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  getColor <- function(data, hour) {
    data %>%
      mutate(color = ifelse( (!!sym(day_of_week) - hour) <= 2, "orange", 
                             "green")) %>%
      pull(color)
  }
  
  # Function to filter open businesses based on selected hour
  filter_data <- function(data, selected_state, selected_hour) {
    result <- data %>%
      filter(!!sym(day_of_week) > selected_hour,
             state == selected_state)
    return(result)
  }
  
  filtered_tbl <- eventReactive(input$showMap, {
    selected_hour <- input$hour
    filter_data(business_tbl, input$state, selected_hour)
      }, ignoreNULL = FALSE)
  
  filtered_icons <- eventReactive(input$showMap, {
    awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(filtered_tbl(), input$hour)
      )  }, ignoreNULL = FALSE)

  output$map <- renderLeaflet({
    
    leaflet(filtered_tbl()) %>%
      addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  observe({
    
    labs <- lapply(seq(nrow(filtered_tbl())), function(i) {
      paste0( '<strong>', filtered_tbl()[i, "name"], '</strong><br/>',
              filtered_tbl()[i, "city"], ', ',
              filtered_tbl()[i, "state"], '<br>',
              'Stars: <strong>', filtered_tbl()[i, "stars"], '</strong>')
    })
    
    leafletProxy("map", data = filtered_tbl()) %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = filtered_tbl(),
                        icon = filtered_icons(),
                        label = lapply(labs, htmltools::HTML))
  })
  
  
  }

# Run the Shiny app
shinyApp(ui, server)
