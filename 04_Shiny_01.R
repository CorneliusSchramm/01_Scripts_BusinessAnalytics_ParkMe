# Description ----------------------------------------

# In this script we will:
# - ...

# Setup ----------------------------------------------

# Load required packages
library(shiny)
library(ggplot2)
library(data.table)
library(tidyverse)
library(ggmap)
library(dplyr)
library(gridExtra)

# Clear workspace
rm(list=ls())
graphics.off()

# Register the Google Maps API key
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")


# -------

# Load parking data
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")
# Load function that creates the map
load("../Schramm, Cornelius - 02_Business_Analytics_Data/create_map.RData")


# ------- ALLES BULLSHIT ---------

# Creating dataframe "locations" with locations of all 1477 parking meters
locations = data.frame(parking_orig[!duplicated(parking_orig[,c("SourceElementKey","lon","lat")]),][,c(6,12,13)])
locations[,2] = as.numeric(locations[,2])
locations[,3] = as.numeric(locations[,3])
# hierbei gehen Nachkommastellen verloren ?!

# Get columns freePercent and ParkingSpaceCount for specific date
# For "2019-03-15" "15:00"
date_filtered = parking_orig %>%
  filter(date == d) %>%
  filter(time == t)
date_filtered = date_filtered[,c(6,8,18)]

# Merge both together by 
df_plot = merge(locations, date_filtered, by="SourceElementKey", all=TRUE)
rm(locations, date_filtered)

# Get map
map = get_map("Seattle",
              zoom = 13,
              maptype = "toner-lite") # Others are toner-lite, terrain-lines





# --------

ui <- fluidPage(
  sidebarLayout(
    # Inputs
    sidebarPanel(
     
      # Date input
      dateInput(inputId = "date", label = "Select Date", 
                value = "2019-03-15", min = min(parking_orig$date), max = max(parking_orig$date),
                format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                language = "en", width = NULL, autoclose = TRUE,
                datesdisabled = NULL, daysofweekdisabled = NULL)
     
       # Select input
      selectInput(inputId = "date",
                  label = "Date",
                  choices = c("gender", "country"),
                  selected = "gender"),
      
      # Slider Input
      sliderInput(inputId = "a",
                  label = "",
                  min = 0, max = 1,
                  step = 0.05,
                  value = 0.7),
      
      numericInput(inputId = "n",
                   label = "Sample size:",
                   min = 1, max = nrow(grade),
                   step = 1,
                   value = 50)
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      textOutput(outputId = "correlation")
    )
    
  )
)

server <- function(input, output) {
  output$scatterplot <- renderPlot({
    ggplot(data = grade[1:input$n,], aes_string(x = input$x, y = input$y, col = input$c)) +
      geom_point(size = input$s, alpha=input$a)
    
    
    # Mapping with ggmmap and ggplot
    ggmap(map) + 
      geom_point(data = df_plot, 
                 mapping = aes(x = lon,
                               y = lat,
                               color = freePercent
                               #,size = ParkingSpaceCount
                 ),
                 alpha=1
                 #,size=2
      ) +
      # Limits on y and x-axis: excluding some values
      ylim(47.59, 47.64) +
      xlim(-122.375, -122.3) +
      # Set color gradient
      scale_colour_gradient(low = "red2", high = "green3",                           # Choose better colors
                            space = "Lab", na.value = "grey50", guide = "colourbar",
                            aesthetics = "colour")
  })
}

# Create a Shiny app 
shinyApp(ui = ui, server = server)





