library(shiny)
library(tidyverse)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(ggmap)

#Get map of Seattle via the Google API
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")
load("../02_Business_Analytics_Data/df_set_02_merged.RData")

shinyServer(function(input, output) {
  output$plot = renderPlot({
    
    
    # Filter DF_merged with user specified date and hour
    data_plot = DF_merged %>%
      filter(date.x == input$dateInput) %>%
      filter(hour == input$hourInput)
    
    # Add free Percent Coulumn for each parkingmeter in order to make the plot look nicer
    data_plot$freePercent = data_plot$x / data_plot$ParkingSpaceCount
    data_plot$freePercent[data_plot$freePercent < 0] <- 0
    
    # Plotting-------------
    map = get_map("Seattle", zoom = 13)
    
    # Plot the final result
    ggmap(map) + 
      geom_point(data=data_plot, 
                 mapping=aes(x=lon,
                             y=lat,
                             colour=freePercent),
                             alpha=.8,
                             size = 3)   +
      scale_colour_gradient(data_plot$freePercent,
                            low= "red", 
                            high = "green",
                            guide = "colourbar", 
                            aesthetics = "color",
                            space= "lab",
                            name="Free %",
                            breaks=c(1,0.5,0),
                            labels=c("100","50","0")
                            ) +
      ylim(47.59, 47.64) +
      xlim(-122.375, -122.3)
  })

})
