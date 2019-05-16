library(shiny)
library(tidyverse)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(ggmap)

#Get map of Seattle via the Google API
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs") # Für Seattle map
load("../02_Business_Analytics_Data/df_set_02_merged.RData") # Für history plot
load("../02_Business_Analytics_Data/locationsToPlot.RData") # Für Blubberblasenplot
load("../02_Business_Analytics_Data/4SHINY.RData") # Für Preds und Preds plot

loc2 = data.frame(locations[!duplicated(locations[,"cluster"]),][,])

shinyServer(function(input, output) {
  
  # output$ClusterPred == renderText ({
  #   Userpred = preds %>%
  #     filter(date ==input$dateInput) %>%
  #     filter(hour == input$hourInput) %>%
  #     filter(cluster == input$clusterInput)
  #   print(Userpred$FreeSpotsCluster)
  # })
  
  output$plotPred = renderPlot ({
    data_plot_pred = shinyPredPlot %>%
      filter(date == input$dateInput) %>%
      filter(hour == input$hourInput )
    
    # Plotting
    map = get_map("Seattle", zoom = 13)
    
    # Plot the final result
    ggmap(map) + 
      geom_point(data=data_plot_pred, 
                 mapping=aes(x=lon,
                             y=lat,
                             colour=clustFreePerc),
                 alpha=.8,
                 size = 3)   +
      scale_colour_gradient(data_plot_pred$clustFreePerc,
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
      xlim(-122.375, -122.3)+
      theme(line = element_blank(),  # remove the background, tickmarks, etc
            axis.text=element_blank(),
            axis.title=element_blank(),
            panel.background = element_blank())
    
  })
  
  
  output$plot = renderPlot({
    # Filter DF_merged with user specified date and hour
    data_plot = DF_merged %>%
      filter(date == input$dateInputH) %>%
      filter(hour == input$hourInputH)
    
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
      xlim(-122.375, -122.3)+
      theme(line = element_blank(),  # remove the background, tickmarks, etc
            axis.text=element_blank(),
            axis.title=element_blank(),
            panel.background = element_blank())
  })
  
  output$plotW2Go = renderPlot({
    map2 = get_map("Seattle", zoom = 13)
    ggmap(map2) + 
      geom_point(data=locations,
                 mapping=aes(x=lon,
                             y=lat,
                             color=cluster),
                 size= 3) +
      geom_point(data=loc2, alpha=.3,
                 mapping=aes(x=lon.center,
                             y=lat.center,
                             size=clustCap,
                             color=cluster)) +
      geom_label(data=loc2, 
                 mapping=aes(x=lon.center,
                             y=lat.center,
                             label=cluster),
                 fontface = "bold", fill="white") +
      scale_size(range = c(1, 44)) +
      ylim(47.59, 47.64) +
      xlim(-122.375, -122.3) +
      theme(line = element_blank(),  # remove the background, tickmarks, etc
            axis.text=element_blank(),
            axis.title=element_blank(),
            panel.background = element_blank(),
            legend.position = "none")

  })

})
