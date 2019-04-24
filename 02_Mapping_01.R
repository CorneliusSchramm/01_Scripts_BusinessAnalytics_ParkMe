# Description ----------------------------------------

# In this script we will:
# - map the different parking locations

# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)
library(ggmap)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Clear workspace
rm(list=ls())
graphics.off()

# -------

#
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")
# remove too high too low before saving image

# Reading our data into R
# parking_orig = fread("../02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")

# Because of OneDrive we need to load from two different paths
# parking_orig = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")


# Google maps API ----------------

# Register the google api key
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")


# Create mapping data and function -----------------

create_map = function(d, t) {
  # Creating dataframe "locations" with locations of all 1477 parking meters
  locations = data.frame(parking_orig[!duplicated(parking_orig[,c("SourceElementKey","lon","lat")]),][,c(6,12,13)])
  locations[,2] = as.numeric(locations[,2])
  locations[,3] = as.numeric(locations[,3])
  # hierbei gehen Nachkommastellen verloren ?!
  
  # locations = data.frame(unique(parking_orig$Location))
  # colnames(locations) = c("Location")
  # locations$Location = gsub("POINT \\(", "", locations$Location)
  # locations$Location = gsub("\\)", "", locations$Location)
  # locations = separate(locations, Location, into = c("lon", "lat"), sep=" ")
  # locations[,1] = as.numeric(locations[,1])
  # locations[,2] = as.numeric(locations[,2])
  
  # locations_2 = data.frame(unique(parking_orig$SourceElementKey))
  # colnames(locations_2) = c("parkingMeterId")
  # locations_2["Locations"] = NA
  
  # Get columns freePercent and ParkingSpaceCount for specific date
  # For "2019-03-15" "15:00"
  date_filtered = parking_orig %>%
    filter(date == d) %>%
    filter(time == t)
  date_filtered = date_filtered[,c(6,8,18)]
  
  # Merge both together by 
  df_plot = merge(locations, date_filtered, by="SourceElementKey", all=TRUE)
  rm(locations, date_filtered)
  
  
  
  # # Mapping ith QMAP
  # map = qmap("seattle", 
  #            zoom = 13, 
  #            source = "stamen", 
  #            maptype = "toner",
  #            darken = c(.3,"#BBBBBB")
  #            )
  
  map = get_map("Seattle",
                zoom = 13,
                maptype = "toner-lite") # Others are toner-lite, terrain-lines
  
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
    # Styling
    # Remove Axis tics etc
}

# Create maps for different times and plot next to each other
p1 = create_map("2019-03-15", "12:00")
p2 = create_map("2019-03-18", "15:00")
grid.arrange(p1, p2, ncol=2)

