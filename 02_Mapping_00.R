# Description ---------------

# In this script
# - cluster the parking meters 
# - by geographical location
# - using kmeans


# Setup ----------------------------------------------

# Load required packages
library(tidyverse)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(ggmap)

# Clear workspace
rm(list=ls())
graphics.off()

# Load the previousely saved merged version of our parking data
load("../02_Business_Analytics_Data/df_set_02_merged.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")


# Getting clusters -----------


# Filter DF_merged with specific date and hour
data_plot = DF_merged %>%
  filter(date.x == "2019-03-25") %>%
  filter(hour == 12)


data_plot$freePercent = data_plot$x / data_plot$ParkingSpaceCount
data_plot$freePercent[data_plot$freePercent < 0] <- 0

# Plotting-------------
# Google maps API 

register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")

map = get_map("Seattle", zoom = 13)

# map
ggmap(map) + 
  geom_point(data=data_plot, 
             mapping=aes(x=lon,
                         y=lat,
                         colour=freePercent),
             alpha=.7) +
  scale_colour_gradient( data_plot$freePercent, low= "red", high = "green", guide = "colourbar", aesthetics = "color") +
  ylim(47.59, 47.64) +
  xlim(-122.375, -122.3)








