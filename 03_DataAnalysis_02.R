# Description ---------------

# In this script
# - try to cluster

  

# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forecast)
library("tseries")
library(sp)
library(rgdal)
library(geosphere)
library(ggmap)

# Clear workspace
rm(list=ls())
graphics.off()

# ...
 set.seed(100)


# Load the previousely saved version of our parking data as well as new data (weather, events)
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")

# Because of OneDrive we need to load from two different paths
load("../02_Business_Analytics_Data/df_set_02_merged.RData")

# Getting one day all obs
locations = data.frame(DF_final_small[!duplicated(DF_final_small[,c("SourceElementKey","lon","lat")]),][,c(1:3)])

KMean = kmeans(locations[,2:3], 30)
locations$cluster = as.factor(KMean$cluster)
locations[,2] = as.numeric(locations[,2])
locations[,3] = as.numeric(locations[,3])


# Google maps API ----------------

register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")

map = get_map("Seattle", zoom = 13)

# ggmap(map)
# map
ggmap(map) + 
  geom_point(data=locations, 
             mapping=aes(x=lon,
                         y=lat,
                         color=cluster),
             alpha=.8) +
  ylim(47.59, 47.64) +
  xlim(-122.375, -122.3)


