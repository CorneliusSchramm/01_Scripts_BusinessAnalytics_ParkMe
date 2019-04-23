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


# Clear workspace
rm(list=ls())
graphics.off()


# Reading our data into R
parking_orig = fread("../02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")

# Because of OneDrive we need to load from two different paths
parking_orig = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")


# Making locationsDF --------
locations = data.frame(unique(parking_orig$Location))
colnames(locations) = c("Location")
locations$Location = gsub("POINT \\(", "", locations$Location)
locations$Location = gsub("\\)", "", locations$Location)
locations = separate(locations, Location, into = c("lon", "lat"), sep=" ")
locations[,1] = as.numeric(locations[,1])
locations[,2] = as.numeric(locations[,2])

# Google maps API ----------------
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")

map.seattle_city <- qmap("seattle", zoom = 11, source="stamen", maptype="toner",darken = c(.3,"#BBBBBB"))
map.seattle_city

map = get_map("Seattle", zoom = 13)
ggmap(map)

ggmap(map) + 
  geom_point(data=locations, mapping= aes(x=lon, y=lat),size=.5) +
  xlim()
  
  
  # xlim(min(locations$lat), max(locations$lat)) +
  # ylim(min(locations$lon), max(locations$lon))

ggplot() +
  geom_point(data=locations, mapping= aes(x=lon, y=lat),size=0.5)


########
length(unique(parking_orig$SourceElementKey))

parking_one = parking_orig %>%
  filter(SourceElementKey==11670)

parking_one = separate(parking_one, OccupancyDateTime, into = c("date", "time", "am/pm"), sep=" ")


# Edit time format
parking_one$time = paste0(parking_one$time, " ", parking_one$`am/pm`)
parking_one$time = format(strptime(parking_one$time, "%I:%M:%S %p"), format="%H:%M")
parking_one = parking_one %>%
  filter(date=="03/18/2019")
