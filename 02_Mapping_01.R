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


##########
locations = data.frame(unique(parking_orig$Location))
colnames(locations) = c("Location")
locations$Location = gsub("POINT \\(", "", locations$Location)
locations$Location = gsub("\\)", "", locations$Location)
locations = separate(locations, Location, into = c("lon", "lat"), sep=" ")

ggplot() +
  geom_point(data=locations, aes(x=lon, y=lat))

register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")

# AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs


map.seattle_city <- qmap("seattle", zoom = 11, source="stamen", maptype="toner",darken = c(.3,"#BBBBBB"))
map.seattle_city
#########
length(unique(parking_orig$SourceElementKey))

parking_one = parking_orig %>%
  filter(SourceElementKey==11670)

parking_one = separate(parking_one, OccupancyDateTime, into = c("date", "time", "am/pm"), sep=" ")


# Edit time format
parking_one$time = paste0(parking_one$time, " ", parking_one$`am/pm`)
parking_one$time = format(strptime(parking_one$time, "%I:%M:%S %p"), format="%H:%M")
parking_one = parking_one %>%
  filter(date=="03/18/2019")

map = get_map("Seattle", zoom = 10)
ggmap(map)

map + geom_point(data=d, aes(x=lon, y=lat), color="red", size=30, alpha=0.5)