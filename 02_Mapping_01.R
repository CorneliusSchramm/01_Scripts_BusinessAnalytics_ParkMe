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


load(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")

# Creating dataframe "locations" with locations of all 1477 parking meters --------

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


locations = data.frame(parking_orig[!duplicated(parking_orig[,c("SourceElementKey","lon","lat")]),][,c(6,12,13)])
locations[,2] = as.numeric(locations[,2])
locations[,3] = as.numeric(locations[,3])
# hierbei gehen Nachkommastellen verloren ?!

# Get columns freePercent and ParkingSpaceCount for specific date

# For "2019-03-15" "15:00"
date_filtered = parking_orig %>%
  filter(date == "2019-03-15") %>%
  filter(time == "12:00")
date_filtered = date_filtered[,c(6,8,18)]

# merge
df_plot_15.3_15.00 = merge(locations, date_filtered, by="SourceElementKey", all=TRUE)
rm(locations, date_filtered)


# Google maps API ----------------

register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")

# map = qmap("seattle", 
#             zoom = 13, 
#             source="stamen"
#   #         ,maptype="toner"
#    #       ,darken = c(.3,"#BBBBBB")
#            )
map = get_map("Seattle", zoom = 13)
# ggmap(map)
# map
ggmap(map) + 
  geom_point(data=df_plot_15.3_15.00, 
             mapping=aes(x=lon,
                         y=lat,
                         color=freePercent,
                         size=ParkingSpaceCount),
             alpha=.8) +
  ylim(47.59, 47.64) +
  xlim(-122.375, -122.3)
  # xlim(min(locations$lon), max(locations$lon)) +
  # ylim(min(locations$lat), max(locations$lat))

ggplot() +
  geom_point(data=locations, mapping= aes(x=lon, y=lat),size=.5)


########

# minus ändern Relation 
# Limits
# Colors: 
# Size: absolute parking lots
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



# -------------------------------

load("../Schramm, Cornelius - 02_Business_Analytics_Data/locationsToPlot.RData")
loc2 = data.frame(locations[!duplicated(locations[,"cluster"]),][,])


map = get_map("Seattle", zoom = 13)
ggmap(map) + 
  geom_point(data=locations,
             mapping=aes(x=lon,
                         y=lat,
                         color=cluster)) +
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
  scale_size(range = c(1, 30)) +
  ylim(47.59, 47.64) +
  xlim(-122.375, -122.3) +
  theme(line = element_blank(),  # remove the background, tickmarks, etc
      axis.text=element_blank(),
      axis.title=element_blank(),
      panel.background = element_blank())
