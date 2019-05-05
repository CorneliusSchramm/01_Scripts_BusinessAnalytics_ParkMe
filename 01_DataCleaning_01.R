# Description ----------------------------------------

# In this script we will:
# - organize our parking data
# - omitt empty columns


# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Clear workspace
rm(list=ls())
graphics.off()

# Reading our data into R
parking_orig = fread("../02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_ (1).csv")
# Because of OneDrive we need to load from two different paths
parking_orig = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_ (1).csv")


# Cleaning "parking_orig" -----------------

# Omitt Columns
# Check whether columns are necessary/empty
unique(parking_orig$PaidParkingRate)
unique(parking_orig$`2010 Census Tracts`)
unique(parking_orig$`City Council Districts`)
# Omitt
parking_orig = parking_orig[, -c(10, 13, 15)]

# Clean Location
# Separate location into latitude and longitude
parking_orig$Location = gsub("POINT \\(", "", parking_orig$Location)
parking_orig$Location = gsub("\\)", "", parking_orig$Location)
parking_orig = separate(parking_orig, Location, into = c("lon", "lat"), sep=" ")

# Clean Date and Time
# Separate into date and time
parking_orig = separate(parking_orig, OccupancyDateTime, into = c("date", "time", "am/pm"), sep=" ")
# Edit date format
parking_orig$date = as.Date(parking_orig$date, "%m/%d/%Y")
# Edit time format
parking_orig$time = paste0(parking_orig$time, " ", parking_orig$`am/pm`)
parking_orig$time = format(strptime(parking_orig$time, "%I:%M:%S %p"), format="%H:%M")
# Omitt am/pm column
parking_orig = parking_orig[,-3]

# Create FreeSpots Column
parking_orig$FreeSpots = parking_orig$ParkingSpaceCount - parking_orig$PaidOccupancy


# Aggregate -----

# Aggregate per hour

# Making hour column
parking_orig$hour = substr(parking_orig$time, start = 1, stop = 2)

# Removing unnecesary Columns but create copyDF of important information to merge it later
parking_InfoCopy = parking_orig[,c(6,1,18,7:13,16)]

# Making mergeCol
parking_InfoCopy = transform(parking_InfoCopy, MergCol=paste(date, hour,SourceElementKey ,sep="_"))

test = data.frame(parking_InfoCopy[!duplicated(parking_InfoCopy[,"MergCol"]),][,c(4:12)])

parking_orig = parking_orig[,c(1,6,17,18)]

# Aggregating FreeSpots
hourly_mean = aggregate(parking_orig$FreeSpots,
                        by = list(SourceElementKey = parking_orig$SourceElementKey, 
                                  date = parking_orig$date, 
                                  hour = parking_orig$hour),
                        FUN = mean)

# Making mergeCol
hourly_mean = transform(hourly_mean, MergCol=paste(date, hour,SourceElementKey ,sep="_"))

# Merging back together

DF_hourly = hourly_mean %>%
  left_join(test, by="MergCol")


# Save -----

# Remove unnecessary dataframes
# rm( # FILL DIS)

#save.image(file = "../02_Business_Analytics_Data/df_set_01.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")
