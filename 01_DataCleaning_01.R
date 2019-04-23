# Description ----------------------------------------

# In this script we will:
# - organize our parking data
# - select features / omitt columns

# The aim is to create one dataframe, which is:
# - easier to handle (less rows, less columns)
# - free of unnecessary data

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
parking_orig = fread("../02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")

# Because of OneDrive we need to load from two different paths
parking_orig = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")

# Clean "parking" Dataset ----------------------------

# Show structure of "parking" dataset
str(parking_orig)
# Create small subset to better handle following steps
parking = parking_orig[c(1:1000)]

# Omitt Columns
# Check whether columns are necessary/empty
unique(parking_orig$PaidParkingRate)
unique(parking_orig$`2010 Census Tracts`)
unique(parking_orig$`City Council Districts`)
# Omitt
parking = parking[, -c(10, 13, 15)]

# Clean Location
# Separate location into latitude and longitude
parking$Location = gsub("POINT \\(", "", parking$Location)
parking$Location = gsub("\\)", "", parking$Location)
parking = separate(parking, Location, into = c("lon", "lat"), sep=" ")

# Clean Date and Time
# Separate into date and time
parking = separate(parking, OccupancyDateTime, into = c("date", "time", "am/pm"), sep=" ")
# Edit date format
parking$date = as.Date(parking$date, "%m/%d/%Y")
# Edit time format
parking$time = paste0(parking$time, " ", parking$`am/pm`)
parking$time = format(strptime(parking$time, "%I:%M:%S %p"), format="%H:%M")
# Omitt am/pm column
parking = parking[,-3]

# Create colum with occupancy percentage (/free parking) of a given parking section at any given point in time
parking$occupancyPercent = parking$PaidOccupancy / parking$ParkingSpaceCount
parking$freePercent = 1- parking$occupancyPercent

# Cleaning "parking_orig" -----------------
# now that we see that everything works the way we like in the smaller parking data set, we do it for the whole(parking_orig)
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

# Create colum with occupancy percentage (/free parking) of a given parking section at any given point in time
# this will most likely be our Y-hat
parking_orig$occupancyPercent = parking_orig$PaidOccupancy / parking_orig$ParkingSpaceCount
parking_orig$freePercent = 1- parking_orig$occupancyPercent

save.image(file = "../02_Business_Analytics_Data/df_set_01.RData")
save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")
