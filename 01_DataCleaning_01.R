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
parking_orig = fread("../02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")
# Because of OneDrive we need to load from two different paths
parking_orig = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")


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

# Aggregate -----

# Aggregate per hour
# CODE FEHLT


# Save -----

# Remove unnecessary dataframes
# rm( # FILL DIS)

# save.image(file = "../02_Business_Analytics_Data/df_set_01.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")