# Description ---------------

# In this script
# - clean weather and holiday
# - merge

# Setup ----------------------------------------------

# Load required packages
library(tidyverse)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)

# Clear workspace
rm(list=ls())
graphics.off()

# Load the previousely saved version of our parking data as well as new data (weather, events)
load("../02_Business_Analytics_Data/df_set_01.RData")
events = fread("../02_Business_Analytics_Data/Special_Events_Permits.csv")
weather = fread("../02_Business_Analytics_Data/weather_all.csv", header = T)

# Because of OneDrive we need to load from two different paths
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")
events = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Special_Events_Permits.csv")
weather = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/weather.csv", header = T)


# Cleaning ---------------------

events = separate(events,`Event End Date`, c("End.date", "End.time", "End AM/PM"), sep=" ")
events$End.date = as.Date(events$End.date, "%m/%d/%Y")
events = separate(events,`Event Start Date`, c("Start.date", "Start.time", "Start AM/PM"), sep=" ")
events$Start.date = as.Date(events$Start.date, "%m/%d/%Y")
colnames(events)[8] = "date.x"

# # Differ between events that have been granted and events that are in process
# complete_index = which(events$`Permit Status`== "Complete")
# events = events[complete_index]

# Check wether there are events that last longer than a day
events = events[events$date.x >= "2019-03-25" & events$date.x <= "2019-04-22",]
which((events$date.x != events$End.date))

# Choose relevant columns
events = events[, c(8, 15, 19)]

# Separate rows with multiple locations into multiple rows
events = separate_rows(events, `Event Location - Neighborhood`, convert = TRUE, sep=" / ")
events = separate_rows(events, `Event Location - Neighborhood`, convert = TRUE, sep=";")

# Clean weather data
weather$date = as.Date(with(weather, paste(Year, Month, Day, Hour, sep="-")), "%Y-%m-%d")

# Create merge column
weather = transform(weather, MergCol = paste(date, Hour, sep="_"))

# Create merge Column in parking data set
DF_hourly$hour = as.numeric(DF_hourly$hour)
DF_hourly = transform(DF_hourly, MergCol=paste(date, hour, sep="_"))


# Merging --------------------

# Merge weather and parking
DF_merged = merge(DF_hourly, weather, 
                            by="MergCol", all.x=TRUE)

# Merge resulting DF with events
DF_merged = merge(DF_merged, events, 
                           by.x=c("date.x","PaidParkingArea"), 
                           by.y=c("date.x","Event Location - Neighborhood"),
                           all.y=F, all.x=T)

# If no event takes place we handle it as an event with attendance of 0
DF_merged[is.na(DF_merged$Attendance),"Attendance"] = 0


# Cleaning ----

# Getting Weekday
DF_merged$Weekday = weekdays(DF_merged$date.x)
DF_merged[DF_merged$Weekday == "Samstag" | DF_merged$Weekday == "Sonntag","is_we"] = 1
DF_merged[is.na(DF_merged$is_we),"is_we"] = 0

# Sorting Columns
DF_merged = DF_merged[,c(4,11,12,34,35, 1,5,7,8,19:31,33,6)]
#check which columns have NAs
colnames(DF_merged)[colSums(is.na(DF_merged)) > 0]
#omit dat shit
DF_merged = na.omit(DF_merged)

# Save -----

# Remove unnecessary dataframes
# STILL HAVE TO EDIT DIS
rm(DF_hourly,test)

save.image(file = "../02_Business_Analytics_Data/df_set_02_merged.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")
