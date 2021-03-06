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
weather = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/weather_all.csv", header = T)


# Cleaning ---------------------

# Eliminate parking meters that are far off the city center
DF_hourly[,11] = as.numeric(DF_hourly[,11])
DF_hourly[,12] = as.numeric(DF_hourly[,12])
DF_hourly = DF_hourly %>%
  filter(lat < 47.64 & lat > 47.59) %>%
  filter(lon > -122.36 & lon < -122.30)

# Check if Capacity stays constant
a = sort(unique(DF_hourly$SourceElementKey))
for (i in a){
  if (length(unique(DF_hourly[DF_hourly$SourceElementKey == i,"ParkingSpaceCount"])) > 1){
    print(paste(i,unique(DF_hourly[DF_hourly$SourceElementKey == i,"ParkingSpaceCount"])))
  }
}

# Eliminate parking meters that change capacity
DF_hourly = filter(DF_hourly, SourceElementKey != 8249 &
         SourceElementKey != 9474 &
         SourceElementKey != 11397 &
         SourceElementKey != 30698 &
         SourceElementKey != 34937 &
         SourceElementKey != 56241 &
         SourceElementKey != 36358 &
         SourceElementKey != 69094)

# Transform mistaken data (120% free into 100% free)
DF_hourly$x[DF_hourly$x < 0] = 0
# No overbooked parking meters, see following lines code
# DF_hourly$freePercent[DF_hourly$freePercent > 1] = 1

# Create FreePercent Column that gives the percentage of free lots per parking meter / cluster
DF_hourly$freePercent = DF_hourly$x / DF_hourly$ParkingSpaceCount
nrow(filter(DF_hourly, freePercent<=1 & freePercent>=0)) == nrow(DF_hourly)

ggplot(DF_hourly) +
  geom_density(aes(freePercent), fill="345672",alpha=.6) +
  theme_minimal()

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
DF_merged = DF_merged[,c(4,11,12,35,36,1,5,7,8,20:32,34,6,14)]
# Check which columns have NAs
colnames(DF_merged)[colSums(is.na(DF_merged)) > 0]
# Omit dat shit
DF_merged = na.omit(DF_merged)

# Column names
colnames(DF_merged)[6] = "date"
colnames(DF_merged)[10:24] = c("temp",
                               "humidity",
                               "meanSeaLevPressure",
                               "precipitation",
                               "snowfall",
                               "TCloudCover",
                               "HCloudCover",
                               "MCloudCover",
                               "LCloudCover",
                               "sunDur",
                               "windSpeed",
                               "windDir",
                               "windGust",
                               "attendance",
                               "freeParkingSpaces")


# Save -----

# Remove unnecessary dataframes
rm(DF_hourly, weather, events, a, i)

# save.image(file = "../02_Business_Analytics_Data/df_set_02_merged.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")
