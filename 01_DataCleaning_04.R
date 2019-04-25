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
weather = fread("../02_Business_Analytics_Data/weather.csv", header = T)
#dates = fread("../02_Business_Analytics_Data/dates.csv")
#holidays = fread("../02_Business_Analytics_Data/holidays.csv")
# weather_2 = fread("../02_Business_Analytics_Data/history_export_2019-04-22T09_14_54.csv", header = T, sep = ";", skip = 11)
# weather_1 = fread("../02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", header = T, sep = ";", skip = 11)



# Because of OneDrive we need to load from two different paths
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")
events = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Special_Events_Permits.csv")
weather = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/weather.csv", header = T)
#dates = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/dates.csv")
#holidays = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/holidays.csv")
#weather_2 = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-04-22T09_14_54.csv", header = T, sep = ";", skip = 11)
#weather_1 = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", header = T, sep = ";", skip = 11)


# Cleaning ---------------------

# Clean Event Data 
events = separate(events,`Event End Date`, c("End.date", "End.time", "End AM/PM"), sep=" ")
events$End.date = as.Date(events$End.date, "%m/%d/%Y")
events = separate(events,`Event Start Date`, c("Start.date", "Start.time", "Start AM/PM"), sep=" ")
events$Start.date = as.Date(events$Start.date, "%m/%d/%Y")
events = events[, -c(9,10, 12, 13)]
events = events[, -c(1,5,6,7,14)]
colnames(events)[4] = "date.x"

complete_index = which(events$`Permit Status`== "Complete")
events = events[complete_index]

# Check wether there are events that last longer than a day
events_relevant = events[events$date.x >= "2019-03-25" & events$date.x <= "2019-04-22",]
which((events_relevant$date.x != events_relevant$End.date))
rm(complete_index, events_relevant)

# Clean Holiday
holidays = holidays[-1, -c(3,4) ]

# Clean weather data
weather$date = as.Date(with(weather, paste(Year, Month, Day, Hour, sep="-")), "%Y-%m-%d")
#Making Merge Column
weather= transform(weather, MergCol=paste(date, Hour, sep="_"))

# Clean parking -> make merge Column
parking$hour = as.numeric(substr(parking$time, start = 1, stop = 2))
parking = transform(parking, MergCol=paste(date, hour, sep="_"))
# Same for parking_orig
memory.limit(70000) ## So we dont get an error due to too big of an vector 
parking_orig$hour = as.numeric(substr(parking_orig$time, start = 1, stop = 2))
parking_orig = transform(parking_orig, MergCol=paste(date, hour, sep="_"))


# Merging --------------------

# Merging weather and parking----------
DF_merged_small_v1 = merge(parking,weather, by="MergCol", all.x=TRUE)
# Same for parking_orig
DF_merged_large_v1 = merge(parking_orig,weather, by="MergCol", all.x=TRUE)

# Merging DF_merged_small_v1 and events
DF_merged_small_v2 = merge(DF_merged_small_v1,events, by="date.x", all.x=TRUE)
# Same for parking_orig
DF_merged_large_v2 = merge(DF_merged_large_v1,events, by="date.x", all.x=TRUE)

# Getting Weekday-----
DF_merged_small_v2$Weekday = weekdays(DF_merged_small_v1$date.x)
# Same for parking_orig
DF_merged_large_v2$Weekday = weekdays(DF_merged_large_v1$date.x)

# Sorting Columns
DF_final_small = DF_merged_small_v2[,c(7,13,14,46,2,1,3,5,6,8,10,11,12,15:17,26:38,40:45, 18,19)]

DF_final_large = DF_merged_large_v2[,c(7,13,14,46,2,1,3,5,6,8,10,11,12,15:17,26:38,40:45, 18,19)]

rm(dates, DF_merged_small_v1, DF_merged_small_v2, 
   events, holidays,parking, parking_orig, weather, 
   too_high_index, too_low_index)

save.image(file = "../02_Business_Analytics_Data/df_set_02_merged.RData")
save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")