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
colnames(events)[8] = "date.x"

# # Differ between events that have been granted and events that are in process
# complete_index = which(events$`Permit Status`== "Complete")
# events = events[complete_index]

# Check wether there are events that last longer than a day
events = events[events$date.x >= "2019-03-25" & events$date.x <= "2019-04-22",]
which((events$date.x != events$End.date))

# Choose relevant columns
events = events[, c(8, 15, 19)]

# ...
events = separate_rows(events, `Event Location - Neighborhood`, convert = TRUE, sep=" / ")
events = separate_rows(events, `Event Location - Neighborhood`, convert = TRUE, sep=";")

# Clean Holiday
# holidays = holidays[-1, -c(3,4) ]

# Clean weather data
weather$date = as.Date(with(weather, paste(Year, Month, Day, Hour, sep="-")), "%Y-%m-%d")
# Making Merge Column
weather = transform(weather, MergCol = paste(date, Hour, sep="_"))

parking = parking_orig[parking_orig$date>= "2019-03-25" & parking_orig$date <= "2019-04-1",]
# Clean parking -> make merge Column
parking$hour = as.numeric(substr(parking$time, start = 1, stop = 2))
parking = transform(parking, MergCol=paste(date, hour, sep="_"))
# Same for parking_orig
memory.limit(70000) ## So we dont get an error due to too big of an vector 
parking_orig$hour = as.numeric(substr(parking_orig$time, start = 1, stop = 2))
parking_orig = transform(parking_orig, MergCol=paste(date, hour, sep="_"))


# Merging --------------------



# Merging weather and parking----------
DF_merged_small_v1 = merge(parking, weather, 
                           by="MergCol", all.x=TRUE)
# Same for parking_orig
DF_merged_large_v1 = merge(parking_orig, weather, 
                            by="MergCol", all.x=TRUE)

# Merging DF_merged_v1 and events -------------
DF_merged_small_v2 = merge(DF_merged_small_v1, events, 
                           by.x=c("date.x","PaidParkingArea"), 
                           by.y=c("date.x","Event Location - Neighborhood"),
                           all.y=F, all.x=T)
DF_merged_small_v2[is.na(DF_merged_small_v2$Attendance),"Attendance"] = 0
# Same for DF_merged_large
DF_merged_large_v2 = merge(DF_merged_large_v1, events, 
                           by.x=c("date.x","PaidParkingArea"), 
                           by.y=c("date.x","Event Location - Neighborhood"),
                           all.y=F, all.x=T)
DF_merged_large_v2[is.na(DF_merged_large_v2$Attendance),"Attendance"] = 0

# Getting Weekday -----

DF_merged_small_v2$Weekday = weekdays(DF_merged_small_v2$date.x)
DF_merged_small_v2[DF_merged_small_v2$Weekday == "Samstag" | DF_merged_small_v2$Weekday == "Sonntag","is_we"] = 1
DF_merged_small_v2[is.na(DF_merged_small_v2$is_we),"is_we"] = 0

# Same for parking_orig
DF_merged_large_v2$Weekday = weekdays(DF_merged_large_v2$date.x)
DF_merged_large_v2[DF_merged_large_v2$Weekday == "Samstag" | DF_merged_large_v2$Weekday == "Sonntag","is_we"] = 1
DF_merged_large_v2[is.na(DF_merged_large_v2$is_we),"is_we"] = 0

# Sorting Columns
DF_final_small = DF_merged_small_v2[,c(8,13,14,41,1,42,4,9,12,17,26:35,40,10,19)]


#WHat does is this line supposed to do?
#DF_final_small = DF_final_small[DF_final_small$date.x >= "2019-03-25" & DF_final_small$date.x <= "2019-04-22",]

DF_final_large = DF_merged_large_v2[,c(8,13,14,41,1,42,4,9,12,17,26:35,40,10,19)]

#What is this supposed to do?
# DF_final_large = DF_final_large[DF_final_large$date.x >= "2019-03-25" & DF_final_large$date.x <= "2019-04-22",]

rm(dates, DF_merged_small_v1, DF_merged_small_v2, 
   events, holidays,parking, parking_orig, weather, 
   too_high_index, too_low_index)

save.image(file = "../02_Business_Analytics_Data/df_set_02_merged.RData")
save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")
