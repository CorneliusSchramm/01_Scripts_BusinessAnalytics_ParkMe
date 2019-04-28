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


# Getting clusters -----------
locations = data.frame(parking_orig[!duplicated(parking_orig[,c("SourceElementKey","lon","lat")]),][,c(6,12,13)])

KMean = kmeans(locations[,2:3], 30)
locations$cluster = as.factor(KMean$cluster)
locations[,2] = as.numeric(locations[,2])
locations[,3] = as.numeric(locations[,3])


# Merge Back
DF_final_large = merge(locations,parking_orig, by = "SourceElementKey", all=TRUE)
DF_final_large = DF_final_large[,-c(2,3)]

DF_final_large$FreeSpots = DF_final_large$freePercent * DF_final_large$ParkingSpaceCount

# Saving the indormation columns
tempDF = DF_final_large[,c(2:5,10)]

#Aggregate by clusters
tempDF2= aggregate(DF_final_large$FreeSpots,
                   by=list(cluster=DF_final_large$cluster, date=DF_final_large$date, time= DF_final_large$time),
                   FUN=sum)

# Merging them back together #stoll mistake
DF_large_clusterd = merge(tempDF2, tempDF, 
                          by.x=c("cluster","date", "time"), 
                          by.y=c("cluster","date", "time"),
                          all.x=T, all.y=F)
