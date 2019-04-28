# Description ---------------

# In this script
# - try to cluster

  

# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(sp)
library(rgdal)
library(geosphere)
library(ggmap)

# Clear workspace
rm(list=ls())
graphics.off()

# ...
 set.seed(100)


# Load the previousely saved version of our parking data as well as new data (weather, events)
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")

# Because of OneDrive we need to load from two different paths
load("../02_Business_Analytics_Data/df_set_02_merged.RData")

# Getting clusters
locations = data.frame(DF_final_small[!duplicated(DF_final_small[,c("SourceElementKey","lon","lat")]),][,c(1:3)])

KMean = kmeans(locations[,2:3], 30)
locations$cluster = as.factor(KMean$cluster)
locations[,2] = as.numeric(locations[,2])
locations[,3] = as.numeric(locations[,3])


# Google maps API ----------------

register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")

map = get_map("Seattle", zoom = 13)

# ggmap(map)
# map
ggmap(map) + 
  geom_point(data=locations, 
             mapping=aes(x=lon,
                         y=lat,
                         color=cluster),
             alpha=.8) +
  ylim(47.59, 47.64) +
  xlim(-122.375, -122.3)

# Merge Back
DF_final_small = merge(locations,DF_final_small, by = "SourceElementKey", all=TRUE)
DF_final_small = DF_final_small[,-c(2,3)]

DF_final_small$FreeSpots = DF_final_small$freePercent * DF_final_small$ParkingSpaceCount

#Aggregate by clusters
test= aggregate(DF_final_small$FreeSpots,
                by=list(cluster=DF_final_small$cluster, date=DF_final_small$date.x, time= DF_final_small$time),
                FUN=sum)

parking_filtered = test
# Merge date and time into one cell
parking_filtered$datetime = paste(parking_filtered$date, parking_filtered$time)
parking_filtered = parking_filtered %>%
  select(datetime, everything())
# Right format
parking_filtered$datetime = as.POSIXct(parking_filtered$datetime, format="%Y-%m-%d %H:%M")

# Order by date and time
parking_filtered = parking_filtered[order(parking_filtered$cluster),]

# Reset index
rownames(parking_filtered) = NULL


# Remove unwanted columns
parking_filtered = parking_filtered[, c(1,2,3,5)]


# Plot parking density over one day
# Choose date
example_date1 = "2019-03-25"
example_cluster = 1

data_plot = parking_filtered %>%
  filter(parking_filtered$date == example_date1, parking_filtered$cluster == example_cluster) 

ggplot(data_plot) +
  geom_line(aes(x=datetime, y=x))

# Plot parking distribution over more days
# Choose dates
example_date2 = "2019-03-30"
data_plot = parking_filtered %>%
  filter(date >= example_date1 & date <= example_date2, parking_filtered$cluster == example_cluster)

ggplot(data_plot) +
  geom_line(aes(x=datetime, y=x))

# Clean outliers
count_ts = ts(data_plot[, c('x')])

data_plot$clean_cnt = tsclean(count_ts)

ggplot() +
  geom_line(data = data_plot, aes(x = datetime, y = clean_cnt)) + ylab('Cleaned Parking Count')

# Decompose Data
data_plot$cnt_ma60 = ma(data_plot$x, order=60)

ggplot() +
  geom_line(data = data_plot, aes(x = datetime, y = x, colour = "green")) +
  geom_line(data = data_plot, aes(x = datetime, y = cnt_ma60, colour = "red"))  +
  























# Separate into training and testing data
train_start = "2019-03-25"
train_end = "2019-03-30"
test_start = "2019-04-01"
test_end = "2019-04-01"



data_train = parking_filtered %>%
  filter(date >= train_start & date <= train_end, parking_filtered$cluster == example_cluster)
data_train = data_train[,-c(3,2)]

data_test = parking_filtered %>%
  filter(date.x >= test_start & date.x <= test_end)
data_test = data_test[,-c(3,2)]
# Forecast model via "forecast" package

ts = ts(data = data_train, frequency = 600)
msts = msts(data_train, seasonal.periods=c(6, 600))
cycle(msts)

plot.ts(ts)

model = auto.arima(ts[,2], seasonal = T)

tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

summary(model)

