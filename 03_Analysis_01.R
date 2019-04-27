# Description ---------------

# In this script
# - we will forecast the parking density
# - of one parking meter
# - without features

# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forecast)

# Clear workspace
rm(list=ls())
graphics.off()

# ...
# set.seed(100)
theme_set(theme_minimal())

# Load the previousely saved version of our parking data as well as new data (weather, events)
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")
# dates = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/dates.csv")
# holidays = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/holidays.csv")
# weather_01 = read_csv2("../02_Business_Analytics_Data/weather.csv") #(25th March to 22nd April)
# weather_02 = read_csv2("../02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
# events = fread("../02_Business_Analytics_Data/events.csv")

# Because of OneDrive we need to load from two different paths
load("../02_Business_Analytics_Data/df_set_01.RData")
# dates = fread("../02_Business_Analytics_Data/dates.csv")
# holidays = fread("../02_Business_Analytics_Data/holidays.csv")
# weather_01 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/weather.csv") #(25th March to 22nd April)
# weather_02 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
# events = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/events.csv")

# Prepare data -----

# Choose parking meter
parkingmeter = 13105

# Remove unnecessary data
rm(too_high_index, too_low_index)

# Filter one parking meter
parking_filtered = parking_orig %>%
  filter(SourceElementKey == parkingmeter)

# Merge date and time into one cell
parking_filtered$datetime = paste(parking_filtered$date, parking_filtered$time)
parking_filtered = parking_filtered %>%
  select(datetime, everything())
parking_filtered$datetime = as.POSIXct(paste(parking_filtered$date, parking_filtered$time), format="%Y-%m-%d %H:%M")

# Order by date and time
parking_filtered = parking_filtered[order(parking_filtered$datetime),]
# Oder ohne die datetime spalte zu kreieren
# parking_filtered = parking_filtered[order(parking_filtered$date, parking_filtered$time),]
# Reset index
rownames(parking_filtered) = NULL


# Analysis -----

# Remove unwanted columns
parking_filtered = parking_filtered[, c(1,2,19)]


# Plot parking density over one day
# Choose date
example_date1 = "2019-03-15"
data_plot = parking_filtered %>%
filter(parking_filtered$date == example_date1)
ggplot(data_plot) +
  geom_line(aes(x=datetime, y=freePercent))
# Plot parking density over two days
# Choose dates
example_date2 = "2019-03-16"
data_plot = parking_filtered %>%
  filter(parking_filtered$date == example_date1 | parking_filtered$date == example_date2)
ggplot(data_plot) +
  geom_line(aes(x=datetime, y=freePercent))

# Separate into training and testing data
data_train = parking_filtered[1:(0.75*nrow(parking_filtered)),c(1,3)]
data_test = parking_filtered[-(1:(0.75*nrow(parking_filtered))),c(1,3)]

# Forecast model via "forecast" package
ts = ts(data = data_train,
           frequency = 525600,
           start = c(2019, 3, 15, 12, 0))

plot.ts(ts)

model = auto.arima(ts[,2])
summary(model)

# Forecast 
# forcast days
fc_days = 5
fc_time = fc_days*24*60
fc = forecast(model, h=fc_time)

plot(fc)



