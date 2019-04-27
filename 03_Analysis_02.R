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
library("tseries")
# Clear workspace
rm(list=ls())
graphics.off()

# ...
# set.seed(100)
theme_set(theme_minimal())

# Load the previousely saved version of our parking data as well as new data (weather, events)
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")
# dates = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/dates.csv")
# holidays = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/holidays.csv")
# weather_01 = read_csv2("../02_Business_Analytics_Data/weather.csv") #(25th March to 22nd April)
# weather_02 = read_csv2("../02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
# events = fread("../02_Business_Analytics_Data/events.csv")

# Because of OneDrive we need to load from two different paths
load("../02_Business_Analytics_Data/df_set_02_merged.RData")
# dates = fread("../02_Business_Analytics_Data/dates.csv")
# holidays = fread("../02_Business_Analytics_Data/holidays.csv")
# weather_01 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/weather.csv") #(25th March to 22nd April)
# weather_02 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
# events = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/events.csv")

# Prepare data -----

# Choose parking meter
parkingmeter = 13105

# Filter one parking meter
parking_orig = DF_final_small
parking_filtered = parking_orig %>%
  filter(SourceElementKey == parkingmeter)

# Merge date and time into one cell
parking_filtered$datetime = paste(parking_filtered$date, parking_filtered$time)
parking_filtered = parking_filtered %>%
  select(datetime, everything())
# Right format
parking_filtered$datetime = as.POSIXct(parking_filtered$datetime, format="%Y-%m-%d %H:%M")

# Order by date and time
parking_filtered = parking_filtered[order(parking_filtered$datetime),]
# Oder ohne die datetime spalte zu kreieren
# parking_filtered = parking_filtered[order(parking_filtered$date, parking_filtered$time),]
# Reset index
rownames(parking_filtered) = NULL


# Analysis -----

# Remove unwanted columns
parking_filtered = parking_filtered[, c(1,6,24)]


# Plot parking density over one day
# Choose date
example_date1 = "2019-03-25"

data_plot = parking_filtered %>%
  filter(parking_filtered$date.x == example_date1)

ggplot(data_plot) +
  geom_line(aes(x=datetime, y=freePercent))

# Plot parking density over two days
# Choose dates
example_date2 = "2019-03-30"
data_plot = parking_filtered %>%
  filter(date.x >= example_date1 & date.x <= example_date2)

ggplot(data_plot) +
  geom_line(aes(x=datetime, y=freePercent))

# Separate into training and testing data
train_start = "2019-03-25"
train_end = "2019-03-30"
test_start = "2019-04-01"
test_end = "2019-04-01"



data_train = parking_filtered %>%
  filter(date.x >= train_start & date.x <= train_end)
data_train = data_train[,-2]

data_test = parking_filtered %>%
  filter(date.x >= test_start & date.x <= test_end)
data_test = data_test[,-2]
# Forecast model via "forecast" package

ts = ts(data = data_train, frequency = 600)
msts = msts(data_train, seasonal.periods=c(6, 600))
cycle(ts)

plot.ts(ts)

model = auto.arima(ts[,2], seasonal = T)

tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

summary(model)

# Forecast 
# forcast days
fc_days = 1
fc_time = fc_days*24*60
fc = forecast(model)

plot(fc)


