# Description ---------------

# In this script
# - clean weather and holiday
# - merge

# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)


library(dplyr)
library(ggplot2)


# Clear workspace
rm(list=ls())
graphics.off()

# Load the previousely saved version of our parking data as well as new data (weather, events)
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")
dates = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/dates.csv")
holidays = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/holidays.csv")
weather_01 = read_csv2("../02_Business_Analytics_Data/weather.csv") #(25th March to 22nd April)
# weather_02 = read_csv2("../02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
# events = fread("../02_Business_Analytics_Data/events.csv")

# Because of OneDrive we need to load from two different paths
load("../02_Business_Analytics_Data/df_set_01.RData")
dates = fread("../02_Business_Analytics_Data/dates.csv")
holidays = fread("../02_Business_Analytics_Data/holidays.csv")
weather_01 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/weather.csv") #(25th March to 22nd April)
# weather_02 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
# events = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/events.csv")

# Clean "dates" Dataset ------------------------------

# Rename columns
colnames(dates) = c(
  "date",
  "day",
  "is_weekend",
  "is_fr/sa"
)
# Edit date format
dates$date = as.Date(dates$date, "%d.%m.%y")


# Clean "holidays" Dataset ---------------------------

# Omitt empty rows and columns
holidays = holidays[c(2:13), c(1,2,3)]
# Rename columns
colnames(holidays) = c(
  "date",
  "holiday_name",
  "is_holiday"
)
# Edit date format
holidays$date = as.Date(holidays$date, "%d.%m.%y")

# Clean "weather" Dataset ----------------------------

# Merge/stack both weather datasets
# Not done here because the first one has less columns (?)

weather = weather_02
# Create columns "date" and "time" in common format
weather$date = paste0(weather$Day, ".", weather$Month, ".", weather$Year)
weather$date = as.Date(weather$date, "%d.%m.%Y")
weather$time = paste0(weather$Hour, ":", weather$Minute)
# Sort columns
weather = select(weather, date, time, everything())