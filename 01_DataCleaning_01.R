# Description ----------------------------------------

# In this script we will:
# - organize our data
# - select features / omitt columns
# - merge dataframes

# The aim is to create one dataframe, which is:
# - easier to handle (less rows, less columns)
# - free of unnecessary data


# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)

# Clear workspace
rm(list=ls())
graphics.off()

# Load data (find sources __________)
dates = fread("../02_Business_Analytics_Data/dates.csv")
dates = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/dates.csv")
holidays = fread("../02_Business_Analytics_Data/holidays.csv")
holidays = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/holidays.csv")
parking_orig = fread("../02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")
parking_orig = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")
# 20th to 27th March
weather_01 = read_csv2("../02_Business_Analytics_Data/history_export_2019-03-27T17_13_57.csv", skip =11)
weather_01 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-03-27T17_13_57.csv", skip =11)
# 25th March to 8th April
weather_02 = read_csv2("../02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
weather_02 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
# events = fread("../02_Business_Analytics_Data/events.csv")
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


# Clean "parking" Dataset ----------------------------

# Show structure of "parking" dataset
str(parking_orig)
# Create small subset to better handle following steps
parking = parking_orig[c(1:1000)]

# Omitt Columns
# Check whether columns are necessary/empty
unique(parking$PaidParkingRate)
unique(parking$`2010 Census Tracts`)
unique(parking$`City Council Districts`)
# Omitt
parking = parking[, -c(10, 13, 15)]

# Clean Location
# Separate location into latitude and longitude
parking$Location = gsub("POINT \\(", "", parking$Location)
parking$Location = gsub("\\)", "", parking$Location)
parking = separate(parking, Location, into = c("lon", "lat"), sep=" ")

# Clean Date and Time
# Separate into date and time
parking = separate(parking, OccupancyDateTime, into = c("date", "time", "am/pm"), sep=" ")
# Edit date format
parking$date = as.Date(parking$date, "%m/%d/%Y")
# Edit time format
parking$time = paste0(parking$time, " ", parking$`am/pm`)
parking$time = format(strptime(parking$time, "%I:%M:%S %p"), format="%H:%M")
# Omitt am/pm column
parking = parking[,-3]


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

# rm(weather_01, weather_02)


# Merge all Datasets ---------------------------------

# Merge "holidays" and "dates" datasets
df0 = merge(holidays, dates, by="date", all=TRUE)
# Turn is_holiday into a binary variable
df0[is.na(df0$is_holiday),"is_holiday"] = 0
# Remove holiday name column
df0 = df0[,-2]

# rm(dates, holidays)

# Merge "weather" with temporary dataframe "df0"
df0 = merge(weather, df0, by="date", all=TRUE)

# rm(dates, holidays)

# Merging the next dataframe will get more difficult. Different times??? help!
