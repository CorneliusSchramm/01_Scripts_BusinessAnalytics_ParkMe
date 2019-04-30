# Description ---------------

# In this script
# - sort the DF to prepare it for

# Setup ----------------------------------------------

# Load required packages

library(tidyverse)
library(data.table)
# Clear workspace
rm(list=ls())
graphics.off()

#Load Data
#load("../02_Business_Analytics_Data/df_set_02_merged.RData")

#Because of Onedrive
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")

rm(DF_merged_large_v1)

#Getting DF into right format
# COls: Datetime| SourceElement Key 1| Source Element Key2| .............



#Create empty time Series
start = as.POSIXct("2019-03-24")
end = as.POSIXct("2019-04-23")
finalDF_large = as.data.table(seq(from=start, by="min", to=end))
colnames(finalDF_large)= "datetime"

parking_orig = as.data.table(DF_merged_large_v1)
parking_orig$FreSpots = parking_orig$freePercent * parking_orig$ParkingSpaceCount

allkeys = sort((unique(DF_final_small$SourceElementKey)))

for (a_Key in allkeys)  {
  
  # First for one then build For loop around it
  parkingmeter = a_Key
  
  # Filter one parking meter
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
  
  # Reset index
  rownames(parking_filtered) = NULL
  
  # Remove unwanted columns
  parking_filtered = parking_filtered[, c(1,25)]
  colnames(parking_filtered)= c("datetime", paste("Key", parkingmeter, sep = ""))
  
  # Overwrite finalDF to add new time series column with current Key
  finalDF_large = merge(finalDF_large, parking_filtered, by= "datetime", all=TRUE)
}

# Removing rows where there are only Nas for every coulmn
finalDF_large = finalDF_large[rowSums(is.na(finalDF_large[,2:1463])) != 1462]

rm(DF_final_small, parking_filtered,parking_orig)

#save.image(file = "../02_Business_Analytics_Data/df_set_04_Sort4Clust.RData")
save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_04_Sort4Clust.RData")

