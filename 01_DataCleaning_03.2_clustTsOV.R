# Description ---------------

# In this script we
# - sort the DF to prepare it for clustering by timeseries similarity

# Output format
# Columns: Datetime | SourceElement Key 1 | Source Element Key2 | ...


# Setup ----------------------------------------------

# Load required packages
library(tidyverse)
library(data.table)

# Clear workspace
rm(list=ls())
graphics.off()

# Load Data
load("../02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")


# Create dataframe ----

# Create empty time Series
start = as.POSIXct("2019-03-24") # FOR ONE WEEK !!!!
end = as.POSIXct("2019-04-23")
finalDF = as.data.table(seq(from = start,
                            by = "hour",
                            to = end))
colnames(finalDF) = "datetime"

# Lazyness related 
parking_orig = as.data.table(DF_KMclust)
parking_orig$cluster = as.numeric(parking_orig$cluster)
# Sorted list of parkingmeter SourceElementKeys
allClust = sort((unique(parking_orig$cluster)))

# Create timeseries for each individual parking meter
for (a_Clust in allClust)  {
  # First for one then build For loop around it
  parkingmeter = a_Clust
  
  # Filter one parking meter
  parking_filtered = parking_orig %>%
    filter(cluster == parkingmeter)
  
  # Merge date and hour into one cell
  parking_filtered$datetime = paste(parking_filtered$date, parking_filtered$hour)
  parking_filtered = parking_filtered %>%
    select(datetime, everything())
  
  # Right format
  parking_filtered$datetime = as.POSIXct(parking_filtered$datetime, format="%Y-%m-%d %H")
  
  # Order by date and time
  parking_filtered = parking_filtered[order(parking_filtered$datetime),]
  
  # Reset index
  rownames(parking_filtered) = NULL
  
  # Remove unwanted columns
  parking_filtered = parking_filtered[, c(1,5)] # CHECK IF COLUMNS ARE GEWD
  colnames(parking_filtered)= c("datetime", paste("Cluster", parkingmeter, sep = ""))
  
  # Overwrite finalDF to add new time series column with current Key
  finalDF= finalDF %>%
    left_join(parking_filtered, by= "datetime")
}

# Removing rows where there are only NA's for every column # ONLY REMOVE NA'S THAT OCCURE BEFORE STARTING TIME IN THIS STEP
finalDF = finalDF[-c(1:32,718:720),]
#finalDF = finalDF[rowSums(is.na(finalDF[,2:1463])) != 1462] # CHECK IF COLUMNS ARE GEWD


# Save ---- 

# Remove unnecessary dataframes
# rm(EDIT)

#save.image(file = "../02_Business_Analytics_Data/df_set_04_Sort4Clust_Clust.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_04_Sort4Clust.RData")