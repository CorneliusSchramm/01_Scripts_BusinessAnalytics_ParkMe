# Description ---------------

# In this script
# - cluster the parking meters 
# - by geographical location
# - using kmeans


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

# Load the previousely saved merged version of our parking data
load("../02_Business_Analytics_Data/df_set_02_merged.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")


# Getting clusters -----------

# Get rid of unnecessary columns for plotting
locations = data.frame(DF_merged[!duplicated(DF_merged[,c("SourceElementKey","lon","lat")]),][,c(6,12,13)]) # COLUMNS STIMMEN NICHT MEHR

# Create cluster column and prepare for plotting
KMean = kmeans(locations[,2:3], 30) # COLUMNS STIMMEN NICHT MEHR ???
locations$cluster = as.factor(KMean$cluster)
locations[,2] = as.numeric(locations[,2])
locations[,3] = as.numeric(locations[,3])

# Merge cluster back to all times and locations
DF_merged = merge(locations, DF_merged, by = "SourceElementKey", all=TRUE)
DF_merged = DF_merged[,-c(2,3,8,9,13:20)] # COLUMNS STIMMEN NICHT MEHR

# Saving the information columns to temporary dataframe
tempDF1 = DF_merged[,c(1:2,6:8)] # COLUMNS STIMMEN NICHT MEHR ???

# Aggregate by clusters
tempDF2 = aggregate(DF_final_large$FreeSpots,
                   by = list(cluster = DF_final_large$cluster, 
                           date = DF_final_large$date, 
                           time = DF_final_large$time),
                   FUN = sum)

# Merging them back together                           # NOT WORKING -- MISTAKE
# DF_clustered = merge(tempDF2, tempDF1, by ="cluster")
# DF_large_clusterd = merge(tempDF2, tempDF1, 
#                           by.x=c("cluster","date", "time"), 
#                           by.y=c("cluster","date", "time"),
#                           all.x=T, all.y=F)

# Save ----

# Remove unnecessary dataframes
# rm(# DO DIS)

# save.image(file = "../02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")


