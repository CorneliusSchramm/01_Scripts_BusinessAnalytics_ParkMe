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
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")


# Getting clusters -----------

# Get rid of unnecessary columns for plotting
locations = data.frame(DF_merged[!duplicated(DF_merged[,c("SourceElementKey","lon","lat")]),][,c(1:3)])

# Create cluster column and prepare for plotting
KMean = kmeans(locations[,2:3], 30)
locations$cluster = as.factor(KMean$cluster)

# Merge cluster back to all times and locations
DF_clustered = merge(locations, DF_merged, by = "SourceElementKey", all=TRUE)
DF_clustered = DF_clustered[,-c(5,6)] 

# Renaming
colnames(DF_clustered)[2:3]= c("lon","lat")
colnames(DF_clustered)[7]= c("date")
colnames(DF_clustered)[25]= c("FreeSpots")

# Saving the information columns to temporary dataframe
# Making mergeCol
DF_clustered = transform(DF_clustered, MergeCol=paste(date, hour,cluster ,sep="_"))
tempDF = data.frame(DF_clustered[!duplicated(DF_clustered[,"MergeCol"]),][,])

# Aggregate by clusters
tempDF2 = aggregate(DF_clustered$FreeSpots,
                   by = list(cluster = DF_clustered$cluster, 
                           date = DF_clustered$date, 
                           hour = DF_clustered$hour),
                   FUN = sum)

# Making mergeCol
tempDF2 = transform(tempDF2, MergeCol=paste(date, hour,cluster ,sep="_"))

# Merging back together
DF_KMclust = tempDF2 %>%
  left_join(tempDF, by= "MergeCol")

# Sorting
DF_KMclust = DF_KMclust[,-c(5,6,9,12,13,14,15,30)]

# Renaming
colnames(DF_KMclust)[1:4]= c("cluster", "date", "hour", "FreeSpots")
DF_KMclust = DF_KMclust[,-c(5,6)]

# Save ----

# Remove unnecessary dataframes
# rm(DF_clustered, DF_merged, KMean, locations, tempDF, tempDF2)

# save.image(file = "../02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")


