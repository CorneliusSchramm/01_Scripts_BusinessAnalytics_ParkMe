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
library(ggmap)

# Clear workspace
rm(list=ls())
graphics.off()

# Load the previousely saved merged version of our parking data
load("../02_Business_Analytics_Data/df_set_02_merged.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")

# Google Maps API
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")


# Getting clusters -----------

# Get rid of unnecessary columns for plotting
locations = data.frame(DF_merged[!duplicated(DF_merged[,c("SourceElementKey","lon","lat")]),][,c(1:3)])

# Create cluster column and prepare for plotting
set.seed(100)
KMean = kmeans(locations[,2:3], 30)
locations$cluster = as.factor(KMean$cluster)

# Merge cluster back to all times and locations
DF_clustered = merge(locations, DF_merged, by = "SourceElementKey", all=TRUE)
DF_clustered = DF_clustered[,-c(5,6)] 

# Renaming
colnames(DF_clustered)[2:3]= c("lon","lat")

# Saving the information columns to temporary dataframe
# Making mergeCol
DF_clustered = transform(DF_clustered, MergeCol=paste(date, hour,cluster ,sep="_"))
tempDF = data.frame(DF_clustered[!duplicated(DF_clustered[,"MergeCol"]),][,])

# For imputing
DF_clustered_slim = DF_clustered[,c(1,4,7,8,10,25,26)]

# Save this one for relation parking meters to kmean-Clusters
# save(DF_clustered_slim, file="../Schramm, Cornelius - 02_Business_Analytics_Data/pm_kmClust_relation.RData")

# Aggregate by clusters
tempDF2 = aggregate(list(DF_clustered$freeParkingSpaces,DF_clustered$ParkingSpaceCount),
                   by = list(cluster = DF_clustered$cluster, 
                           date = DF_clustered$date, 
                           hour = DF_clustered$hour),
                   FUN = sum)

# Making mergeCol
tempDF2 = transform(tempDF2, MergeCol=paste(date, hour, cluster ,sep="_"))

# Merging back together
DF_KMclust = tempDF2 %>%
  left_join(tempDF, by= "MergeCol")

# Sorting
DF_KMclust = DF_KMclust[,-c(6,7,10,13,14,15,16,31,32)]

# Renaming
colnames(DF_KMclust)[1:5]= c("cluster", "date", "hour", "freeParkingSpaces", "ClustCap")
DF_KMclust = DF_KMclust[,-c(6,7)]

# Create freePercent for clusters
DF_KMclust$freePercent_kmClust = DF_KMclust$freeParkingSpaces / DF_KMclust$ClustCap

# Are there any false percentages?
filter(DF_KMclust, freePercent_kmClust>1)
filter(DF_KMclust, freePercent_kmClust<0)
# Nahh

# Plot distribution
ggplot(filter(DF_KMclust, date=="2019-03-30" & hour== 14)) +
  geom_histogram(aes(freePercent_kmClust), fill="green4", bins=30) +
  xlim(0,1)

# Plot cluster
map = get_map("Seattle", zoom = 13)
ggmap(map) + 
  geom_point(data=locations, 
             mapping=aes(x=lon,
                         y=lat,
                         color=cluster),
             alpha=.8) +
  ylim(47.59, 47.64) +
  xlim(-122.375, -122.3)

# Create plotting dataframe 
centers = as.data.frame(KMean$centers)
centers$cluster = seq(1,30)
colnames(centers)[1:2] = c("lon.center", "lat.center")
locations = merge(locations, centers, by = "cluster", all=T)
# save(locations, file="../Schramm, Cornelius - 02_Business_Analytics_Data/locations.RData")


# Save ----

# Remove unnecessary dataframes
# rm(DF_clustered, DF_merged, KMean, tempDF, tempDF2, map, locations, DF_clustered_slim, centers)

# save.image(file = "../02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")


