# Description ---------------

# In this script
# - cluster the parking meters 
# - by geographical location
# - using longitude and latitude


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

# Register Google Key
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")

# Load the previousely saved merged version of our parking data
load("../02_Business_Analytics_Data/df_set_02_merged.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")

# ... -----

# ...
df = DF_merged[,c(1:3)]
locations = data.frame(df[!duplicated(df[,c("SourceElementKey","lon","lat")]),][,c(1:3)])
row.names(locations) = NULL

# ...
ysteps = seq(47.64, 47.59, -0.0015)
xsteps = seq(-122.36, -122.30, 0.0025)

# Map Grid to create
map = get_map("Seattle", zoom = 13)
ggmap(map) + 
  geom_point(data=locations, 
             mapping=aes(x=lon,
                         y=lat),
             alpha=.8) +
  ylim(47.59, 47.64) +
  xlim(-122.375, -122.3) +
  geom_hline(yintercept = ysteps, color="red") +
  geom_vline(xintercept = xsteps, color="red")

for (i in seq(1,length(xsteps),1)) {
  locations[locations$lon > xsteps[i] & locations$lon < ysteps[i+1], "xcluster"] = i
}

for (i in seq(1,length(ysteps),1)) {
  locations[locations$lat < ysteps[i] & locations$lat > ysteps[i+1], "ycluster"] = i
}

locations$cluster = paste0(locations$xcluster,"-",locations$ycluster)
table(factor(as.character(locations$cluster)))

# Create numeric cluster names
tempDF = data.frame(locations[!duplicated(locations[,"cluster"]),][,c(6)])
tempDF$ClustNum = c(seq(1,nrow(tempDF), 1))
colnames(tempDF) = c("cluster","ClustNum")
locations = locations %>%
  left_join(tempDF, by ="cluster")

# Merge cluster into main dataframe
locations = locations[,c(1,6,7)]
DF_merged = merge(locations, DF_merged, by="SourceElementKey")

# Renaming
colnames(DF_merged)[8]= c("date")
colnames(DF_merged)[26]= c("FreeSpots")
colnames(DF_merged)[2:3]= c("ClusterLabel","cluster")

# Aggregating by clusters----

# Saving the information columns to temporary dataframe
# Making mergeCol
DF_merged = transform(DF_merged, MergeCol=paste(date, hour,cluster ,sep="_"))

tempDF = data.frame(DF_merged[!duplicated(DF_merged[,"MergeCol"]),][,])

# Aggregate by clusters
tempDF2 = aggregate(DF_merged$FreeSpots,
                    by = list(cluster = DF_merged$cluster, 
                              date = DF_merged$date, 
                              hour = DF_merged$hour),
                    FUN = sum)

# Making mergeCol
tempDF2 = transform(tempDF2, MergeCol=paste(date, hour,cluster ,sep="_"))

# Merging back together
DF_Rastclust = tempDF2 %>%
  left_join(tempDF, by= "MergeCol")

# Sorting
DF_Rastclust = DF_Rastclust[,-c(6,7,9,10,13,14,15,16,31)]
DF_Rastclust = DF_Rastclust[,-6]

# Renaming
colnames(DF_Rastclust)[1:4]= c("cluster", "date", "hour", "FreeSpots")

# Save ----- 

rm(df, DF_merged, xsteps, ysteps, locations, tempDF, tempDF2, i,map)

# save.image(file = "../02_Business_Analytics_Data/df_set_03_rasterCluster.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_03_rasterCluster.RData")

