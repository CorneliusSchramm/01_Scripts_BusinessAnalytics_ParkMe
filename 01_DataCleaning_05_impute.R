# Description ---------------

# In this script
# - imputing missing values
# - deleting incomplete parking meters


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
load("../02_Business_Analytics_Data/pm_kmClust_relation.RData")
load("../02_Business_Analytics_Data/pmTsOv.RData")


# Because of Onedrive
load("../Schramm, Cornelius - 02_Business_Analytics_Data/pm_kmClust_relation.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/pmTsOv.RData")


# Create name vector for every parking meter in every cluster ------------
referenceDF = data.frame(DF_clustered_slim[!duplicated(DF_clustered_slim[,"SourceElementKey"]),][,c(1:2)])
ClusterList = as.numeric(sort(unique(referenceDF$cluster)))

# For Loop to get every Cluster and their corresponding ParkingMeters in sepreates dataframes
count = 0
for (a_clust in ClusterList ) {
  tempVec = as.character(filter(referenceDF,cluster == a_clust)[,-2])
  tempDF = OV_DF %>%
    select(datetime, tempVec)
  tempDF$hour = as.numeric(substr(tempDF$datetime, start = 12, stop = 13))
  tempDF = tempDF %>%
    filter(hour >= 8 & hour <= 17)
  tempDF$Weekday = weekdays(tempDF$datetime)
  RMindex = which(tempDF$Weekday == "Sonntag")
  tempDF = tempDF[-RMindex, -c(ncol(tempDF), (ncol(tempDF)-1))]
  tempDF = tempDF[, colMeans(is.na(tempDF)) < .20] 
  count = count + ncol(tempDF) # to see how many parking meters were dropped-> 179
  assign(paste0("Cluster",a_clust), tempDF )
}

# Imputing ----------

# Step 1: Make all values relative

for (i in seq(2,ncol(Cluster1),1)) {
  Cluster1_perc <<- Cluster1
  tempSEK = as.numeric(colnames(Cluster1_perc)[i])
  tempPMCnt = filter(DF_clustered_slim, SourceElementKey == tempSEK)[1,5]
  Cluster1_perc[,i] = Cluster1_perc[,i]/ tempPMCnt
}  

