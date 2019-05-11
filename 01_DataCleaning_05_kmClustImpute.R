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


#  -----

# Create name vector for every parking meter in every cluster 
referenceDF = data.frame(DF_clustered_slim[!duplicated(DF_clustered_slim[,"SourceElementKey"]),][,c(1:2)])
ClusterList = as.numeric(sort(unique(referenceDF$cluster)))
OV_DF_imp = data.frame(c(1:250))
colnames(OV_DF_imp) = "datetime"
dropped_pm = 0

# Negative values (!!!!!)
sum(OV_DF < 0 & !is.na(OV_DF))

# For Loop to ...
for (a_clust in ClusterList ) {
  # Print Cluster name
  print(paste0("Cluster", a_clust, ":"))
  # get every Cluster and their corresponding ParkingMeters in sepreates dataframes
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
  dropped_pm = dropped_pm + ncol(tempDF) # to see how many parking meters were dropped-> 179
  
  # Step 1: Make all values relative
  for (i in 2:ncol(tempDF)) {
    tempSEK = as.numeric(colnames(tempDF)[i])
    tempPMCnt = filter(DF_clustered_slim, SourceElementKey == tempSEK)[1,5]
    tempDF[,i] = tempDF[,i]/tempPMCnt
  }  
  print(paste("NA's before Imputation:",sum(is.na(tempDF))))
  
  # Step 2: Imute empty values with the mean of all values in same timespot and cluster
  for (row in 1:nrow(tempDF)) {
    rowMean = rowMeans(tempDF[row,c(2:ncol(tempDF))], na.rm = T)
    cols = which(is.na(tempDF[row,]))
    tempDF[row, cols] = rowMean
  }
  print(paste("NA's after Imputation:",sum(is.na(tempDF))))
  
  # Step 3: Make all values absolute
  for (i in 2:ncol(tempDF)) {
    tempSEK = as.numeric(colnames(tempDF)[i])
    tempPMCnt = filter(DF_clustered_slim, SourceElementKey == tempSEK)[1,5]
    tempDF[,i] = tempDF[,i]*tempPMCnt
  }
  
  # Use cbind to save df to wide overview
  OV_DF_imp = cbind(OV_DF_imp, tempDF[,c(2:ncol(tempDF))])
  
  # Save single cluster
  assign(paste0("Cluster",a_clust), tempDF )
  }

# Check results
sum(is.na(OV_DF_imp))
OV_DF_imp[,1] = Cluster1[,1]


# Save -----

# Remove unnecessary dataframes
rm(list=setdiff(ls(), "OV_DF_imp"))

# save.image(file = "../02_Business_Analytics_Data/clustTsOVimp.RData")
# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/clustTsOVimp.RData")


