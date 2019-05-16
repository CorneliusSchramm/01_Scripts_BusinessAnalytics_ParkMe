# Description ---------------

# In this script
# - we finalize cleaning
# - output will be the final dataframe for analysis

# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)
library(ggplot2)



# Clear workspace
rm(list=ls())
graphics.off()

load("../02_Business_Analytics_Data/FinalDFKmean.RData")
load("../02_Business_Analytics_Data/shinyPredsDF.RData")
load("../02_Business_Analytics_Data/df_set_02_merged.RData")
load("../02_Business_Analytics_Data/pm_kmClust_relation.RData")

# Data_plot (locations modified)
# SEK  lon   lat    freePerc
                  # FreeSpots von Cluster / PSC CLuster

start = as.POSIXct("2019-04-22 08:00:00") # FOR ONE WEEK !!!!
end = as.POSIXct("2019-04-27 21:00:00 ")
datetime = as.data.table(seq(from = start,
                            by = "hour",
                            to = end))
colnames(datetime) = "datetime"

datetime = separate(datetime,"datetime", c(NA, "hour"), sep=" ", remove = F)
datetime = separate(datetime,"hour", c("hour",NA), sep=":")

datetime$hour = as.numeric(datetime$hour)
datetime = datetime %>%
  filter(hour<=17 & hour>= 8)


# Clusterpreds   an datetime                                                                                  // datetime
# Wir brauchen die predictins aus dem dataanalysis file für alle 30 cluster für die ganze Woche  // clustpred (abs) datetime cluster PSCClust
# Reference DF zum zuordnen von SEK und cluster   // cluster sek psc lon lat
# dann mergen der beiden obigen über cluster mergen // datetime SEK psc lon lat cluster clustpred (abs) PSCCluster
# dann free percent pro cluster = pro SEK schaffen  // datetime SEK psc lon lat cluster clustpred (abs) PSCCluster FreePercClust FreeSpotsSEK



shinyPredsDF = as.data.frame(shinyPredsDF[,-2])
shinyPredsDF[,2:30] = shinyPredsDF$x
colnames(shinyPredsDF)= c(seq(1,30,1))
preds = as.data.frame(cbind(datetime,shinyPredsDF))
preds = gather(preds, key= cluster, value = FreeSpotsCluster, 3:32)


#reference DF
referenceDF = data.frame(DF_clustered_slim[!duplicated(DF_clustered_slim[,"SourceElementKey"]),][,c(1:2,5)])

# Getting lon lat
locations = data.frame(DF_merged[!duplicated(DF_merged[,"SourceElementKey"]),][,c(1:3)])

referenceDF = referenceDF %>%
  left_join(locations, by = "SourceElementKey")

# 
preds = merge(preds, referenceDF, by = "cluster", all.x = T, all.y = F)

load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")
load("../02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")
clustcap = DF_KMclust[,c(1,5)]

merge(preds, clustcap, by="cluster")