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

# ...
theme_set(theme_minimal())

# Load Data
load("../Schramm, Cornelius - 02_Business_Analytics_Data/clustTsOVimp.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/pm_kmClust_relation.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")  


# OneDrive
load("../02_Business_Analytics_Data/clustTsOVimp.RData")
load("../02_Business_Analytics_Data/pm_kmClust_relation.RData")
load("../02_Business_Analytics_Data/df_set_02_merged.RData")  


# Clean ------

df_gathered = gather(data=OV_DF_imp, key=SourceElementKey , value=FreeParkingSpaces, 2:ncol(OV_DF_imp))

# Merge with clusters
reference = DF_clustered_slim[,c(1:2,5)]
reference = data.frame(reference[!duplicated(reference[,"SourceElementKey"]),][,])
df_gathered = merge(df_gathered, reference, by="SourceElementKey")

load("../Schramm, Cornelius - 02_Business_Analytics_Data/locations.RData")

## DER DF IST GUT
tempDF = data.frame(df_gathered[!duplicated(df_gathered[,"SourceElementKey"]),][,])
aggregated_df = aggregate(list(tempDF$ParkingSpaceCount, tempDF$FreeParkingSpaces),
                          by = list(cluster = tempDF$cluster),
                          FUN = sum)
colnames(aggregated_df)[2] = c("clustCap")
aggregated_df = aggregated_df[,-3]

# Save not-aggregated df
save(df_gathered, file="../Schramm, Cornelius - 02_Business_Analytics_Data/impSEK.RData")
save(aggregated_df, file="../Schramm, Cornelius - 02_Business_Analytics_Data/clustCap.RData")


locations = merge(locations, aggregated_df, by="cluster", all=T)
save(locations, file="../Schramm, Cornelius - 02_Business_Analytics_Data/locationsToPlot.RData")

# Aggregate by cluster
tempDF = aggregate(list(df_gathered$FreeParkingSpaces,df_gathered$ParkingSpaceCount),
                   by = list(cluster = df_gathered$cluster, 
                             datetime = df_gathered$datetime),
                   FUN = sum)
# Colnames
colnames(tempDF)[3:4] = c("freeParkingSpaces", "ClusterCap")


# Getting Weather Data (again) -----------
DF_merged$datetime = paste(DF_merged$date, DF_merged$hour)
DF_merged = DF_merged %>%
  select(datetime, c(11:22))
DF_merged$datetime = as.POSIXct(DF_merged$datetime,format= "%Y-%m-%d %H")
DF_merged = data.frame(DF_merged[!duplicated(DF_merged[,"datetime"]),][,])

# Merging
FinalDFKmean = merge(tempDF,DF_merged, by= "datetime")
FinalDFKmean$FreeClustPerc = FinalDFKmean$freeParkingSpaces/ FinalDFKmean$ClusterCap

# Remove unnecessary dataframes
rm(list=setdiff(ls(), "FinalDFKmean"))

# save.image(file = "../Schramm, Cornelius - 02_Business_Analytics_Data/FinalDFKmean.RData")



