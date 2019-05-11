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


# Clean ------

df_gathered = gather(data=OV_DF_imp, key=SourceElementKey , value=FreeParkingSpaces, 2:ncol(OV_DF_imp))

# Merge with clusters
reference = DF_clustered_slim[,c(1:2,5)]
reference = data.frame(reference[!duplicated(reference[,"SourceElementKey"]),][,])
df_gathered = merge(df_gathered, reference, by="SourceElementKey")

# Aggregate by cluster
tempDF = aggregate(list(df_gathered$FreeParkingSpaces,df_gathered$ParkingSpaceCount),
                   by = list(cluster = df_gathered$cluster, 
                             datetime = df_gathered$datetime),
                   FUN = sum)
# Colnames
colnames(tempDF)[3:4] = c("freeParkingSpaces", "ClusterCap")
### REAL ### ----------------------------------
