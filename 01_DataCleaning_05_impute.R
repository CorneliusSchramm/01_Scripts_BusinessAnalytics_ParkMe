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

# Register Google Key
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")

# Load the previousely saved merged version of our parking data
load("../Schramm, Cornelius - 02_Business_Analytics_Data/pm_kmClust_relation.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/pmTsOv.RData")


#
# Create name vector for every parking meter in every cluster
parking_filtered = DF_clustered_slim %>%
  filter(cluster == 1)
names = unique(parking_filtered$SourceElementKey)






