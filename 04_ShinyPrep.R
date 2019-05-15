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
load("../02_Business_Analytics_Data/df_set_02")

# Data_plot (locations modified)
# SEK  lon   lat    freePerc
                  # FreeSpots von Cluster / PSC CLuster


# Getting Freeperc of Cluster
tempDF = FinalDFKmean %>%
  filter(cluster == 17 & datetime >= "2019-04-09 08:00:00" & datetime <= "2019-04-15 17:00:00") 

Data_plot = merge(tempDF,shinyPredsDF)
Data_plot$FreePercClust = Data_plot$x/ Data_plot$ClusterCap



# PredictionsDF (Value pro Cluster)
# Date  time Cluster FreeSpots

