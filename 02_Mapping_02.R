# Description ---------------

# In this script
# - plot the clustered parking meteres

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

# Load the previousely saved version of our parking data as well as new data (weather, events)
load("../02_Business_Analytics_Data/df_set_03_tempCluster.RData")
# Merge date and time into one cell
tempDF2$datetime = paste(tempDF2$date, tempDF2$time)
tempDF2 = tempDF2 %>%
  select(datetime, everything())

# Right format
tempDF2$datetime = as.POSIXct(tempDF2$datetime, format="%Y-%m-%d %H:%M")

# Order by date and time
tempDF2 = tempDF2[order(tempDF2$datetime),]
# Oder ohne die datetime spalte zu kreieren
# parking_filtered = parking_filtered[order(parking_filtered$date, parking_filtered$time),]
# Reset index
rownames(tempDF2) = NULL



# taking one cluster
example_cluster = 2
data_plot = tempDF2 %>%
  filter (tempDF2$cluster == example_cluster) 

ggplot(data_plot) +
  geom_line(aes(x=datetime, y=x))

# finding ot wether dip is due to event
which.min(data_plot$x)
data_plot$x[9403]
data_plot$date[9403]
