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
load("../02_Business_Analytics_Data/df_set_02_merged.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")

# ... -----
