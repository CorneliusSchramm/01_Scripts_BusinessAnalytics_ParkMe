# Description ---------------

# In this script
# - cluster the time series based on similarities

# Setup ----------------------------------------------

# Load required packages

library(tidyverse)
library(data.table)
library(quantmod)
library(dtwclust)
library(ggplot2)
library(dtw)
# Clear workspace
rm(list=ls())
graphics.off()

#Load Data
load("../02_Business_Analytics_Data/df_set_04_Sort4Clust.RData")

#Because of Onedrive
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_04_Sort4Clust.RData")

#Standardizing all observations
datetime = finalDF[,1]
ScaledDF = scale(finalDF[,-1])
FinalScaled  = cbind(datetime,ScaledDF)

# Compartmentalizing by start and end dates-----------
#This part still needs some work
test = head(FinalScaled)
NAind = (colnames(test)[colSums(is.na(test)) > 0])

!names(FinalScaled) %in% NAind

DF_compart1 = subset(FinalScaled, select= !names(FinalScaled) %in% NAind)
DF_compart2 = subset(FinalScaled, select=c(1,names(FinalScaled) %in% NAind))

#Converting to time series-----------------
tsDF = as.ts(FinalScaled)
SpielDF = tsDF[c(1:720),c(1,426:435)]
SpielDF = SpielDF[,c(1:8)]
plot.ts(SpielDF[,2:11], 
        type = "b",
        col = "blue")


# Calculating distance
distance = dist (SpielDF, method = "DTW") 

#Hirachical Clustering
hc = hclust(distance, method = "average")
plot(hc)



