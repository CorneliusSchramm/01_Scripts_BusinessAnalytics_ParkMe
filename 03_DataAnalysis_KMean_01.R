# Description ---------------

# In this script
# -  Predict KMeans Cluster as Time Series without Features, per cluster



# Setup ----------------------------------------------
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)


rm(list=ls())
graphics.off()


load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_04_Sort4Clust_Clust.RData")

#only 8-17 hour, main hours (all obs. have data)
DF_KMC = DF_KMclust[DF_KMclust$hour < 18, ]

#choose cluster
DF_KMC_1 = DF_KMC[DF_KMC$cluster == 1,]

#Time series object creation
ts_kmc_1 = msts(DF_KMC_1, seasonal.periods = c(9,6), start = c(2019, 3, 25, 8), end = c(2019, 4, 22, 17))
