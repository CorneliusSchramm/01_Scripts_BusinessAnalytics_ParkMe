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
DF_KMC = DF_KMclust[DF_KMclust$hour < 22, ]

#choose cluster
DF_KMC_1 = DF_KMC[DF_KMC$cluster == 3,]

#features raus
DF_KMC_1 = DF_KMC_1[,2:4]

#Time series object creation
#msts object creation

DF_KMC_1$datetime = paste(DF_KMC_1$date, DF_KMC_1$hour)

DF_KMC_1$datetime = as.POSIXct(DF_KMC_1$datetime, format="%Y-%m-%d %H")

ts_kmc = msts(DF_KMC_1$FreeSpots, seasonal.periods = c(12,12*6), 
                start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                ts.frequency = 12*6*52)

#tbats model smoothing
tbats = tbats(ts_kmc)
plot(tbats, main="Multiple Season Decomposition")


# predicttions tbat
sp = predict(tbats,h=12*6)
plot(sp, main = "TBATS Forecast")

# testing tbat model on real data

##splitting and creating msts train and test
parking_filtered_train = parking_filtered[parking_filtered$datetime <= "2019-04-16",]
parking_filtered_test = parking_filtered[parking_filtered$datetime > "2019-04-16",]
ts_kmc_train = msts(parking_filtered_train$FreeSpots, seasonal.periods = c(10,10*6), 
                    start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                    ts.frequency = 10*6*52)
ts_kmc_test = msts(parking_filtered_test$FreeSpots, seasonal.periods = c(10,10*6), 
                   start = decimal_date(as.POSIXct("2019-04-15 08:00:00")),
                   ts.frequency = 10*6*52)

## preds
tbats_2 = tbats(ts_kmc_train)

preds = predict(tbats_2, h=10*6)
plot(preds, main = "TBATS Forecast")
lines(ts_kmc_test)


