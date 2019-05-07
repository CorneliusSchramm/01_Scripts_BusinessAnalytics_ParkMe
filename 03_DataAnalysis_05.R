# Description ---------------

#### Mal checken: https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials #####

# Todo: Train Data für TBATS kann beide male 3 Wochen gross sein (gleiches modell), für das Train Data Set für RF muss dann letzte woche abgeschnitten werden (damit merge klappt, da variablen nur 3 monate)
# momentan: nimmt erst 2 wochen und predicted daraus 3, dann 3 wochen und daraus 4. (aber unnötig, da erste beide wochen bereits biased)

# In this script
# - we will forecast the parking density

# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)
library(ggplot2)
library(forecast)
library(tseries)
library(lubridate)
library(caret)


# Clear workspace
rm(list=ls())
graphics.off()

# ...
# set.seed(100)
theme_set(theme_minimal())

# Load the previousely saved version of our parking data as well as new data (weather, events)
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")
# dates = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/dates.csv")
# holidays = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/holidays.csv")
# weather_01 = read_csv2("../02_Business_Analytics_Data/weather.csv") #(25th March to 22nd April)
# weather_02 = read_csv2("../02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
# events = fread("../02_Business_Analytics_Data/events.csv")

# Because of OneDrive we need to load from two different paths
load("../02_Business_Analytics_Data/df_set_02_merged.RData")
# dates = fread("../02_Business_Analytics_Data/dates.csv")
# holidays = fread("../02_Business_Analytics_Data/holidays.csv")
# weather_01 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/weather.csv") #(25th March to 22nd April)
# weather_02 = read_csv2("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", skip =11)
# events = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/events.csv")

# Prepare data -----

# From large data
p_large_slim = DF_merged

####### Aggregated und mit absoluten Zahlen
rm(list=ls())
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_03_kmeanCluster.RData")

p_large_slim = DF_KMclust[,1:21]
colnames(p_large_slim)[1] = "SourceElementKey"
colnames(p_large_slim)[2] = "date.x"

#######

# Choose cluster
parkingmeter = 3

# Filter one parking meter
parking_filtered = p_large_slim %>%
  filter(SourceElementKey == parkingmeter)

# Merge date and time into one cell
parking_filtered$datetime = paste(parking_filtered$date.x, parking_filtered$hour)

# Right format
parking_filtered$datetime = as.POSIXct(parking_filtered$datetime, format="%Y-%m-%d %H")

# Order by date and time
parking_filtered = parking_filtered[order(parking_filtered$datetime),]
# Oder ohne die datetime spalte zu kreieren

rownames(parking_filtered) = NULL

# Training ----

# msts (2 seasonalities)
ts_kmc_2 = msts(parking_filtered$FreeSpots, seasonal.periods = c(12,12*6), 
                start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                ts.frequency = 12*6*52)

# tbats model smoothing
tbats = tbats(ts_kmc_2)
plot(tbats, main="Multiple Season Decomposition")

tbats.components(tbats)
# predicttions tbat
sp = predict(tbats,h=12*6)
plot(sp, main = "TBATS Forecast")

# testing tbat model on real data ------------------------

##splitting and creating msts train and test
parking_filtered_train = parking_filtered[parking_filtered$datetime <= "2019-04-09",]
parking_filtered_test = parking_filtered[parking_filtered$datetime > "2019-04-09" & parking_filtered$datetime <= "2019-04-16",]
ts_kmc_train = msts(parking_filtered_train$FreeSpots, seasonal.periods = c(12,12*6), 
                    start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                    ts.frequency = 12*6*52)
ts_kmc_test = msts(parking_filtered_test$FreeSpots, seasonal.periods = c(12,12*6), 
                   start = decimal_date(as.POSIXct("2019-04-09 08:00:00")),
                   ts.frequency = 12*6*52)

## preds
tbats_2 = tbats(ts_kmc_train)

preds = predict(tbats_2, h=12*6)
plot(preds, main = "TBATS Forecast")
lines(ts_kmc_test)


#show decimal date as actual date
print(format(date_decimal(2019.31), "%d-%m-%Y %H"))

#stacking

#test data
parking_filtered_train_2 = parking_filtered[parking_filtered$datetime <= "2019-04-16",]
parking_filtered_test_2 = parking_filtered[parking_filtered$datetime > "2019-04-16",]
ts_kmc_train_2 = msts(parking_filtered_train_2$FreeSpots, seasonal.periods = c(12,12*6), 
                   start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                   ts.frequency = 12*6*52)
tbats_test = tbats(ts_kmc_train_2)

preds_test = predict(tbats_test, h=12*6)

parking_filtered_test_2$preds = preds_test$mean

#create train data with preds from tbats
parking_filtered_stacked = parking_filtered_test
parking_filtered_stacked$preds = preds$mean

#model building
  
model = train(FreeSpots ~ ., data=parking_filtered_stacked, method="rf")
tree_predict = predict(model, newdata = parking_filtered_test_2)
plot(tree_predict ~ parking_filtered_test_2$FreeSpots)

pred_stacked = ts(tree_predict, start = decimal_date(as.POSIXct("2019-04-16 08:00:00")), frequency = 12*6*52)
preds_test_ts = ts(preds_test$mean, start = decimal_date(as.POSIXct("2019-04-16 08:00:00")), frequency = 12*6*52 )
preds_ts = ts(preds$mean, start = decimal_date(as.POSIXct("2019-04-09 08:00:00")), frequency = 12*6*52 )

plot(ts_kmc_2)
lines(preds_test_ts, col = "blue")
lines(pred_stacked, col = "red")
lines(preds_ts, col = "green3")
