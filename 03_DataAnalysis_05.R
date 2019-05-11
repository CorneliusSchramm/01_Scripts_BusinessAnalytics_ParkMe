# Description ---------------

#### Mal checken: https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials #####

# Todo: Train Data f?r TBATS kann beide male 3 Wochen gross sein (gleiches modell), f?r das Train Data Set f?r RF muss dann letzte woche abgeschnitten werden (damit merge klappt, da variablen nur 3 monate)
# momentan: nimmt erst 2 wochen und predicted daraus 3, dann 3 wochen und daraus 4. (aber unn?tig, da erste beide wochen bereits biased)

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
theme_set(theme_minimal())


### REAL ### ----------------------------------
load("../Schramm, Cornelius - 02_Business_Analytics_Data/clustTsOVimp.RData")
df_gathered = gather(data=OV_DF_imp, key=SourceElementKey , value=FreeParkingSpaces, 2:ncol(OV_DF_imp))

# Merge with clusters
load("../Schramm, Cornelius - 02_Business_Analytics_Data/pm_kmClust_relation.RData")
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

# Choose cluster
choice = 3

# Filter one parking meter
parking_filtered = tempDF %>%
  filter(cluster == choice)

# Plot
ggplot(parking_filtered, aes(x=datetime, y=freeParkingSpaces)) +
  geom_line() +
  geom_hline(yintercept=parking_filtered[1,4])


# Training ----------

# msts (2 seasonalities)
ts_kmc_2 = msts(parking_filtered$freeParkingSpaces, seasonal.periods = c(10,10*6), 
                start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                ts.frequency = 10*6*52)

# tbats model smoothing
tbats = tbats(ts_kmc_2)
plot(tbats, main="Multiple Season Decomposition")

tbats.components(tbats)
# Predictions tbat
sp = predict(tbats,h=10*6)
plot(sp, main = "TBATS Forecast")

# Testing tbat model on real data ------------------------

# Splitting and creating msts train and test
parking_filtered_train = parking_filtered[parking_filtered$datetime <= "2019-04-09",]
parking_filtered_test = parking_filtered[parking_filtered$datetime > "2019-04-09" & parking_filtered$datetime <= "2019-04-16",]
ts_kmc_train = msts(parking_filtered_train$freeParkingSpaces, seasonal.periods = c(10,10*6), 
                    start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                    ts.frequency = 10*6*52)
ts_kmc_test = msts(parking_filtered_test$freeParkingSpaces, seasonal.periods = c(10,10*6), 
                   start = decimal_date(as.POSIXct("2019-04-09 08:00:00")),
                   ts.frequency = 10*6*52)

# Predictions
tbats_2 = tbats(ts_kmc_train)

preds = predict(tbats_2, h=10*6)
plot(preds, main = "TBATS Forecast")
lines(ts_kmc_test)


# Show decimal date as actual date
# print(format(date_decimal(2019.31), "%d-%m-%Y %H"))


# Stacking ----

# Test data
parking_filtered_train_2 = parking_filtered[parking_filtered$datetime <= "2019-04-16",]
parking_filtered_test_2 = parking_filtered[parking_filtered$datetime > "2019-04-16",]
ts_kmc_train_2 = msts(parking_filtered_train_2$FreeSpots, seasonal.periods = c(12,12*6), 
                   start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                   ts.frequency = 12*6*52)
tbats_test = tbats(ts_kmc_train_2)

preds_test = predict(tbats_test, h=12*6)

parking_filtered_test_2$preds = preds_test$mean

# Create train data with preds from tbats
parking_filtered_stacked = parking_filtered_test
parking_filtered_stacked$preds = preds$mean

# Model building
  
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
