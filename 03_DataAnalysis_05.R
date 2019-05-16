# Description ---------------

#### Mal checken: https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials #####

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
#load("../Schramm, Cornelius - 02_Business_Analytics_Data/clustTsOVimp.RData")
#df_gathered = gather(data=OV_DF_imp, key=SourceElementKey , value=FreeParkingSpaces, 2:ncol(OV_DF_imp))

### REAL with Features ### --------------------
load("../Schramm, Cornelius - 02_Business_Analytics_Data/FinalDFKmean.RData")

choice = 2
parking_filtered = FinalDFKmean %>%
  filter(cluster == choice)
parking_filtered = parking_filtered[,-17] # remove free percent
#---------------------------------------------
# Merge with clusters
#load("../Schramm, Cornelius - 02_Business_Analytics_Data/pm_kmClust_relation.RData")
#reference = DF_clustered_slim[,c(1:2,5)]
#reference = data.frame(reference[!duplicated(reference[,"SourceElementKey"]),][,])
#df_gathered = merge(df_gathered, reference, by="SourceElementKey")

# Aggregate by cluster
#tempDF = aggregate(list(df_gathered$FreeParkingSpaces,df_gathered$ParkingSpaceCount),
                #   by = list(cluster = df_gathered$cluster, 
                #             datetime = df_gathered$datetime),
                #   FUN = sum)
# Colnames
#colnames(tempDF)[3:4] = c("freeParkingSpaces", "ClusterCap")
### REAL ### 

# Choose cluster
#choice = 3

# Filter one parking meter
#parking_filtered = tempDF %>%
#  filter(cluster == choice)

# Plot
#ggplot(parking_filtered, aes(x=datetime, y=freeParkingSpaces)) +
#  geom_line() +
#  geom_hline(yintercept=parking_filtered[1,4])


# Training -------------------------------

# msts (2 seasonalities)
#ts_kmc_2 = msts(parking_filtered$freeParkingSpaces, seasonal.periods = c(10,10*6), 
          #      start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
          #      ts.frequency = 10*6*52)

# tbats model smoothing
#tbats = tbats(ts_kmc_2)
#plot(tbats, main="Multiple Season Decomposition")

# Predictions tbat
#sp = predict(tbats,h=10*6)
#plot(sp, main = "TBATS Forecast")

# Testing tbat model on REAL data with features ------------------------

# Splitting and creating msts train and test

ts_kmc = msts(parking_filtered$freeParkingSpaces, seasonal.periods = c(10,10*6), 
                    start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                    ts.frequency = 10*6*52)

# Predictions
tbats_2 = tbats(ts_kmc)

preds = predict(tbats_2, h=10*6)
plot(preds, main = "TBATS Forecast")


# Show decimal date as actual date
# print(format(date_decimal(2019.31), "%d-%m-%Y %H"))


# Stacking ----

# Test data
parking_filtered$preds = tbats_2$fitted.values

parking_filtered_train = parking_filtered[parking_filtered$datetime <= "2019-04-16",]
parking_filtered_test = parking_filtered[parking_filtered$datetime > "2019-04-16",]

# Create train data with preds from tbats

# Model building RF
  
model_rf = train(freeParkingSpaces ~ ., data=parking_filtered_train, method="rf")
tree_predict = predict(model_rf, newdata = parking_filtered_test)
plot(tree_predict ~ parking_filtered_test$freeParkingSpaces)

pred_stacked_rf = ts(tree_predict, start = decimal_date(as.POSIXct("2019-04-16 08:00:00")), frequency = 10*6*52)
preds_test_ts = ts(preds$mean, start = decimal_date(as.POSIXct("2019-04-16 08:00:00")), frequency = 10*6*52 )

plot(ts_kmc)
lines(preds_test_ts, col = "blue")
lines(pred_stacked_rf, col = "green")

# Model building GLM
model_glm = train(freeParkingSpaces ~ ., data=parking_filtered_train, method="glm")
glm_predict = predict(model_glm, newdata = parking_filtered_test)
plot(tree_predict ~ parking_filtered_test$freeParkingSpaces)

pred_stacked_glm = ts(glm_predict, start = decimal_date(as.POSIXct("2019-04-16 08:00:00")), frequency = 10*6*52)
plot(ts_kmc)
lines(preds_test_ts, col = "blue")
lines(pred_stacked_glm, col = "red")
lines(pred_stacked_rf, col = "green")

## Compare RMSE
RMSE_rf = RMSE(tree_predict, parking_filtered_test$freeParkingSpaces)
RMSE_glm = RMSE(glm_predict, parking_filtered_test$freeParkingSpaces)
RMSE_ts = RMSE(preds$mean, parking_filtered_test$freeParkingSpaces)


#------- LOOP -------

stop = max(as.numeric(FinalDFKmean$cluster))

Result_TS_RSME = vector("numeric", stop)
Result_GLM_RSME = vector("numeric", stop)
Result_RF_RSME = vector("numeric", stop)

i = 1

for (i in 1:stop) {
  
  choice = i
  parking_filtered = FinalDFKmean %>%
    filter(cluster == choice)
  parking_filtered = parking_filtered[,-17]  
  
  ts_kmc = msts(parking_filtered$freeParkingSpaces, seasonal.periods = c(10,10*6), 
                start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                ts.frequency = 10*6*52)
  
  # Predictions
  tbats_2 = tbats(ts_kmc)
  
  preds = predict(tbats_2, h=10*6)
  
  # Stacking ----
  
  # Test data
  parking_filtered$preds = tbats_2$fitted.values
  
  parking_filtered_train = parking_filtered[parking_filtered$datetime <= "2019-04-16",]
  parking_filtered_test = parking_filtered[parking_filtered$datetime > "2019-04-16",]
  
  # Model building RF
  
  model_rf = train(freeParkingSpaces ~ ., data=parking_filtered_train, method="rf", ntree = 100) # aus perfomancegründen ntree = 100
  tree_predict = predict(model_rf, newdata = parking_filtered_test)
  
  pred_stacked_rf = ts(tree_predict, start = decimal_date(as.POSIXct("2019-04-16 08:00:00")), frequency = 10*6*52)
  preds_test_ts = ts(preds$mean, start = decimal_date(as.POSIXct("2019-04-16 08:00:00")), frequency = 10*6*52 )
  
  # Model building GLM
  model_glm = train(freeParkingSpaces ~ ., data=parking_filtered_train, method="glm")
  glm_predict = predict(model_glm, newdata = parking_filtered_test)
  
  pred_stacked_glm = ts(glm_predict, start = decimal_date(as.POSIXct("2019-04-16 08:00:00")), frequency = 10*6*52)
  plot(ts_kmc, main = paste("Predicitions for Cluster ", i), xlab = "Date/Time in decimal", ylab = "Free parking spots")
  lines(preds_test_ts, col = "blue")
  lines(pred_stacked_glm, col = "red")
  lines(pred_stacked_rf, col = "green")
  legend("bottomleft", legend = c("Time Series Predicitions", "GLM Stacked Predicitions", "RF Stacked Predictions"), col = c("blue", "red", "green"),text.col = c("blue", "red", "green"), bty = "n", cex = 0.8)
  
  ## RMSE
  
  Result_TS_RSME[i] = RMSE(preds$mean, parking_filtered_test$freeParkingSpaces)
  Result_GLM_RSME[i] = RMSE(glm_predict, parking_filtered_test$freeParkingSpaces)
  Result_RF_RSME[i] = RMSE(tree_predict, parking_filtered_test$freeParkingSpaces)
  
  i = i+1
  
}

#----- Ende LOOP -----

# Total RSME über alle Cluster
sum(Result_TS_RSME)       
sum(Result_GLM_RSME)
sum(Result_RF_RSME)

# select Model for each cluster
results = as.data.frame(cbind(Result_TS_RSME, Result_GLM_RSME, Result_RF_RSME))
colnames(results)[1:3] = c("TS", "GLM", "RF")

best_model = vector("character", stop)
best_model = colnames(results)[apply(results,1,which.min)]
view(best_model)

# Save Plots (be carefull, all plots in session!)
plots.dir.path = list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths = list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="../Schramm, Cornelius - 02_Business_Analytics_Data/Graphs")
#corniisthässlich