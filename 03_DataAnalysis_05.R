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
library(car)


# Clear workspace
rm(list=ls())
graphics.off()

# ...
theme_set(theme_minimal())


### REAL ### ----------------------------------
load("../Schramm, Cornelius - 02_Business_Analytics_Data/FinalDFKmean.RData")

# Merge with clusters
load("../02_Business_Analytics_Data/FinalDFKmean.RData")


# Choose cluster
choice = 17

# Filter one parking meter
parking_filtered = FinalDFKmean %>%
  filter(cluster == choice)

# Plot
# ggplot(parking_filtered, aes(x=datetime, y=freeParkingSpaces)) +
#   geom_line() +
#   geom_hline(yintercept=parking_filtered[1,4])


# Training ----------

# msts (2 seasonalities)
ts_kmc_2 = msts(parking_filtered$freeParkingSpaces, seasonal.periods = c(10,10*6), 
                start = decimal_date(as.POSIXct("2019-03-25 08:00:00")),
                ts.frequency = 10*6*52)

# tbats model smoothing
tbats = tbats(ts_kmc_2)
#plot(tbats, main="Multiple Season Decomposition")

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

# Why mean-> not sure but dont care (maybe care a little)
shinyPredsDF = as.data.frame(preds$mean)

# Create empty time Series to merge to shinyPredsDF
datetime = FinalDFKmean[FinalDFKmean$datetime >= "2019-04-09 08:00:00",]
datetime = datetime[datetime$cluster == 17,1]  
datetime = datetime[c(1:60)]
# Cbind
shinyPredsDF = cbind(shinyPredsDF,datetime)

# Saving Predictions
#save.image(file = "../02_Business_Analytics_Data/shinyPredsDF.RData")




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

parking_filtered = FinalDFKmean %>%
  filter(cluster == 1)
parking_filtered_train = parking_filtered[parking_filtered$datetime <= "2019-04-16",]
parking_filtered_test = parking_filtered[parking_filtered$datetime > "2019-04-16",]
DF_GLM = as.data.frame(parking_filtered_test$datetime)
DF_RF = as.data.frame(parking_filtered_test$datetime)
DF_TS = as.data.frame(parking_filtered_test$datetime)

colnames(DF_GLM)[1] = "datetime"
colnames(DF_RF)[1] = "datetime"
colnames(DF_TS)[1] = "datetime"

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
  
  model_rf = train(freeParkingSpaces ~ ., data=parking_filtered_train, method="rf", ntree = 100) # aus perfomancegr�nden ntree = 100
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
  
  #Build DF with results
  
  DF_TS = cbind(DF_TS, as.vector(preds$mean))
  colnames(DF_TS)[i+1] = i
  
  DF_GLM = cbind(DF_GLM, glm_predict)
  colnames(DF_GLM)[i+1] = i
  
  DF_RF = cbind(DF_RF, tree_predict)
  colnames(DF_RF)[i+1] = i
  
  ## RMSE
  
  Result_TS_RSME[i] = RMSE(preds$mean, parking_filtered_test$freeParkingSpaces)
  Result_GLM_RSME[i] = RMSE(glm_predict, parking_filtered_test$freeParkingSpaces)
  Result_RF_RSME[i] = RMSE(tree_predict, parking_filtered_test$freeParkingSpaces)
  
  i = i+1
  
}

#----- Ende LOOP -----

# Total RSME �ber alle Cluster
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


## Save Data Frames
save(DF_GLM,file = "../Schramm, Cornelius - 02_Business_Analytics_Data/results_glm.RData")
save(DF_RF,file = "../Schramm, Cornelius - 02_Business_Analytics_Data/results_rf.RData")
save(DF_TS,file = "../Schramm, Cornelius - 02_Business_Analytics_Data/results_ts.RData")
save(best_model, file = "../Schramm, Cornelius - 02_Business_Analytics_Data/best_model.RData")


# auto.arima TEST ---------------

# Set up harmonic regressors
fourier_train = fourier(ts_kmc_train, K = c(2,4))
fourier_test = fourier(ts_kmc_test, K = c(2,4))

data_train = as.matrix(parking_filtered_train[c(5,7,9,12,13,14,15)])
data_test = as.matrix(parking_filtered_test[c(5,7,9,12,13,14,15)])
# Fit regression model with ARIMA errors

model_arima = auto.arima(ts_kmc_train, xreg = cbind(fourier_train,data_train), seasonal = T)

pred_arima = forecast(model_arima, xreg=cbind(fourier_test,data_test))
plot(ts_kmc)
lines(pred_arima$mean, col = "red")
