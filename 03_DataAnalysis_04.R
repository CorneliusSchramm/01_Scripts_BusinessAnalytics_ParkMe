# Description ---------------

#### Mal checken: https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials #####

# In this script
# - we will forecast the parking density
# - of one parking meter
# - without features

# Setup ----------------------------------------------

# Load required packages
# library(data.table)
# library(tidyverse)
# library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
# library(SpatioTemporal)
# library(plotrix)

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
p_large_slim = DF_merged_large_v1[,c(2,3,7,19,4)]

# Choose large or small dataset to proceed
# parking_orig = DF_final_small[,c(5,7,1,23)]
# parking_orig = p_large_slim[,c(1:4)]
# Mit absoluten Zahlen
parking_orig = p_large_slim[,c(1:3,5)]
colnames(parking_orig)[4] = "freePercent" # damit ich nicht den ganzen code ändern muss unten



####### Aggregated und mit absoluten Zahlen
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_03_tempCluster.RData")
library(data.table)
#######

# Choose parking meter
parkingmeter = 36161 # (der mit den meisten Parkplätzen)

# Filter one parking meter
parking_filtered = parking_orig %>%
  filter(SourceElementKey == parkingmeter)

# Merge date and time into one cell
parking_filtered$datetime = paste(parking_filtered$date, parking_filtered$time)
parking_filtered = parking_filtered %>%
  select(datetime, date.x, freePercent)
# Right format
parking_filtered$datetime = as.POSIXct(parking_filtered$datetime, format="%Y-%m-%d %H:%M")

# Order by date and time
parking_filtered = parking_filtered[order(parking_filtered$datetime),]
# Oder ohne die datetime spalte zu kreieren
# parking_filtered = parking_filtered[order(parking_filtered$date, parking_filtered$time),]
# Reset index
rownames(parking_filtered) = NULL


# Analysis -----

# Now starting to copy from website mentioned above -----

# TS-Format and first plot ---- 

ggplot(parking_filtered, aes(datetime, freePercent)) + 
  geom_line() + 
  ylab("Percentage of Free Parking Spaces") +
  xlab("")
  # + geom_vline(xintercept = 2019-03-25 08:00:00, col="red")

fPct_ts = ts(parking_filtered[, c('freePercent')])

parking_filtered$cleanPercentage = tsclean(fPct_ts)
unique(parking_filtered$cleanPercentage == parking_filtered$freePercent)
# No outliers found all the same data as before. Drop column and save shorter name
pf = parking_filtered[,c(1:3)]

# Moving average ----

pf$fPct_ma_h = ma(pf$freePercent, order=60)
pf$fPct_ma_d = ma(pf$freePercent, order=1440)

ggplot() +
  geom_line(data = pf, aes(x = datetime, y = freePercent, colour = "Counts")) +
  geom_line(data = pf, aes(x = datetime, y = fPct_ma_h,   colour = "Hourly Moving Average"))  +
  geom_line(data = pf, aes(x = datetime, y = fPct_ma_d, colour = "Daily Moving Average"))  +
  ylab("Percentage of Free Parking Spaces")

# wtf does this tell me

# Step 3: Decompose Your Data

fPct_ma = ts(na.omit(pf$fPct_ma_h), frequency=1440)
decomp = stl(fPct_ma, s.window="periodic")
deseasonal_fPct <- seasadj(decomp)
plot(decomp)

# Step 4: Stationarity

adf.test(fPct_ma, alternative = "stationary")


# Step 5: Autocorrelations and Choosing Model Order

Acf(fPct_ma, main='')
Pacf(fPct_ma, main='')

# Skip rest of step 5
# ...
# ...
# ...

# Step 6: Fitting an ARIMA model

auto.arima(deseasonal_fPct, seasonal=FALSE)

# Step 7: Evaluate and Iterate

fit<-auto.arima(deseasonal_fPct, seasonal=FALSE)
tsdisplay(residuals(fit),
          lag.max=1440,
          main='(1,1,1) Model Residuals') # Was für ne Nummer muss da rein?

# Wenn man da jetzt was erkennen würde könnte man mit en Zahlen in der Klammer spielen
# Ich erkenne aber nichts
fcast <- forecast(fit, h=60)
plot(fcast, 
     xlim=range(6:7))

# Mit Parametern spielen

pred_length = 1440
fit2 = arima(ts(deseasonal_fPct[-c((15300-pred_length):15300)]),
             order=c(1,1,1))
tsdisplay(residuals(fit2), 
          lag.max=240, 
          main='(1,1,1) Model Residuals')

fcast2 <- forecast(fit2, h=pred_length)
plot(fcast2)
lines(ts(deseasonal_fPct))
abline(v=seq(0,10000,1440), col="red") # Täglich
abline(v=seq(0,1439,60), col="green")  # Stündlich


# With seasonality
pred_length = 1440
fit_w_seasonality = auto.arima(ts(deseasonal_fPct[-c((15300-pred_length):15300)]), 
                               seasonal=TRUE)
seas_fcast <- forecast(fit_w_seasonality, h=pred_length)
plot(seas_fcast,
     xlim=range(12000:15300))
lines(ts(deseasonal_fPct))


graphics.off()















# Plot parking density over one day
# Choose date
example_date1 = "2019-03-25"

data_plot = parking_filtered %>%
  filter(parking_filtered$date.x == example_date1)

ggplot(data_plot) +
  geom_line(aes(x=datetime, y=freePercent))

# Plot parking density over two days
# Choose dates
example_date2 = "2019-03-30"
data_plot = parking_filtered %>%
  filter(date.x >= example_date1 & date.x <= example_date2)

ggplot(data_plot) +
  geom_line(aes(x=datetime, y=freePercent))

# Separate into training and testing data
train_start = "2019-03-25"
train_end = "2019-03-30"
test_start = "2019-04-01"
test_end = "2019-04-01"



data_train = parking_filtered %>%
  filter(date.x >= train_start & date.x <= train_end)
data_train = data_train[,-2]

data_test = parking_filtered %>%
  filter(date.x >= test_start & date.x <= test_end)
data_test = data_test[,-2]
# Forecast model via "forecast" package

ts = ts(data = data_train, frequency = 600)
msts = msts(data_train, seasonal.periods=c(6, 600))
cycle(msts)

plot.ts(msts)

model = auto.arima(msts[,2], seasonal = T)

#Play with SpatioTemporal Package

obs = data.frame(paste(DF_final_small$date.x, DF_final_small$time, sep = " "), paste(DF_final_small$lon , DF_final_small$lat, sep = " "), DF_final_small$freePercent)
colnames(obs) = c("date", "ID", "obs")

createSTdata(obs)

model_st_1 = create.data.matrix(obs = DF_final_small$freePercent, date = DF_final_small$date.x , ID = c(DF_final_small$lon, DF_final_small$lat), subset = NULL)

createSTdata(DF_final_small)

# K Means
locations = data.frame(DF_final_small[!duplicated(DF_final_small[,c("SourceElementKey","lon","lat")]),][,c(1,2,3)])

KMean = kmeans(locations[,2:3], 10)
locations$cluster = KMean$cluster

ggplot(data = locations, )

