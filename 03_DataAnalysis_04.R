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
p_large_slim = DF_merged[,c(1,6,7,24)]

####### Aggregated und mit absoluten Zahlen
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_03_rasterCluster.RData")
library(data.table)
tempDF2 = DF_Rastclust

p_large_slim = tempDF2[, c(1:4)]
colnames(p_large_slim)[1] = "SourceElementKey"
colnames(p_large_slim)[2] = "date.x"

#######

# Choose parking meter
parkingmeter = 3

# Filter one parking meter
parking_filtered = p_large_slim %>%
  filter(SourceElementKey == parkingmeter)

# Merge date and time into one cell
parking_filtered$datetime = paste(parking_filtered$date.x, parking_filtered$hour)
parking_filtered = parking_filtered %>%
  select(datetime, date.x, FreeSpots)
# Right format
parking_filtered$datetime = as.POSIXct(parking_filtered$datetime, format="%Y-%m-%d %H")

# Order by date and time
parking_filtered = parking_filtered[order(parking_filtered$datetime),]
# Oder ohne die datetime spalte zu kreieren
# parking_filtered = parking_filtered[order(parking_filtered$date, parking_filtered$time),]
# Reset index
rownames(parking_filtered) = NULL


# Analysis -----

# Now starting to copy from website mentioned above -----

# TS-Format and first plot ---- 

ggplot(parking_filtered, aes(as.numeric(datetime), FreeSpots)) + 
  geom_line() + 
  ylab("Free Parking Spaces") +
  xlab("") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-03-28")), color="red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-03-29")), color="red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-03-30")), color="red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-03-31")), color="red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-04-01")), color="red") +
  geom_vline(xintercept=as.numeric(as.POSIXct("2019-04-02")), color="red") +
  xlim(1553750000,1554000000)

ts = ts(parking_filtered[, c('x')])

parking_filtered$cleanX = tsclean(ts)
unique(parking_filtered$cleanX == parking_filtered$x)
# No outliers found all the same data as before. Drop column and save shorter name
pf = parking_filtered[,c(1:3)]

# Moving average ----

pf$ma_d = ma(pf$x, order=24)
pf$ma_w = ma(pf$x, order=168)

ggplot() +
  geom_line(data = pf, aes(x = datetime, y = x, colour = "Counts")) +
  geom_line(data = pf, aes(x = datetime, y = ma_d,   colour = "Daily Moving Average"))  +
  geom_line(data = pf, aes(x = datetime, y = ma_w, colour = "Weekly Moving Average"))  +
  ylab("Percentage of Free Parking Spaces")

# wtf does this tell me

# Step 3: Decompose Your Data

ma = ts(na.omit(pf$ma_d), frequency=168)
decomp = stl(ma, s.window=24)
deseasonal_fPct <- seasadj(decomp)
plot(decomp)

# Step 4: Stationarity

adf.test(ma, alternative = "stationary")


# Step 5: Autocorrelations and Choosing Model Order

Acf(ma, main='')
Pacf(ma, main='')

# Skip rest of step 5
# ...
# ...
# ...

# Step 6: Fitting an ARIMA model

auto.arima(ma, seasonal=FALSE)

# Step 7: Evaluate and Iterate

fit = auto.arima(ma, seasonal=FALSE)
tsdisplay(residuals(fit),
          lag.max=168,
          main='(1,1,1) Model Residuals') # Was für ne Nummer muss da rein?

# Wenn man da jetzt was erkennen würde könnte man mit en Zahlen in der Klammer spielen
# Ich erkenne aber nichts
fcast <- forecast(fit, h=24)
plot(fcast)

# Mit Parametern spielen

pred_length = 24
fit2 = arima(ts(ma[-c((276-pred_length):276)]),
             order=c(1,1,1))
tsdisplay(residuals(fit2), 
          lag.max=24, 
          main='(1,1,1) Model Residuals')

fcast2 <- forecast(fit2, h=pred_length)
plot(fcast2)
lines(ts(ma))
abline(v=seq(0,300,168), col="red") # Täglich
abline(v=seq(0,300,24), col="green")  # Stündlich
##################################################

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

