library(data.table)
library(tidyr)

#read data

parking_orig = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Paid_Parking_Occupancy__Last_30_Days_.csv")
weather_2 = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-04-22T09_14_54.csv", header = T, sep = ";", skip = 11)
weather_1 = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/history_export_2019-04-08T16_02_14.csv", header = T, sep = ";", skip = 11)
events = fread("../Schramm, Cornelius - 02_Business_Analytics_Data/Special_Events_Permits.csv")

# Clean Event Data

events = separate(events,`Event End Date`, c("End.date", "End.time", "End AM/PM"), sep=" ")
events$End.date = as.Date(events$End.date, "%m/%d/%Y")
events = separate(events,`Event Start Date`, c("Start.date", "Start.time", "Start AM/PM"), sep=" ")
events$Start.date = as.Date(events$Start.date, "%m/%d/%Y")
events = events[, -c(9,10, 12, 13)]
events = events[, -c(1,5,6,7,14)]

complete_index = which(events$`Permit Status`== "Complete")
events = events[complete_index]

# merge weather data

weather_all = merge(weather_1, weather_2, all = T)
weather_all = weather_all[-337,]

fwrite(weather_all, "weather_all.csv", sep = ",")

