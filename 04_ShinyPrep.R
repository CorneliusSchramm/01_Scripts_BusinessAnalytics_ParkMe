# Description ---------------

# In this script
# - we finalize cleaning
# - output will be the final dataframe for analysis

# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)
library(ggplot2)



# Clear workspace
rm(list=ls())
graphics.off()

load("../02_Business_Analytics_Data/FinalDFKmean.RData")
load("../02_Business_Analytics_Data/shinyPredsDF.RData")
# load("../02_Business_Analytics_Data/df_set_02_merged.RData")
load("../02_Business_Analytics_Data/pm_kmClust_relation.RData")

load("../Schramm, Cornelius - 02_Business_Analytics_Data/FinalDFKmean.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/shinyPredsDF.RData")
# load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_02_merged.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/pm_kmClust_relation.RData")


# ------

start = as.POSIXct("2019-04-16 08:00:00")
end = as.POSIXct("2019-04-22 17:00:00 ")
datetime = as.data.table(seq(from = start,
                            by = "hour",
                            to = end))
colnames(datetime) = "datetime"

datetime = separate(datetime,"datetime", c(NA, "hour"), sep=" ", remove = F)
datetime = separate(datetime,"hour", c("hour",NA), sep=":")

datetime$hour = as.numeric(datetime$hour)
datetime = datetime %>%
  filter(hour<=17 & hour>= 8)
datetime$weekdays = weekdays(datetime$datetime)
datetime = filter(datetime, weekdays != "Sonntag")
datetime = datetime[,-3]

load("../Schramm, Cornelius - 02_Business_Analytics_Data/results_glm.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/results_ts.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/results_rf.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/best_model.RData")
best_model = as.data.frame(best_model)
rf_cluster = c(which(best_model$best_model == "RF" ))+1
glm_cluster = c(which(best_model$best_model == "GLM" ))+1
ts_cluster = c(which(best_model$best_model == "TS" ))+1

preds = data.frame(DF_RF$datetime, DF_RF[,rf_cluster],DF_RF[,glm_cluster],DF_RF[,ts_cluster])

# Clusterpreds   an datetime                                                                                  // datetime
# Wir brauchen die predictins aus dem dataanalysis file für alle 30 cluster für die ganze Woche  // clustpred (abs) datetime cluster PSCClust
# Reference DF zum zuordnen von SEK und cluster   // cluster sek psc lon lat
# dann mergen der beiden obigen über cluster mergen // datetime SEK psc lon lat cluster clustpred (abs) PSCCluster
# dann free percent pro cluster = pro SEK schaffen  // datetime SEK psc lon lat cluster clustpred (abs) PSCCluster FreePercClust FreeSpotsSEK

preds = gather(preds, key= cluster, value = FreeSpotsCluster, 2:31)

preds$cluster = gsub("X", "", preds$cluster)
preds$cluster = as.numeric(preds$cluster)
colnames(preds)[1] = "datetime"
preds <- preds[order(preds$cluster,preds$datetime),]
row.names(preds) = NULL

load("../Schramm, Cornelius - 02_Business_Analytics_Data/clustCap.RData")
preds = merge(preds, aggregated_df, by = "cluster")
preds$clustFreePerc = preds[,3]/preds[,4]

#reference DF
referenceDF = data.frame(DF_clustered_slim[!duplicated(DF_clustered_slim[,"SourceElementKey"]),][,c(1:2,5)])

temp = merge(preds[,c(1,2,5)], referenceDF, by="cluster")
nrow(filter(temp, datetime == "2019-04-16 08:00:00"))

load("../Schramm, Cornelius - 02_Business_Analytics_Data/locations.RData")
shinyPredPlot = merge(temp, locations[,2:4], by="SourceElementKey")
shinyPredPlot$pmFreeSpots = shinyPredPlot$clustFreePerc * shinyPredPlot$ParkingSpaceCount

# Create hour column
preds = separate(preds, datetime, into = c("date", "hour"), sep=" ")
preds = separate(preds, hour, into = c("hour", NA, NA), sep=":")
preds$hour = as.numeric(preds$hour)
preds$date = as.Date(preds$date)

shinyPredPlot = separate(shinyPredPlot, datetime, into = c("date", "hour"), sep=" ")
shinyPredPlot = separate(shinyPredPlot, hour, into = c("hour", NA, NA), sep=":")
shinyPredPlot$hour = as.numeric(shinyPredPlot$hour)
shinyPredPlot$date = as.Date(shinyPredPlot$date)

# Save -----

rm(list=setdiff(ls(), list("shinyPredPlot","preds")))
save.image("../Schramm, Cornelius - 02_Business_Analytics_Data/4SHINY.RData")
