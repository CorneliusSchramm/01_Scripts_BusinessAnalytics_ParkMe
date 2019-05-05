# Description ---------------

# In this script
# - cluster the parking meters 
# - by geographical location
# - using longitude and latitude


# Setup ----------------------------------------------

# Load required packages
library(tidyverse)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(ggmap)

# Clear workspace
rm(list=ls())
graphics.off()

# Load the previousely saved merged version of our parking data
load("../02_Business_Analytics_Data/df_set_02_merged.RData")
load("../Schramm, Cornelius - 02_Business_Analytics_Data/df_set_01.RData")

df = DF_hourly[,c(1,11,12)]
locations = data.frame(df[!duplicated(df[,c("SourceElementKey","lon","lat")]),][,c(1:3)])
row.names(locations) = NULL

# Filter parkingmetersoutside the center of the city
locations[,2] = as.numeric(locations[,2])
locations[,3] = as.numeric(locations[,3])
locations = locations %>%
  filter(lat < 47.64 & lat > 47.59) %>%
  filter(lon > -122.36 & lon < -122.30)

# ...
ysteps = seq(47.64, 47.59, -0.0015)
xsteps = seq(-122.36, -122.30, 0.0025)

# Map Grid to create
register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")
map = get_map("Seattle", zoom = 13)
ggmap(map) + 
  geom_point(data=locations, 
             mapping=aes(x=lon,
                         y=lat),
             alpha=.8) +
  ylim(47.59, 47.64) +
  xlim(-122.375, -122.3) +
  geom_hline(yintercept = ysteps, color="red") +
  geom_vline(xintercept = xsteps, color="red")

for (i in seq(1,length(xsteps),1)) {
  locations[locations$lon > xsteps[i] & locations$lon < ysteps[i+1], "xcluster"] = i
}

for (i in seq(1,length(ysteps),1)) {
  locations[locations$lat < ysteps[i] & locations$lat > ysteps[i+1], "ycluster"] = i
}

locations$cluster = paste0(locations$xcluster,"-",locations$ycluster)

table(factor(as.character(locations$cluster)))
