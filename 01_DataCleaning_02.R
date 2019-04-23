# Setup ----------------------------------------------

# Load required packages
library(data.table)
library(tidyverse)

library(ggmap)
library(dplyr)
library(ggplot2)


# Clear workspace
rm(list=ls())
graphics.off()

# Load data (find sources __________)



load("df_set_01.RData")

# Merge all Datasets ---------------------------------

# Merge "holidays" and "dates" datasets
df0 = merge(holidays, dates, by="date", all=TRUE)
# Turn is_holiday into a binary variable
df0[is.na(df0$is_holiday),"is_holiday"] = 0
# Remove holiday name column
df0 = df0[,-2]

# rm(dates, holidays)

# Merge "weather" with temporary dataframe "df0"
df0 = merge(weather, df0, by="date", all=TRUE)

# rm(dates, holidays)

# Merging the next dataframe will get more difficult. Different times??? help!