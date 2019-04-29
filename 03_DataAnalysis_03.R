#Play with SpatioTemporal Package

library(spacetime)
library(rgdal)


coords_1 = DF_final_small[,c(1:3)]
coords_1$latlon = paste(coords_1$lat, coords_1$lon, sep = " ")
coords_2 = coords_1[!duplicated(coords_1$latlon),]
coords_2 = coords_2[,-4]
coords_3 = data.frame(coords_2, row.names = NULL)
coords_3$lon = as.numeric(coords_3$lon)
coords_3$lat = as.numeric(coords_3$lat)
colnames(coords_3)[1] = "ID"

DF_final_small_SP = data.frame(DF_final_small, row.names = NULL)

SP_DF = SpatialPointsDataFrame(coords_3, DF_final_small_SP, match.ID = "SourceElementKey")


