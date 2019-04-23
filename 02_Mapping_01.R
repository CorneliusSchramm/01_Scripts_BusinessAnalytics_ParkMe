##########
locations = data.frame(unique(parking_orig$Location))
colnames(locations) = c("Location")
locations$Location = gsub("POINT \\(", "", locations$Location)
locations$Location = gsub("\\)", "", locations$Location)
locations = separate(locations, Location, into = c("lon", "lat"), sep=" ")

ggplot() +
  geom_point(data=locations, aes(x=lon, y=lat))

register_google(key="AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs")

# AIzaSyAfPULmtU7hUcoj4lboRAbzVg-810wrkJs


map.seattle_city <- qmap("seattle", zoom = 11, source="stamen", maptype="toner",darken = c(.3,"#BBBBBB"))
map.seattle_city
#########
length(unique(parking_orig$SourceElementKey))

parking_one = parking_orig %>%
  filter(SourceElementKey==11670)

parking_one = separate(parking_one, OccupancyDateTime, into = c("date", "time", "am/pm"), sep=" ")


# Edit time format
parking_one$time = paste0(parking_one$time, " ", parking_one$`am/pm`)
parking_one$time = format(strptime(parking_one$time, "%I:%M:%S %p"), format="%H:%M")
parking_one = parking_one %>%
  filter(date=="03/18/2019")

##########
