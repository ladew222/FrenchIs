#install.packages("sf") # helps us work with spatial data
## for os specific requirments and directions see https://github.com/r-spatial/sf
#install.packages("tmap") #helps create simple choropleths
#install.packages("plotly") #helps create scatterplots
library(sf)
library(tmap)
library(plotly)
library(tmaptools)
library(raster)
library(ggmap)

register_google(key = 'AIzaSyCYGK_waYm7jqyZY4MRixwFgMZt8q1lEoA')
raw <- read.csv("~/Projects/FrenchIs/data/campbell.csv", as.is=TRUE)
Addresses <- paste(raw$Site_Address, ", La Crosse, WI, 54603", sep = " ") 
Member_Coordinates <- geocode(Addresses)
write.csv(Member_Coordinates,"geocode.csv")
#raw <- geocoded_addresses[!is.na(site_address) & !is.na(geocoded_addresses$Longitude),]


print(Member_Coordinates[,1:2])
raw$Latitude<-Member_Coordinates$lat
raw$Longitude<-Member_Coordinates$lon
geocoded_addresses <- raw[!is.na(raw$Latitude) & !is.na(raw$Longitude),]
head(geocoded_addresses)
write.csv(geocoded_addresses,"geocoded.csv")
# Convert sf object for spatial analysis
my.sf.point <- st_as_sf(x = geocoded_addresses, 
                        coords = c("Longitude", "Latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# simple plot
plot(my.sf.point)
# interactive map:
library(mapview)
mapview(my.sf.point)
# Convert data frame to sf object so it can be converted into a planar object
my.sp.point <- as(my.sf.point, "Spatial")
##PPP for point analysis;
pppd <- as(my.sp.point, "ppp")
plot(pppd)

