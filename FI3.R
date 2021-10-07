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
library(RColorBrewer)

register_google(key = 'AIzaSyCYGK_waYm7jqyZY4MRixwFgMZt8q1lEoA')
raw <- read.csv("~/Projects/FrenchIs/data/campbell.csv", as.is=TRUE)

register_google(key = 'AIzaSyCYGK_waYm7jqyZY4MRixwFgMZt8q1lEoA')
##lets simplify for now
#keep.cols <- c('SUM_PFOS_PFOA','Site_Address')
#filtered<- raw[, names(raw) %in% keep.cols]
#filtered<-raw
Addresses <- paste(raw$Site_Address, ", La Crosse, WI, 54603", sep = " ") 
Member_Coordinates <- geocode(Addresses)
write.csv(Member_Coordinates,"geocode.csv")
#raw <- geocoded_addresses[!is.na(site_address) & !is.na(geocoded_addresses$Longitude),]


print(Member_Coordinates[,1:2])
raw$Latitude<-Member_Coordinates$lat
raw$Longitude<-Member_Coordinates$lon

geocoded_addresses <- filtered[!is.na(filtered$Latitude) & !is.na(filtered$Longitude),]
head(geocoded_addresses)
write.csv(geocoded_addresses,"geocoded.csv")
#geocoded_addresses$Site_Address<-NULL
##need to remove -999
geocoded_addresses[geocoded_addresses=="-999"]<-NA


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

str(my.sp.point, 1)
names(my.sp.point)
summary(my.sp.point$SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA)

hist(my.sp.point$SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA)
##really imbalanced

# Make a vector that is TRUE for the missing data
miss <- is.na(my.sp.point$SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA)
table(miss)

## Number of intervals (colors)
N <- 8
## Sequential palette
ucQuantPal <- brewer.pal(n = N, "Accent")


# Plot a map of pfas
spplot(my.sp.point[!miss, ], "SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA", col.regions = ucQuantPal)
spplot(my.sp.point[!miss, ], "SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA",  col.regions=bpy.colors(300))
spplot(my.sp.point[!miss, ], "SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA", legendEntries = LETTERS[1:5])

library(sp)
library(raster)
library(grid)

spplot(my.sp.point[!miss, ], "SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA", at = seq(0,20,4000))
spplot(my.sp.point[!miss, ], "SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA", do.log = TRUE,
       key.space=list(x = 0.1, y = 0.95, corner = c(0, 1)),
       scales=list(draw = TRUE))


scale = list("SpatialPolygonsRescale", layout.scale.bar(), 
             offset = c(178600,332490), scale = 500, fill=c("transparent","black"))
text1 = list("sp.text", c(178600,332590), "0")
text2 = list("sp.text", c(179100,332590), "500 m")
arrow = list("SpatialPolygonsRescale", layout.north.arrow(), 
             offset = c(178750,332000), scale = 400)
spplot(my.sp.point[!miss, ], "SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA", do.log=T,
       key.space=list(x=0.1,y=0.93,corner=c(0,1)),
       sp.layout=list(scale,text1,text2,arrow),
       main = "Zinc (top soil)")



rv = list("sp.polygons", meuse.riv, fill = "blue", alpha = 0.1)
pts = list("sp.points", meuse, pch = 3, col = "grey", alpha = .7)



spplot(my.sp.point[!miss, ], "SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA", do.log = TRUE,
       key.space=list(x=0.2,y=0.9,corner=c(0,1)),
       scales=list(draw = TRUE), cuts = 4,
       legendEntries = c("low", "intermediate","high", "highest"))

mapview(my.sp.point[!miss, ], zcol = c("SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA", "PFBS"), legend = TRUE)

mapview(my.sp.point[!miss,],  zcol = "SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA", map.types = "Esri.WorldImagery", legend = TRUE)

mapview(my.sp.point[!miss,], zcol = "SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA", at = seq(1, 2200, 400), legend = TRUE)


mapview(my.sp.point[!miss,], zcol = "sqsum", at = seq(1, 20, 3), legend = TRUE)






my.sp.point$sqsum <- sqrt(my.sf.point$SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA)



coordnames(my.sp.point)

m_trend <- lm(my.sp.point_filtered$SUM_FOSA_NEtFOSE_NEtFOSA_NetFOSAA_PFOS_PFOA ~ coords.x1 + coords.x2, as.data.frame(my.sp.point_filtered))
#Check the coefficients
summary(m_trend)



demo(meuse, ask = FALSE, echo = FALSE) # loads the meuse data sets

