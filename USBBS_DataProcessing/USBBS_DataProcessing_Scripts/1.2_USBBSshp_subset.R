
library(rgdal)
library(tidyverse)
library(mapview)

# import dataset subset for years 00.01.15.16 
load("USBBS_DataProcessing/USBBS_00.01_15.16.rda") 
usbbs.data <- as_tibble(usbbs.data) %>% mutate_if(is.factor, as.character)

# import route csv data, obtained from USBBS website
usbbs.routes <- read.csv("D:/USBBS_DATA/USBBS_Diversity/routes.csv") 

usbbs.routes$StateNum <- str_pad(usbbs.routes$StateNum, width=2, side="left", pad="0")  
usbbs.routes$Route <- str_pad(usbbs.routes$Route, width=3, side="left", pad="0")

usbbs.routes$U_S_R_I <- paste0(usbbs.routes$StateNum, "_", usbbs.routes$Route)

#'
#' *setup shapefile with route starting point for segment ordering from QGIS output*
#'
#'
# generate starting point csv for each route to get directionality over routes segment shapefile, see methodology supplementary material

usbbs.routes.start <- usbbs.routes %>% 
                        filter(U_S_R_I %in% usbbs.data$U_S_R_I) %>%
                        filter(CountryNum==840) %>%
                        select(Latitude, Longitude, U_S_R_I)

# get only vector with routename
usbbs.routes <- usbbs.routes.start$U_S_R_I 

#' import routes shp cleaned as indicated in folder USBBS_shapefile_cleaning
routes.shp <- readOGR("USBBS_DataProcessing/routes_fixed/Routes_Compiled.shp")

#quick visualization of shp
mapview(routes.shp)

# subset to keep only routes that were surveyed consistently in the years of interested 
usbbs.routes <- subset(routes.shp, U_S_R_I %in% usbbs.routes.start$U_S_R_I)

# one route is 25 miles circa, which is 40.23 km, check that each route is within that length in shapefile
# keep only routes of circa 40230 meters, RTELENG field in shapefile attributes is route length in meters
usbbs.routes <- subset(usbbs.routes, RTELENG > 32186) # longer than 20 miles
usbbs.routes <- subset(usbbs.routes, RTELENG < 48280) # shorter than 30 miles

#' *this cuts the routes from 1413 to 1407 and then again down 1317*

mapview(usbbs.routes)

# save shapefile
rgdal::writeOGR(usbbs.routes, dsn="D:/USBBS_DATA/USBBS_LandCover/USBBS_shps/USBBS_routes", 
                layer="USBBS.routes.00.01.15.16", driver = "ESRI Shapefile",
                check_exists = FALSE)

#'
#' *GIS processing, using v.split GRASS geoprocessing tool* 
#' *--- Divide routes into 5 segments of equal length, approx. 5 miles/8km long each considering the whole route length of 25 miles circa * 
#' 

# import the now segmented route shapefile
segments.shp <- readOGR("D:/USBBS_DATA/USBBS_LandCover/USBBS_shps/USBBS_routes/USBBS.routes.segmented.shp")

#quick visualization of shp
mapview(segments.shp)

segments.data <- segments.shp@data

# count how many segments per route and keep only routes with 5 segments
segments.data <- segments.data %>% count(U_S_R_I) %>% filter(n==5)

#' *this reduces the total number of routes to 1140*

# select only vector features of selected routes 
segments.shp.clean <- subset(segments.shp, U_S_R_I %in% as.vector(segments.data$U_S_R_I))

mapview(segments.shp.clean)

# save shapefile
rgdal::writeOGR(segments.shp.clean, dsn="D:/USBBS_DATA/USBBS_LandCover/USBBS_shps/USBBS_routes", 
                layer="FINAL_USBBS.segmented.00.01.15.16", driver = "ESRI Shapefile",
                check_exists = FALSE)

# update route start point shapefile made in QGIS with new route subset
usbbs.routes.start <- usbbs.routes.start %>% filter(U_S_R_I %in% as.vector(segments.data$U_S_R_I))
  
write.csv(usbbs.routes.start, "USBBS_DataProcessing/usbbs.routes.start.csv", row.names=F) 

#' * made into point vector shapefile by importing csv into GIS*
