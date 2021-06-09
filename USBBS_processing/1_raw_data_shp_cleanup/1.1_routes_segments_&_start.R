#'
#'*ROUTES shapefile import, START POINTS import, SEGMENTS import and cleanup*
#'

library(rgdal); library(tidyverse); library(mapview); library(sf)

# import dataset subset for years 00.01.02 & 15.16.17 
load("USBBS_data/diversity_data/2years_x_timepoint/USBBS_years_subset.rda") 

# import route csv data, obtained from USBBS website
usbbs.routes <- read.csv("USBBS_data/routes_data/routes_info.csv") 

usbbs.routes$StateNum <- str_pad(usbbs.routes$StateNum, width=2, side="left", pad="0")  
usbbs.routes$Route <- str_pad(usbbs.routes$Route, width=3, side="left", pad="0")
usbbs.routes$U_S_R_I <- paste0(usbbs.routes$StateNum, "_", usbbs.routes$Route)

#'
#' *setup shapefile with route starting point for segment ordering from QGIS output*

usbbs.routes.start <- usbbs.routes %>% filter(CountryNum==840) %>%
                        filter(U_S_R_I %in% usbbs.data$U_S_R_I) %>%
                          select(Latitude, Longitude, U_S_R_I)
length(unique(usbbs.routes.start$U_S_R_I))

#'
#' *import routes shapefile*
#' from https://databasin.org/datasets/02fe0ebbb1b04111b0ba1579b89b7420/

routes.shp <- read_sf("USBBS_data/routes_data/1_routes_raw.shp") %>%
                filter(U_S_R_I %in% usbbs.routes.start$U_S_R_I) # 1195 routes
length(unique(routes.shp$U_S_R_I))
# quick visualization of shp
mapview(routes.shp)

# one route is 25 miles circa, which is 40.23 km, check that each route is within that length in shapefile
# keep only routes of circa 40230 meters, RTELENG field in shapefile attributes is route length in meters
routes.shp <- subset(routes.shp, RTELENG > 32186) # longer than 20 miles, 1407 kept
routes.shp <- subset(routes.shp, RTELENG < 48280) # shorter than 30 miles, 1311 kept

mapview(routes.shp)

# save shapefile
write_sf(routes.shp, dsn="USBBS_data/routes_data/2_routes_clean.shp")

#'
#' *import shp in QGIS, use v.split GRASS geoprocessing tool* 
#' *Divide routes into 5 segments of equal length, approx. 5 miles/8km long each considering the whole route length of 25 miles circa * 
#' 

# import the now segmented route shapefile
segments.shp <- read_sf("USBBS_data/routes_data/3_routes_segmented.shp")

#quick visualization of shp
mapview(segments.shp)

# count how many segments per route and keep only routes with 5 segments
segments.shp <- segments.shp %>% filter(U_S_R_I %in% routes.shp$U_S_R_I)
segments.data <- as.data.frame(segments.shp) %>% select(-geometry) %>% count(U_S_R_I) %>% filter(n==5)
segments.shp <- subset(segments.shp, U_S_R_I %in% as.vector(segments.data$U_S_R_I))

length(unique(segments.shp$U_S_R_I))
#' this keeps 4805 segments which divided by 5 is 961 routes 
mapview(segments.shp)

# save shapefile
write_sf(segments.shp, dsn="USBBS_data/routes_data/3.1_routes_segmented_cleaned.shp")

#' *make routes start point into a shp and keep only start points of U_S_RI in segments*
usbbs.routes.start <- usbbs.routes.start %>% filter(U_S_R_I %in% as.vector(segments.data$U_S_R_I))
usbbs.routes.start <- st_as_sf(usbbs.routes.start, crs=st_crs(segments.shp), coords = c("Longitude", "Latitude"))
mapview(usbbs.routes.start)
write_sf(usbbs.routes.start, dsn="USBBS_data/routes_data/4_routes_start_points.shp") 

