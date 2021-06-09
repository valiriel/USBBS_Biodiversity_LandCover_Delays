#'-------------------------------------------------------------------------------------------------
#' * Sourcing climatic and elevation data *  

library(sf); library(tidyverse); library(exactextractr)

shp <- read_sf("USBBS_data/routes_data/5_segments_final.shp") %>%  
  mutate(route = str_sub(partition,1, 6)) %>% st_transform(5070)
shp$partition[shp$route == "53_800"] <- paste0("53_800_", 1:5)

buffers <- c(500, 1000, 2000, 4000, 6000)
years <- c(2000, 2001, 2015, 2016)

#'-------------------------------------------------------------------------------------------------
#' * Temperature and precipitation *
#' Data from *PRISM*, 4km gridded data, https://prism.oregonstate.edu/recent/
# download your own into climate_raw folder

# load in average temperature rasters in June and July for years
folder <- "USBBS_data/landscape_data/climate_elev_data/climate_raw/tmean"
tmean.stack.t1 <- stack(raster(paste0(folder,"/","PRISM_","tmean","_4km_",years[1],"_May.asc")),
                        raster(paste0(folder,"/","PRISM_","tmean","_4km_",years[1],"_June.asc")),
                        raster(paste0(folder,"/","PRISM_","tmean","_4km_",years[2],"_May.asc")),
                        raster(paste0(folder,"/","PRISM_","tmean","_4km_",years[2],"_June.asc")))
tmean.stack.t2 <- stack(raster(paste0(folder,"/","PRISM_","tmean","_4km_",years[3],"_May.asc")),
                        raster(paste0(folder,"/","PRISM_","tmean","_4km_",years[3],"_June.asc")),
                        raster(paste0(folder,"/","PRISM_","tmean","_4km_",years[4],"_May.asc")),
                        raster(paste0(folder,"/","PRISM_","tmean","_4km_",years[4],"_June.asc")))

# load in precipitation rasters in June and July for years
folder <- "USBBS_data/landscape_data/climate_elev_data/climate_raw/prec"    
prec.stack.t1 <- stack(raster(paste0(folder,"/","PRISM_","prec","_4km_",years[1],"_May.asc")),
                       raster(paste0(folder,"/","PRISM_","prec","_4km_",years[1],"_June.asc")),
                       raster(paste0(folder,"/","PRISM_","prec","_4km_",years[2],"_May.asc")),
                       raster(paste0(folder,"/","PRISM_","prec","_4km_",years[2],"_June.asc")))
prec.stack.t2 <- stack(raster(paste0(folder,"/","PRISM_","prec","_4km_",years[3],"_May.asc")),
                       raster(paste0(folder,"/","PRISM_","prec","_4km_",years[3],"_June.asc")),
                       raster(paste0(folder,"/","PRISM_","prec","_4km_",years[4],"_May.asc")),
                       raster(paste0(folder,"/","PRISM_","prec","_4km_",years[4],"_June.asc")))

#'-------------------------------------------------------------------------------------------------
#' * Elevation *
#' Data from *SRTM* sourced automatically from raster package, 90m gridded data, https://srtm.csi.cgiar.org/
# download your own into elevation_raw folder

#raster::getData('alt', country='USA', path="USBBS_data/landscape_data/climate_elev_data/elevation_raw")
# keep only contiguous mainland USA
elevation <- raster("USBBS_data/landscape_data/climate_elev_data/elevation_raw/USA1_msk_alt.grd")

#'-------------------------------------------------------------------------------------------------
#' * Extract values *

for (size.buffer in buffers) {
  
  # snake-like buffer around each segment/line    
  segment.buffer <- st_buffer(shp, dist=size.buffer)
  
  # circular buffer around the centroid of each segment
  centroid.buffer <- st_buffer(st_centroid(shp), dist=size.buffer)
  
  #'---------------------------------------------------------------------------------------------
  #'  *mean temperature, precipitation, elevation* calculations

  climate.data <- tibble(partition = shp$partition, 
                         temp.t1 = rowSums(exact_extract(tmean.stack.t1, segment.buffer, "mean"))/nlayers(tmean.stack.t1), 
                         temp.t2 = rowSums(exact_extract(tmean.stack.t2, segment.buffer, "mean"))/nlayers(tmean.stack.t2),
                         temp.sd.t1 = rowSums(exact_extract(tmean.stack.t1, segment.buffer, "stdev"))/nlayers(tmean.stack.t1),
                         temp.sd.t2 = rowSums(exact_extract(tmean.stack.t2, segment.buffer, "stdev"))/nlayers(tmean.stack.t2),
                         prec.t1 = rowSums(exact_extract(prec.stack.t1, segment.buffer, "mean"))/nlayers(prec.stack.t1), 
                         prec.t2 = rowSums(exact_extract(prec.stack.t2, segment.buffer, "mean"))/nlayers(prec.stack.t2),
                         prec.sd.t1 = rowSums(exact_extract(prec.stack.t1, segment.buffer, "stdev"))/nlayers(prec.stack.t1),
                         prec.sd.t2 = rowSums(exact_extract(prec.stack.t2, segment.buffer, "stdev"))/nlayers(prec.stack.t2),
                         elev = exact_extract(elevation, segment.buffer, "mean"),
                         elev.sd = exact_extract(elevation, segment.buffer, "stdev"))
  climate.data$delta.temp <- climate.data$temp.t2 - climate.data$temp.t1 
  climate.data$delta.prec <- climate.data$prec.t2 - climate.data$prec.t1  
  save(climate.data, file=paste0("USBBS_data/landscape_data/climate_elev_data/climate_elev_extracted/climate_elev_segment_",size.buffer,".rda"))

  climate.data <- tibble(partition = shp$partition, 
                         temp.t1 = rowSums(exact_extract(tmean.stack.t1, centroid.buffer, "mean"))/nlayers(tmean.stack.t1), 
                         temp.t2 = rowSums(exact_extract(tmean.stack.t2, centroid.buffer, "mean"))/nlayers(tmean.stack.t2),
                         temp.sd.t1 = rowSums(exact_extract(tmean.stack.t1, centroid.buffer, "stdev"))/nlayers(tmean.stack.t1),
                         temp.sd.t2 = rowSums(exact_extract(tmean.stack.t2, centroid.buffer, "stdev"))/nlayers(tmean.stack.t2),
                         prec.t1 = rowSums(exact_extract(prec.stack.t1, centroid.buffer, "mean"))/nlayers(prec.stack.t1), 
                         prec.t2 = rowSums(exact_extract(prec.stack.t2, centroid.buffer, "mean"))/nlayers(prec.stack.t2),
                         prec.sd.t1 = rowSums(exact_extract(prec.stack.t1, centroid.buffer, "stdev"))/nlayers(prec.stack.t1),
                         prec.sd.t2 = rowSums(exact_extract(prec.stack.t2, centroid.buffer, "stdev"))/nlayers(prec.stack.t2),
                         elev = exact_extract(elevation, centroid.buffer, "mean"),
                         elev.sd = exact_extract(elevation, centroid.buffer, "stdev"))
  climate.data$delta.temp <- climate.data$temp.t2 - climate.data$temp.t1 
  climate.data$delta.prec <- climate.data$prec.t2 - climate.data$prec.t1  
  save(climate.data, file=paste0("USBBS_data/landscape_data/climate_elev_data/climate_elev_extracted/climate_elev_centroid_",size.buffer,".rda"))
  
}

