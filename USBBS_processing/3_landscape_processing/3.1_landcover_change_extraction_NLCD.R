library(tidyverse); library(sf); library(stringr); library(exactextractr); library(raster); library(mapview); library(dplyr)

shp <- read_sf("USBBS_data/routes_data/5_segments_final_2years.shp") %>% 
  mutate(route = str_sub(partition,1, 6)) %>% st_transform(5070)
shp$partition[shp$route == "53_800"] <- paste0("53_800_", 1:5)

# load in raster containing land cover change categorized pixels, 30m per pixel
# @ https://www.mrlc.gov/data & https://www.srs.fs.usda.gov/pubs/60009
#' This is not provided in the repo as they are quite big
lc.change <- raster::raster("D:/USA_LandCover/USA_lc_change/NLCD_01_16_LC_change.img")

#'--------------------------------------------------------------------------------------------------------
#'*Extract land cover change from official change product to compare with delta calculated in 1.0 scripts*
#'*at different buffer sizes and centered around segment curve or centroid*
#'

buffers <- c(500, 1000, 2000, 4000, 6000) 
buffer.type <- c("centroid", "segment")
routes <- unique(shp$route)

# setup loop across buffer sizes
for (size.buffer in buffers) {
  
  # snake-like buffer around each segment/line    
  segment.buffer <- st_buffer(shp, dist=size.buffer)
  
  # circular buffer around the centroid of each segment
  centroid.buffer <- st_buffer(st_centroid(shp), dist=size.buffer)
  
  # setup loop across lc years   
  for(b in buffer.type) {
    
    # pick 10 routes at the time as I have 12 cores available to parallel
    for(r in seq(1,length(routes),2) ) {
      
      print(paste0("working on ",b," buffer = ", size.buffer," on route ", r, "/",length(routes)," ..."))
      
      if(b == "centroid") {
        
        centroid.temp <- centroid.buffer %>% filter(route == routes[r] | route == routes[r+1])
        extracted.lc <- exact_extract(x=lc.change, y=centroid.temp) 
        temp.partition <- centroid.temp$partition
        
      }
      
      if(b == "segment") {
        
        segment.temp <- segment.buffer %>% filter(route == routes[r] | route == routes[r+1])
        extracted.lc <- exact_extract(x=lc.change, y=segment.temp)
        temp.partition <- segment.temp$partition
        
      }
      
      temp.shp <- tibble("partition" = temp.partition)
      
      extracted <- lapply(extracted.lc, function(x){ x %>% dplyr::select(value) %>% c() %>% unlist()})

      #' calculate *% of urban land cover*, class = 3(urban change)
      temp.shp$change.urban <- unlist(lapply(extracted, function(x){ (sum(x==3)/length(x)) })) *100

      #' calculate *% of forest land cover*, class = 11(forest change) 
      temp.shp$change.forest <- unlist(lapply(extracted, function(x){ sum(x==11)/length(x) })) *100

      #' calculate *% of grassland cover*, class = 9(Shrub grass change) + 8(Pasture/Hay change)
      temp.shp$change.grass <- unlist(lapply(extracted, function(x){ sum(x==9 | x==8)/length(x) })) *100

      #' calculate *% of cropland cover*, class = 6(agriculture within change) + 7(cultivated crop change)
      temp.shp$change.crop <- unlist(lapply(extracted, function(x){ sum(x==6 | x==7)/length(x) })) *100

      #' calculate *% of barren*, class = 10(Barren change)
      temp.shp$change.barren <- unlist(lapply(extracted, function(x){ sum(x==10)/length(x) })) *100

      #' calculate *% of water*, class = 2(water change)
      temp.shp$change.water <- unlist(lapply(extracted, function(x){ sum(x==2)/length(x) })) *100

      #' calculate *% of wetland cover*, class = 4(Wetland within change) + 5(Herbaceous Wetland change) + 12(Woody wetland change)
      temp.shp$change.wet <- unlist(lapply(extracted, function(x){ sum(x==4 | x==5 | x==12) /length(x) })) *100

      temp.shp[2:ncol(temp.shp)] <- round(temp.shp[2:ncol(temp.shp)],2)
      
      if (r==1) lc.df <- temp.shp
      
      if (r>1) lc.df <- rbind(lc.df, temp.shp)
      
    }
    
    lc <- left_join(shp, lc.df, by="partition")
    
    save(lc, file = paste0("USBBS_data/landscape_data/landcover_data/landcover_change_product_extracted/lc_change_", b, "_", size.buffer, ".rda"))
    
    rm(list=c("extracted", "extracted.lc", "lc", "temp.shp", "lc.df"))
    
  }
}

