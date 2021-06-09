library(tidyverse); library(sf); library(stringr); library(exactextractr); library(raster); library(mapview); library(dplyr)

mapview(shp)

# get list of available partitions from routes/segment shp
# do all extractions for version of 2 years aggregate for timepoint, subset later for space and time economics
shp <- read_sf("USBBS_data/routes_data/5_segments_final_2years.shp") %>%  
  mutate(route = str_sub(partition,1, 6)) %>% st_transform(5070)
shp$partition[shp$route == "53_800"] <- paste0("53_800_", 1:5)
                                               
# load in raster containing land cover categorized pixels, 30m per pixel
# @ https://www.mrlc.gov/data
#' These are not provided in the repo as they are 50 GB each, download from link above for your use
lc.2001 <- raster::raster("D:/USA_LandCover/USA_lc/NLCD_01_LC.img") 
lc.2016 <- raster::raster("D:/USA_LandCover/USA_lc/NLCD_16_LC.img") 

# setup raster stack, each band is a year, 2001 = 1 & 2016 = 26
lcs <- raster::stack(lc.2001, lc.2016); rm(lc.2001, lc.2016)

#'--------------------------------------------------------------------------------------------------------
#'*Extract land cover % at each timepoint and delta between for each partition (USBBS 10 point aggregate)*
#'*at different buffer sizes and centered around segment curve or centroid*
#'

buffers <- c(500, 1000, 2000, 4000, 6000) 
buffer.type <- c("centroid", "segment")
routes <- unique(shp$route)

# this could take about an hour, it can be faster if route loop is removed but requires RAM > 16 GB
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
      extracted.lc <- exact_extract(x=lcs, y=centroid.temp) 
      temp.partition <- centroid.temp$partition
          
    }
      
    if(b == "segment") {
        
      segment.temp <- segment.buffer %>% filter(route == routes[r] | route == routes[r+1])
      extracted.lc <- exact_extract(x=lcs, y=segment.temp)
      temp.partition <- segment.temp$partition
          
    }

    temp.shp <- tibble("partition" = temp.partition)
      
    extracted.t1 <- lapply(extracted.lc, function(x){ x %>% dplyr::select(NLCD_01_LC) %>% c() %>% unlist()})
    extracted.t2 <- lapply(extracted.lc, function(x){ x %>% dplyr::select(NLCD_16_LC) %>% c() %>% unlist()})
      
    #' calculate *% of urban land cover*, class = 21(Developed-Open Space) + 22(Developed-Low Intensity) + 23(Developed-Medium Intensity) + 24(Developed-High Intensity)
    temp.shp$urban.t1 <- unlist(lapply(extracted.t1, function(x){ (sum(x==21 | x==22 | x==23 | x==24)/length(x)) })) *100
    temp.shp$urban.t2 <- unlist(lapply(extracted.t2, function(x){ (sum(x==21 | x==22 | x==23 | x==24)/length(x)) })) *100 
    temp.shp$delta.urban <- temp.shp$urban.t2 - temp.shp$urban.t1 
    
    #' calculate *% of forest land cover*, class = 41(Deciduous Forest) + 42(Evergreen Forest) + 43(Mixed Forest) 
    temp.shp$forest.t1 <- unlist(lapply(extracted.t1, function(x){ sum(x==41 | x==42 | x==43)/length(x) })) *100
    temp.shp$forest.t2 <- unlist(lapply(extracted.t2, function(x){ sum(x==41 | x==42 | x==43)/length(x) })) *100
    temp.shp$delta.forest <- temp.shp$forest.t2 - temp.shp$forest.t1 
    
    #' calculate *% of grassland cover*, class = 52(Shrub) + 71(Grassland/Herbaceous) + 81(Pasture/Hay)
    temp.shp$grass.t1 <- unlist(lapply(extracted.t1, function(x){ sum(x==52 | x==71 | x==81)/length(x) })) *100
    temp.shp$grass.t2 <- unlist(lapply(extracted.t2, function(x){ sum(x==52 | x==71 | x==81)/length(x) })) *100
    temp.shp$delta.grass <- temp.shp$grass.t2 - temp.shp$grass.t1 
    
    #' calculate *% of cropland cover*, class = 82(Cultivated Crops) 
    temp.shp$crop.t1 <- unlist(lapply(extracted.t1, function(x){ sum(x==82)/length(x) })) *100
    temp.shp$crop.t2 <- unlist(lapply(extracted.t2, function(x){ sum(x==82)/length(x) })) *100
    temp.shp$delta.crop <- temp.shp$crop.t2 - temp.shp$crop.t1 
    
    #' calculate *% of wetland cover*, class = 90(Woody Wetland) + 95(Herbaceous Wetland)
    temp.shp$wet.t1 <- unlist(lapply(extracted.t1, function(x){ sum(x==90 | x==95)/length(x) })) *100
    temp.shp$wet.t2 <- unlist(lapply(extracted.t2, function(x){ sum(x==90 | x==95)/length(x) })) *100
    temp.shp$delta.wet <- temp.shp$wet.t2 - temp.shp$wet.t1 
    
    #' calculate *% of water*
    temp.shp$water.t1 <- unlist(lapply(extracted.t1, function(x){ sum(x==11)/length(x) })) *100
    temp.shp$water.t2 <- unlist(lapply(extracted.t2, function(x){ sum(x==11)/length(x) })) *100
    temp.shp$delta.water <- temp.shp$water.t2 - temp.shp$water.t1 
    
    #' calculate *% of barren*, 
    temp.shp$barren.t1 <- unlist(lapply(extracted.t1, function(x){ sum(x==31)/length(x) })) *100
    temp.shp$barren.t2 <- unlist(lapply(extracted.t2, function(x){ sum(x==31)/length(x) })) *100
    temp.shp$delta.barren <- temp.shp$barren.t2 - temp.shp$barren.t1 
    
    #' calculate *land cover diversity*, as q=0 or species richness.
    temp.shp$lc.q0.t1 <- unlist(lapply(extracted.t1, function(x){ length(unique(x))}))
    temp.shp$lc.q0.t2 <- unlist(lapply(extracted.t2, function(x){ length(unique(x))}))
    temp.shp$delta.lc.q0 <- temp.shp$lc.q0.t2 - temp.shp$lc.q0.t1 
    
    #' calculate *landscape heterogeneity/diversity*, as q=1 or Shannon entropy measure.
    #' Total number of different entities (in this case land covers) when accounting for their abundance
    #' i.e. Effective number of land cover
    temp.shp$lc.q1.t1 <- unlist(lapply(extracted.t1, function(x){ # get vector of abundance of each land cover
                                                                abundance.vector <- tibble(lc = as.factor(x)) %>% count(lc) %>% dplyr::select(n)
                                                                # calculate ratio of each lc category within total n of pixels
                                                                ratio.vector <- as.vector(t(abundance.vector/sum(abundance.vector)))
                                                                # get Shannon entropy as effective number of species, q=1
                                                                exp(-sum(ratio.vector * log(ratio.vector)))}))
    temp.shp$lc.q1.t2 <- unlist(lapply(extracted.t2, function(x){ abundance.vector <- tibble(lc = as.factor(x)) %>% count(lc) %>% dplyr::select(n)
                                                                    ratio.vector <- as.vector(t(abundance.vector/sum(abundance.vector)))
                                                                    exp(-sum(ratio.vector * log(ratio.vector)))}))
    temp.shp$delta.lc.q1 <- temp.shp$lc.q1.t2 - temp.shp$lc.q1.t1 
    
    temp.shp[2:ncol(temp.shp)] <- round(temp.shp[2:ncol(temp.shp)],2)
      
    if (r==1) lc.df <- temp.shp
      
    if (r>1) lc.df <- rbind(lc.df, temp.shp)
    
    }
    
    lc <- left_join(shp, lc.df, by="partition")
  
    save(lc, file = paste0("USBBS_data/landscape_data/landcover_data/landcover_%_&_delta_extracted/lc_", b, "_", size.buffer, ".rda"))
    
    rm(list=c("extracted.t1", "extracted.t2", "extracted.lc", "lc", "temp.shp", "lc.df"))
  
  }
}

sf::st_as_sf(lc)
