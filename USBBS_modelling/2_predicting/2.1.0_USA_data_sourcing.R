library(tidyverse); library(sf); library(stringr); library(exactextractr); library(raster); library(mapview); library(dplyr); library(ggthemes)

#'--------------------------------------------------------------------------------------------------------
#' * generate grid to extract landscape data and predict on*
#'

usa_borders <- read_sf("USBBS_data/map_predict_data/USA_border.shp") %>% st_transform(crs=5070) %>% group_by(ID) %>% summarise()
# ggplot(usa_borders) + geom_sf() + theme_bw()

# hexagonal grid, 10x10km, made in QGIS as its easier
usa_landscape <- read_sf("USBBS_data/map_predict_data/USA_landscape.shp") %>% st_transform(crs=5070)
# ggplot(usa_landscape) + geom_sf() + theme_bw()
usa_landscape$ID <- 1:nrow(usa_landscape)

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

hexs <- usa_landscape$ID 
h <- 1
h.seq <- seq(1,length(hexs),13)

# pick 10 routes at the time as I have 12 cores available to parallel
for(h in h.seq) {
    
  print(paste0("working on hexagon ", hexs[h], "/",length(hexs)," ..."))
    
  landscape.temp <- usa_landscape %>% filter(ID >= hexs[h] & ID < hexs[h+13])
  
  if(h == 92015)
    landscape.temp <- usa_landscape %>% filter(ID >= 92015 & ID < 92027 )
  
  extracted.lc <- exact_extract(x=lcs, y=landscape.temp) 

  temp.shp <- tibble("ID" = landscape.temp$ID)
    
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
  
  #' calculate *% of water*, class = 90(Woody Wetland) + 95(Herbaceous Wetland)
  temp.shp$water.t1 <- unlist(lapply(extracted.t1, function(x){ sum(x==11)/length(x) })) *100
  temp.shp$water.t2 <- unlist(lapply(extracted.t2, function(x){ sum(x==11)/length(x) })) *100
  temp.shp$delta.water <- temp.shp$water.t2 - temp.shp$water.t1 
  
  #' calculate *% of barren*, class = 90(Woody Wetland) + 95(Herbaceous Wetland)
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
  
  if (h==1) lc.df <- temp.shp
  
  if (h>1) lc.df <- rbind(lc.df, temp.shp)
  
}

usa_landscape <- left_join(usa_landscape, lc.df, by="ID")

save(usa_landscape, file = paste0("USBBS_data/map_predict_data/USA_landscape_lc.rda"))

#'-------------------------------------------------------------------------------------------------
#' * Temperature and precipitation *
#' Data from *PRISM*, 4km gridded data, https://prism.oregonstate.edu/recent/
# download your own into climate_raw folder

years <- c(2000, 2001, 2015, 2016)

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

#'  *mean temperature, precipitation, elevation* calculations

usa_landscape$temp.t1 = rowSums(exact_extract(tmean.stack.t1, usa_landscape, "mean"))/nlayers(tmean.stack.t1)
usa_landscape$temp.t2 = rowSums(exact_extract(tmean.stack.t2, usa_landscape, "mean"))/nlayers(tmean.stack.t2)
usa_landscape$temp.sd.t1 = rowSums(exact_extract(tmean.stack.t1, usa_landscape, "stdev"))/nlayers(tmean.stack.t1)
usa_landscape$temp.sd.t2 = rowSums(exact_extract(tmean.stack.t2, usa_landscape, "stdev"))/nlayers(tmean.stack.t2)
usa_landscape$prec.t1 = rowSums(exact_extract(prec.stack.t1, usa_landscape, "mean"))/nlayers(prec.stack.t1)
usa_landscape$prec.t2 = rowSums(exact_extract(prec.stack.t2, usa_landscape, "mean"))/nlayers(prec.stack.t2)
usa_landscape$prec.sd.t1 = rowSums(exact_extract(prec.stack.t1, usa_landscape, "stdev"))/nlayers(prec.stack.t1)
usa_landscape$prec.sd.t2 = rowSums(exact_extract(prec.stack.t2, usa_landscape, "stdev"))/nlayers(prec.stack.t2)
usa_landscape$elev = exact_extract(elevation, usa_landscape, "mean")
usa_landscape$elev.sd = exact_extract(elevation, usa_landscape, "stdev")
usa_landscape$delta.temp <- usa_landscape$temp.t2 - usa_landscape$temp.t1 
usa_landscape$delta.prec <- usa_landscape$prec.t2 - usa_landscape$prec.t1  

save(usa_landscape, file = paste0("USBBS_data/map_predict_data/USA_landscape_lc_climate.rda"))

#'-----------------------------------------------------------------------------------------------------
#'* generate vectors of positive & negative land cover change*

names(usa_landscape) <- c("geometry", "ID", "urban_t1", "urban_t2", "delta_urban", "forest_t1", "forest_t2", "delta_forest",
                          "grass_t1", "grass_t2", "delta_grass", "crop_t1", "crop_t2", "delta_crop", "wet_t1", "wet_t2",      
                          "delta_wet", "water_t1", "water_t2", "delta_water", "barren_t1", "barren_t2", "delta_barren", "lc_q0_t1",    
                          "lc_q0_t2", "delta_lc_q0", "lc_q1_t1", "lc_q1_t2", "delta_lc_q1", "temp_t1", "temp_t2", "temp_sd_t1",   
                          "temp_sd_t2", "prec_t1", "prec_t2", "prec_sd_t1", "prec_sd_t2", "elev", "elev_sd", "delta_temp", "delta_prec")

usa_landscape$delta_pos_urban <- 0; usa_landscape$delta_neg_urban <- 0
for(i in 1:nrow(usa_landscape)){ if(usa_landscape$delta_urban[i]>0) usa_landscape$delta_pos_urban[i] <- abs(usa_landscape$delta_urban[i])
if(usa_landscape$delta_urban[i]<=0) usa_landscape$delta_neg_urban[i] <- abs(usa_landscape$delta_urban[i]); print(i)}

usa_landscape$delta_pos_forest<- 0; usa_landscape$delta_neg_forest <- 0
for(i in 1:nrow(usa_landscape)){ if(usa_landscape$delta_forest[i]>0) usa_landscape$delta_pos_forest[i] <- abs(usa_landscape$delta_forest[i])
if(usa_landscape$delta_forest[i]<=0) usa_landscape$delta_neg_forest[i] <- abs(usa_landscape$delta_forest[i]); print(i)}

usa_landscape$delta_pos_grass <- 0; usa_landscape$delta_neg_grass <- 0
for(i in 1:nrow(usa_landscape)){ if(usa_landscape$delta_grass[i]>0) usa_landscape$delta_pos_grass[i] <- abs(usa_landscape$delta_grass[i])
if(usa_landscape$delta_grass[i]<=0) usa_landscape$delta_neg_grass[i] <- abs(usa_landscape$delta_grass[i]); print(i)}

usa_landscape$delta_pos_crop <- 0; usa_landscape$delta_neg_crop <- 0
for(i in 1:nrow(usa_landscape)){ if(usa_landscape$delta_crop[i]>0) usa_landscape$delta_pos_crop[i] <- abs(usa_landscape$delta_crop[i])
if(usa_landscape$delta_crop[i]<=0) usa_landscape$delta_neg_crop[i] <- abs(usa_landscape$delta_crop[i]); print(i)}

usa_landscape$delta_pos_wet <- 0; usa_landscape$delta_neg_wet <- 0
for(i in 1:nrow(usa_landscape)){ if(usa_landscape$delta_wet[i]>0) usa_landscape$delta_pos_wet[i] <- abs(usa_landscape$delta_wet[i])
if(usa_landscape$delta_wet[i]<=0) usa_landscape$delta_neg_wet[i] <- abs(usa_landscape$delta_wet[i]); print(i)}

#usa_landscape$delta_pos_temp <- 0; usa_landscape$delta_neg_temp <- 0
#for(i in 1:N){ if(delta_temp[i]>0) delta_pos_temp[i] <- abs(delta_temp[i])
#if(delta_temp[i]<=0) delta_neg_temp[i] <- abs(delta_temp[i]); print(i)}

#usa_landscape$delta_pos_prec <- 0; usa_landscape$delta_neg_prec <- 0
#for(i in 1:N){ if(delta_prec[i]>0) delta_pos_prec[i] <- abs(delta_prec[i])
#if(delta_prec[i]<=0) delta_neg_prec[i] <- abs(delta_prec[i]); print(i)}

#usa_landscape$delta_pos_lc_q1 <- 0; usa_landscape$delta_neg_lc_q1 <- 0
#for(i in 1:N){ if(delta_lc_q1[i]>0) delta_pos_lc_q1[i] <- abs(delta_lc_q1[i])
#if(delta_lc_q1[i]<=0) delta_neg_lc_q1[i] <- abs(delta_lc_q1[i]); print(i)}

#'-----------------------------------------------------------------------------------------------------
#'* log data same way as in the model *

usa_landscape$urban_t2 <- log(usa_landscape$urban_t2+1)
usa_landscape$forest_t2 <- log(usa_landscape$forest_t2+1) 
usa_landscape$grass_t2 <- log(usa_landscape$grass_t2+1) 
usa_landscape$crop_t2 <- log(usa_landscape$crop_t2+1) 
usa_landscape$wet_t2 <- log(usa_landscape$wet_t2+1) 

usa_landscape$urban_t1 <- log(usa_landscape$urban_t1+1)
usa_landscape$forest_t1 <- log(usa_landscape$forest_t1+1) 
usa_landscape$grass_t1 <- log(usa_landscape$grass_t1+1) 
usa_landscape$crop_t1 <- log(usa_landscape$crop_t1+1) 
usa_landscape$wet_t1 <- log(usa_landscape$wet_t1+1) 

#'-----------------------------------------------------------------------------------------------------
#'* add quadratic terms *

usa_landscape$urban_squared_t2 <- usa_landscape$urban_t2 ^ 2 
usa_landscape$forest_squared_t2 <- usa_landscape$forest_t2 ^ 2 
usa_landscape$grass_squared_t2 <- usa_landscape$grass_t2 ^ 2 
usa_landscape$crop_squared_t2 <- usa_landscape$crop_t2 ^ 2 
usa_landscape$wet_squared_t2 <- usa_landscape$wet_t2 ^ 2 
usa_landscape$temp_squared_t2 <- usa_landscape$temp_t2 ^ 2 

usa_landscape$urban_squared_t1 <- usa_landscape$urban_t1 ^ 2 
usa_landscape$forest_squared_t1 <- usa_landscape$forest_t1 ^ 2 
usa_landscape$grass_squared_t1 <- usa_landscape$grass_t1 ^ 2 
usa_landscape$crop_squared_t1 <- usa_landscape$crop_t1 ^ 2 
usa_landscape$wet_squared_t1 <- usa_landscape$wet_t1 ^ 2  

#'-----------------------------------------------------------------------------------------------------
#'* fix some other stuff *

# add time as a constant, mean sampling time for segments
usa_landscape$time <- 330

# remove vars not used in model, save some space
usa_landscape <- usa_landscape %>% dplyr::select( -c("delta_temp", "delta_prec", "temp_sd_t1", "temp_sd_t2", "prec_t1", "prec_sd_t1",                         
                                                     "prec_sd_t2", "elev", "elev_sd", "delta_lc_q1", "temp_t1", "prec_t2",
                                                     "lc_q0_t1", "lc_q0_t2", "delta_lc_q0", "lc_q1_t1", "water_t1",        
                                                     "water_t2", "delta_water", "barren_t1", "barren_t2", "delta_barren"))

save(usa_landscape, file = "USBBS_data/map_predict_data/USA_landscape_predict_ready.rda")





