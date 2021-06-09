#'
#' *order segments and buffers according to the distance from the starting point position*

library(tidyverse); library(rgdal); library(sf); library(nngeo)

# import segments shapefile, clean and add unique id 1:number of segments to match back later
segments <- read_sf("USBBS_data/routes_data/3.1_routes_segmented_cleaned.shp") %>% 
              select(partition=U_S_R_I, name=RTENAME) %>% arrange(partition)
segments$id <- 1:nrow(segments) 

# import starting points
start.points <- read_sf("USBBS_data/routes_data/4_routes_start_points.shp") %>% rename(partition=U_S_R_I) %>% arrange(partition)

mapview(segments) + mapview(start.points)

#'---------------------------------------------------------------------------------------------------------
#'* Spatial join by nearest neighbour, returns list of ids of each feature in y closest to x*

start.points$id <- unlist(st_nn(start.points, segments, parallel = 11)) # change based on your number of cores for parallel

#'---------------------------------------------------------------------------------------------------------
#'* Order segments from 1 to 5 based on where the starting point was*
#' this is needed to match back with the diversity bird counts data

# extract partition names for loop
partition.names <- start.points$partition %>% as.character()

#set a saved up df
df.og <- segments

# initialize dataframe
df.fixed <- data.frame()
partition.now <- partition.names[1]
i<-1
for(partition.now in partition.names){
  
  print(paste0("working on partition = ",partition.now, " which is ", i, "/", length(partition.names), "..."))
  i<- i+1
  
  # subset dataset to only one route at time
  df.now <- df.og %>% filter(partition == partition.now)
  
  # pick which unique id is the starting point of the route
  startpoint.now <- start.points %>% filter(partition == partition.now) %>% as.data.frame() %>%
    select(id) %>% as.numeric()
  
  # set new route order based on starting point data
  if(as.data.frame(df.now)[1,"id"] == startpoint.now || as.data.frame(df.now)[2,"id"] == startpoint.now )
    df.now$partition <- paste(partition.now, 1:5, sep = "_")
  
  if(as.data.frame(df.now)[5,"id"] == startpoint.now || as.data.frame(df.now)[4,"id"] == startpoint.now )
    df.now$partition <- paste(partition.now, 5:1, sep = "_") #else # catch in case something went wrong
  #stop(warning(print("error in order, either first or last segments are not the first in the actual order")))
  
  # add up df onto ordered routes one
  #if(ncol(df.now) >2)
    df.fixed <- rbind(df.fixed, df.now)
}

# save segments from the 2years aggregate subset
segments <- df.fixed %>% select(partition)
write_sf(segments, "USBBS_data/routes_data/5_segments_final_2years.shp")

# save segments from the 3years aggregate subset
load("USBBS_data/diversity_data/3years_x_timepoint/USBBS_years_subset.rda")
segments$route <- substring(segments$partition, 1, 6)
segments <- segments %>% filter(route %in% unique(usbbs.data$U_S_R_I)) %>% select(partition)
write_sf(segments, "USBBS_data/routes_data/5_segments_final_3years.shp")

