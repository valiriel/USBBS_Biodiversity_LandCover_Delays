#'
#' *order segments and buffers according to the distance from the starting point position*
#'
#' *point takes closest route segment U_S_R_I & id code, using GIS join by nearest neighbors *

library(tidyverse)
library(rgdal)

# import the covariates df
load("USBBS_DataProcessing/covariates.rda")

######################################################################################################
#'
#'* import Unique State ROute IDentifier code and id of closest segment for each starting point*
#'
lc.segm.start <- read.csv("USBBS_DataProcessing/usbbs.routes.start.id.csv")[,-c(1,2)] %>% transmute(partition=U_S_R_I,
                                                                                                   id = j_id)
glimpse(lc.segm.start)
glimpse(covariates)

# extract partition name for loop
partition.names <- lc.segm.start$partition %>% as.character()

#set a saved up df
df.og <- covariates

# initialize dataframe
df.fixed <- data.frame()
partition.now <- partition.names[1]

for(partition.now in partition.names){
  
  # subset dataset to only one route at time
  df.now <- df.og %>% filter(partition == partition.now)
  
  # pick which unique id is the starting point of the route
  startpoint.now <- lc.segm.start %>% filter(partition == partition.now) %>% 
    select(id) %>% as.numeric()
  
  # set new route order based on starting point data
  if(df.now[1,"id"] == startpoint.now || df.now[2,"id"] == startpoint.now )
    df.now$partition <- paste(partition.now, 1:5, sep = "_")
  
  if(df.now[5,"id"] == startpoint.now || df.now[4,"id"] == startpoint.now )
    df.now$partition <- paste(partition.now, 5:1, sep = "_") #else # catch in case something went wrong
  #stop(warning(print("error in order, either first or last segments are not the first in the actual order")))
  
  # add up df onto ordered routes one
  if(ncol(df.now) >2)
    df.fixed <- rbind(df.fixed, df.now)
}

climate.buffer.springtemp <- df.fixed 
save(climate.buffer.springtemp, file="USBBS_DataProcessing/climate.buffer.springtemp.rda", row.names = F)
