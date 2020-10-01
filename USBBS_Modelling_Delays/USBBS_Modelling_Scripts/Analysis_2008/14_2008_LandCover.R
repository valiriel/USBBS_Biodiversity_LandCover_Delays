
# Subset routes to the one available in 2008

library(rgdal)
library(tidyverse)
library(mapview)

#'###############################################################################################################
#'
#'*Load in shapefile of 4km buffer with sampled by zonal histogram from 2008 Land Cover dataset*
#'
#' *need to order route segments to add number*

#import routes shp
routes.shp <- readOGR("D:/USBBS_DATA/USBBS_LandCover/NLCD_01_16_4km_BufferHist_Segments/Segment_buffer_4km_08.shp")
temp.shp <- readOGR("USBBS_Modelling_Scripts/one more time/buffer_temp_08.shp")

mapview(routes.shp)

a <- routes.shp@data

routes.shp@data <- merge(routes.shp@data, reduncy, by="partition" )

a<- a[,-c(2:5, 7)]

colnames(a)[2] <- "route"

######################################################################################################
lc.segm.start <- read.csv("D:/GITHUB/USBBS_LandCover/buffer_LC_rawdata/USBBS.segment.start.csv")

if(nlevels(unique(lc.segm.4km.16$route)) != 835)
  stop(warning(print("number of routes not coinciding between starting point segment buffer check")))

route.names <- lc.segm.start$route %>% as.character()

#set the df
df.og <- a

# initialize dataframe
df.fixed <- data.frame()
route.now <- route.names[1]
for(route.now in route.names){
  
  # subset datset to only one route at time
  df.now <- df.og %>% filter(route == route.now)
  
  # pick which unique id is the starting point of the route
  startpoint.now <- lc.segm.start %>% filter(route == route.now) %>% 
    select(start.route) %>% as.numeric()
  
  # set new route order based on starting point data
  if(df.now[1,1] == startpoint.now || df.now[2,1] == startpoint.now )
    df.now$fix.route <- paste(route.now, 1:5)
  
  if(df.now[5,1] == startpoint.now || df.now[4,1] == startpoint.now )
    df.now$fix.route <- paste(route.now, 5:1) #else # catch in case something went wrong
  #stop(warning(print("error in order, either first or last segments are not the first in the actual order")))
  
  if(ncol(df.now) == 20) # switch to 9 for temp df
    df.fixed <- rbind(df.fixed, df.now)
  
}

unique(df.fixed$route) # 835 total roads

df.fixed$route <- df.fixed$fix.route

df.fixed <- df.fixed %>% select(-fid, -fix.route)

write.csv(df.fixed, file = ("USBBS_Modelling_Scripts/one more time/LandCover_08.csv"), row.names = F)

#'
#' *Clean into percentages*
#'

LandCover.categories <- c("partition", "null", "water", "icesnow", 
                          "urban0to20", "urban20to49", "urban50to79", "urban80to100",
                          "barrenland", "deciduousforest", "evergreenforest", "mixedforest",
                          "shrubland", "grassland", "pasture", "cropland", "woodywetlands", "herbaceouswetlands")

colnames(df.fixed) <- LandCover.categories

clean.fun <- function(x){ # setup function to clean the datasets
  
  x$totpixels <- rowSums(x[2:ncol(x)]) # calculate total pixels and proportions
  
  for (i in 2:ncol(x)) {
    
    x[,i] <- x[,i]/x$totpixels * 100
    
  } 
  
  # remove buffer overlapping areas of no data
  # group up subclasses of land cover categories
  x %>% filter(null==0) %>% 
    transmute(partition = partition,
              water = water,
              urban = urban0to20 + urban20to49 + urban50to79 + urban80to100,
              forest = deciduousforest + evergreenforest + mixedforest,
              grassland = grassland + pasture + shrubland,
              cropland = cropland,
              wetland = woodywetlands + herbaceouswetlands)
  
}

LC.t08 <- clean.fun(df.fixed)

sum(rowSums(LC.t08[,-1]) > 101) # check nothing went wrong

str(LC.t08)

# import dataset of alpha diversity for 2008
load("USBBS_Modelling_Scripts/one more time/alpha_08.rda")

data.08 <- merge(alpha.08, LC.t08, by="partition")

save(data.08, file= "USBBS_Modelling_Scripts/one more time/data_08.rda")

#'###################################################################################
#'
#' *Add temperature data from PRISM as mean between May and June across the 3 years*
#'

load("USBBS_Modelling_Scripts/one more time/data_08.rda")

temp.08 <- read.csv("USBBS_Modelling_Scripts/one more time/Temp_08.csv")

temp.08 <- temp.08 %>% transmute(partition = route, # get mean temperature between two months of survey and across the 3 years 
                                 t.08 = ((X_07_mmean + X_07_jmean)/2 + (X_08_mmean + X_08_jmean)/2 + (X_09_mmean + X_09_jmean)/2)/3)

data.08 <- merge(data.08, temp.08, by="partition")

colnames(data.08)[4:9] <- paste(colnames(data.08)[4:9], ".t08", sep="")

save(data.08, file= "USBBS_Modelling_Scripts/one more time/data_08.rda")
