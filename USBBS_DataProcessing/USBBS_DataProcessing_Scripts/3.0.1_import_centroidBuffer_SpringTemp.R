#'
#'*All csv files containing the extracted data are present in the repository in*
#'*USBBS_Biodiversity_LandCover_Delays/USBBS_DataProcessing/buffer_extracted_data*
#'
#'*LC data extracted through the zonal histogram geoprocessing tool*
#'*LC data extracted through the zonal statistics geoprocessing tool, returning the mean values of all raster pixels falling within the buffer*
#'

library(tidyverse)
library(corrplot)
library(RColorBrewer)

#'#####################################################################################################
#'
#' *LANDCOVER processing*
#'
#'raw LC data was extracted from the NCDL product for the whole USA
#'for the years 2001 and 2016 corresponding with the same location of the biodiversity data analysed
#'
#'Data format presented is the amount of pixels belonging to each land cover class 
#'within each segment/community buffer, this comes from zonal histogram tool in QGIS
#'
#'4 km round buffer around each survey route segment
#'remember each route is 50 point count over 25 miles, each segment is 5miles and 10 point count, 5 segments per route 
#'3km is average mean dispersal distance for bird and route is 8km total
#'

# import the raw data csv files
lc.01 <- read.csv("USBBS_DataProcessing/buffer_extracted_data/centroid_springTemp/USBBS_centroid_4km_buffer_LC_01.csv")[,-c(1:3)]
lc.16 <- read.csv("USBBS_DataProcessing/buffer_extracted_data/centroid_springTemp/USBBS_centroid_4km_buffer_LC_16.csv")[,-c(1:3)]

LandCover.categories <- c("partition", "id", "null", "water", "icesnow", 
                          "urban0to20", "urban20to49", "urban50to79", "urban80to100",
                          "barrenland", "deciduousforest", "evergreenforest", "mixedforest",
                          "shrubland", "grasslandherbaceous", "pasture", "cropland", "woodywetlands", "herbaceouswetlands")

colnames(lc.01) <- LandCover.categories
colnames(lc.16) <- LandCover.categories

df.list <- list(lc.01, lc.16)

clean.fun <- function(x){ # setup function to clean the land cover  datasets

  x$totpixels <- rowSums(x[3:ncol(x)]) # calculate total pixels and proportions
  
  for (i in 3:ncol(x)) {
    x[,i] <- x[,i]/x$totpixels * 100
  } 
  
  # remove buffer overlapping areas of no data
  # group up subclasses of land cover categories
  x %>%  
        transmute(partition = partition,
                  id=id,
                  water = water,
                  icesnow = icesnow,
                  urban = urban0to20 + urban20to49 + urban50to79 + urban80to100,
                  urban0to20 = urban0to20,
                  urban20to49 = urban20to49,
                  urban50to79 = urban50to79, 
                  urban80to100 = urban80to100,
                  forest = deciduousforest + evergreenforest + mixedforest,
                  deciduousforest = deciduousforest,
                  evergreenforest = evergreenforest,
                  mixedforest = mixedforest,
                  grassland = grasslandherbaceous + pasture + shrubland,
                  grasslandherbaceous = grasslandherbaceous,
                  pasture = pasture,
                  shrubland = shrubland,
                  cropland = cropland,
                  wetland = woodywetlands + herbaceouswetlands,
                  woodywetlands = woodywetlands,
                  herbaceouswetlands = herbaceouswetlands,
                  barrenland = barrenland)
}

df.list <- lapply(df.list, clean.fun) # apply the cleaning function to all the datasets
 
lc.01 <- df.list[[1]] %>% mutate_if(is.factor, as.character) # reassign datasets
lc.16 <- df.list[[2]] %>% mutate_if(is.factor, as.character)

# rename vars with ending either t1 or t2
names(lc.01)[3:ncol(lc.01)] <- paste0(names(lc.01)[3:ncol(lc.01)], ".t1")
names(lc.16)[3:ncol(lc.16)] <- paste0(names(lc.16)[3:ncol(lc.16)], ".t2")

#'#####################################################################################################
#'
#' *Climate processing*
#'
#' *mean annual temperature MAT* and *mean annual precipitation MAP* as average across year 2000-2001 and 2015-2016 
#' data extracted from the PRISM climatic open access datasets
#'
#' *Annual Heat:Moisture index* climate variable, calculated as follows: ((MAT+10)/(MAP/1000)).

climate <- read.csv("USBBS_DataProcessing/buffer_extracted_data/centroid_springTemp/USBBS_centroid_4km_buffer_Climate.csv")[,-c(1:3)]
names(climate)
climate <- climate %>%
            transmute(partition=U_S_R_I,
                      id=id,
                      #rain.t1 = (ppt00_mean + ppt01_mean)/2,
                      #rain.t2 = (ppt15_mean + ppt16_mean)/2,
                      temp.t1 = ((t00m_mean + t00j_mean)/2 + (t01m_mean + t01j_mean)/2)/2 ,
                      temp.t2 = ((t15m_mean + t15j_mean)/2 + (t16m_mean + t16j_mean)/2)/2
                      #heatmoist.t1 = ((/2+10)/(ppt00_mean/1000) + (tmean01_me+10)/(ppt01_mean/1000))/2,
                      #heatmoist.t2 = ((tmean15_me+10)/(ppt15_mean/1000) + (tmean16_me+10)/(ppt16_mean/1000))/2
                      )
glimpse(climate)

plot(climate$rain.t1, climate$temp.t1)
plot(climate$rain.t2, climate$temp.t2)
plot(climate$heatmoist.t1, climate$temp.t1)
plot(climate$heatmoist.t2, climate$temp.t2)
plot(climate$heatmoist.t1, climate$rain.t1)
plot(climate$heatmoist.t2, climate$rain.t2)

###################################################################################
#'
#' *merge the datasets*
#'

covariates <- merge(lc.01, lc.16[,-1], by="id")
covariates <- merge(covariates, climate[,-1], by="id")

# check correlation matrix
corr.matrix <- cor(covariates[3:ncol(covariates)])
cor.test <- cor.mtest(corr.matrix, conf.level = .95)
diag(corr.matrix) <- NA # set diagonal to 0 instead of 1 for plotting reasons
corrplot(corr.matrix, method = "color",  
                    na.label = ".", #set symbols for diagonal easier to visualize
                    col = brewer.pal(n = 10, name = "RdBu"), # set color blue and red, positive negative
                    p.mat = cor.test$p, insig="blank") # assign significance values and set insignificant to be white

covariates[3:ncol(covariates)] <- round(covariates[3:ncol(covariates)], 2) # round them up  

###################################################################################
#'
#' *calculate deltas*
#'

covariates <- covariates %>%
                mutate(delta.urban = urban.t2 - urban.t1,
                       delta.forest = forest.t2 - forest.t1,
                       delta.grassland = grassland.t2 - grassland.t1,
                       delta.cropland = cropland.t2 - cropland.t1,
                       delta.wetland = wetland.t2 - wetland.t1,
                       delta.temp = temp.t2 - temp.t1,
                       #delta.rain = rain.t2 - rain.t1,
                       #delta.heatmoist = heatmoist.t2 - heatmoist.t1
                       )

save(covariates, file="USBBS_DataProcessing/covariates.centroid_springtemp.rda") # save as rda files




