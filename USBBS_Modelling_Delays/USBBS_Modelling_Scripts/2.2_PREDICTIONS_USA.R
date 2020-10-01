library(tidyverse)
library(rgdal)
library(mapview)
library(coda)

# Import shapefiles grid
usa.lc.t1 <- readOGR("D:/USBBS_DATA/USBBS_LandCover/USA_Example/USA_LC1.shp")
usa.lc.t2 <- readOGR("D:/USBBS_DATA/USBBS_LandCover/USA_Example/USA_LC2.shp")
usa.lc.temp <- readOGR("D:/USBBS_DATA/USBBS_LandCover/USA_Example/USA_climate.shp")

#'###############################################################################################
#'
#' *Calculate LC predictions*
#'

head(usa.lc.t2@data)

LandCover.categories <- c("partition", "null", "water", "icesnow", 
                          "urban0to20", "urban20to49", "urban50to79", "urban80to100",
                          "barrenland", "deciduousforest", "evergreenforest", "mixedforest",
                          "shrubland", "grassland", "pasture", "cropland", "woodywetlands", "herbaceouswetlands")

colnames(usa.lc.t1@data)[-(1:4)] <- LandCover.categories
colnames(usa.lc.t2@data)[-(1:4)] <- LandCover.categories

unique(usa.lc.t1@data$partition)

#
# setup function to clean the datasets

clean.fun <- function(x){ 
  
  x$totpixels <- rowSums(x[2:ncol(x)]) # calculate total pixels and proportions
  
  for (i in 2:ncol(x)) {
    
    x[,i] <- x[,i]/x$totpixels * 100
    
  } 
  
  # remove areas of no data
  # group up subclasses of land cover categories
  x %>% 
    transmute(partition = partition,
              water = water,
              urban = urban0to20 + urban20to49 + urban50to79 + urban80to100,
              forest = deciduousforest + evergreenforest + mixedforest,
              grassland = grassland + pasture + shrubland,
              cropland = cropland,
              wetland = woodywetlands + herbaceouswetlands,
              barrenland=barrenland)
  
  
}

clean.lc1 <- clean.fun(usa.lc.t1@data[,-(1:4)])
colnames(clean.lc1)[-1] <- paste0(colnames(clean.lc1)[-1], ".t1")
usa.lc.t1@data <- cbind(usa.lc.t1@data[,1:4], clean.lc1)

clean.lc2 <- clean.fun(usa.lc.t2@data[,-(1:4)])
colnames(clean.lc2)[-1] <- paste0(colnames(clean.lc2)[-1], ".t2")
usa.lc.t2@data <- cbind(usa.lc.t2@data[,1:4], clean.lc2)

usa.lc <- usa.lc.t1

usa.lc.t2@data <- usa.lc.t2@data %>% select(-top, -bottom, -right, -left, -partition)

usa.lc@data <- cbind(usa.lc@data, usa.lc.t2@data)

head(usa.lc@data)

#############################################################################
#'
#' *Calculate mean temperature in t2*
#'

head(usa.lc.temp@data)

usa.lc.temp@data <- usa.lc.temp@data %>% select(tmean.t1, tmean.t2)

usa.lc@data <- cbind(usa.lc@data, usa.lc.temp@data)

head(usa.lc@data)

rm(usa.lc.t1, usa.lc.t2, usa.lc.temp, clean.lc1, clean.lc2, LandCover.categories, clean.fun)

#'###############################################################################################
#'###############################################################################################
#'
#' *Calculate predicted species richness*
#'

N <- nrow(usa.lc@data)

###################################################################
#'
#'*load model result*
#'
#load("USBBS_Modelling_Delays/USBBS_models_jags/results.q0.rda")
load("USBBS_Modelling_Delays/USBBS_models_jags/results.q0.centroid.springtemp.rda")
results <- results.q0.centroidspringtemp

###################################################################
#'
#'*Calculate delta for land covers*
#'

usa.lc@data$delta.urban <- usa.lc@data$urban.t2 - usa.lc@data$urban.t1
# setup vectors
usa.lc@data$delta.pos.urban <- vector("numeric",length = N)
usa.lc@data$delta.neg.urban <- vector("numeric",length = N)
#delta.null <- vector("numeric",length = N)

for(i in 1:N){
  
  if(usa.lc@data$delta.urban[i]>0)
    usa.lc@data$delta.pos.urban[i] <- abs(usa.lc@data$delta.urban[i])
  if(usa.lc@data$delta.urban[i]<=0)
    usa.lc@data$delta.neg.urban[i] <- abs(usa.lc@data$delta.urban[i])
}

#####
usa.lc@data$delta.forest <- usa.lc@data$forest.t2 - usa.lc@data$forest.t1
# setup vectors
usa.lc@data$delta.pos.forest<- vector("numeric",length = N)
usa.lc@data$delta.neg.forest <- vector("numeric",length = N)
#delta.null <- vector("numeric",length = N)

for(i in 1:N){
  
  if(usa.lc@data$delta.forest[i]>0)
    usa.lc@data$delta.pos.forest[i] <- abs(usa.lc@data$delta.forest[i])
  if(usa.lc@data$delta.forest[i]<=0)
    usa.lc@data$delta.neg.forest[i] <- abs(usa.lc@data$delta.forest[i])
}

###########
usa.lc@data$delta.grassland <- usa.lc@data$grassland.t2 - usa.lc@data$grassland.t1
# setup vectors
usa.lc@data$delta.pos.grassland <- vector("numeric",length = N)
usa.lc@data$delta.neg.grassland <- vector("numeric",length = N)
#delta.null <- vector("numeric",length = N)

for(i in 1:N){
  
  if(usa.lc@data$delta.grassland[i]>0)
    usa.lc@data$delta.pos.grassland[i] <- abs(usa.lc@data$delta.grassland[i])
  if(usa.lc@data$delta.grassland[i]<=0)
    usa.lc@data$delta.neg.grassland[i] <- abs(usa.lc@data$delta.grassland[i])
}

############
usa.lc@data$delta.cropland <- usa.lc@data$cropland.t2 - usa.lc@data$cropland.t1
# setup vectors
usa.lc@data$delta.pos.cropland <- vector("numeric",length = N)
usa.lc@data$delta.neg.cropland <- vector("numeric",length = N)
#delta.null <- vector("numeric",length = N)

for(i in 1:N){
  
  if(usa.lc@data$delta.cropland[i]>0)
    usa.lc@data$delta.pos.cropland[i] <- abs(usa.lc@data$delta.cropland[i])
  if(usa.lc@data$delta.cropland[i]<=0)
    usa.lc@data$delta.neg.cropland[i] <- abs(usa.lc@data$delta.cropland[i])
}


usa.lc@data$delta.wetland <- usa.lc@data$wetland.t2 - usa.lc@data$wetland.t1

# setup vectors
usa.lc@data$delta.pos.wetland <- vector("numeric",length = N)
usa.lc@data$delta.neg.wetland <- vector("numeric",length = N)
#delta.null <- vector("numeric",length = N)

for(i in 1:N){
  
  if(usa.lc@data$delta.wetland[i]>0)
    usa.lc@data$delta.pos.wetland[i] <- abs(usa.lc@data$delta.wetland[i])
  if(usa.lc@data$delta.wetland[i]<=0)
    usa.lc@data$delta.neg.wetland[i] <- abs(usa.lc@data$delta.wetland[i])
}

############
usa.lc@data$delta.temp <- usa.lc@data$tmean.t2 - usa.lc@data$tmean.t1
usa.lc@data$delta.temp[is.na(usa.lc@data$delta.temp)] <- 0 # there's like 10 nas from data extraction processing

# setup vectors
usa.lc@data$delta.pos.temp <- vector("numeric",length = N)
usa.lc@data$delta.neg.temp <- vector("numeric",length = N)
#delta.null <- vector("numeric",length = N)

for(i in 1:N){
  
  if(usa.lc@data$delta.temp[i]>0)
    usa.lc@data$delta.pos.temp[i] <- abs(usa.lc@data$delta.temp[i])
  if(usa.lc@data$delta.temp[i]<=0)
    usa.lc@data$delta.neg.temp[i] <- abs(usa.lc@data$delta.temp[i])
}


# add evenness as mean across survey points
load("data.centroid.springtemp.rda")
usa.lc@data$even.t1 <- mean(data$even.t1)

###################################################################
#'
#'*Generate prediction by applying model to USA wide data*
#'
#'
###################################################################
#'
#'*SETUP PARAMETERS*
#'
#'*they are all symmetric so can just use param mean/mode*
#'

# extract mcmc from result jags file
mcmc.final <- mcmc(results$BUGSoutput$sims.list)

# function to calculate mode as R does not have a base one apparently
estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}

# retain mode of each parameter mcmc chain

intercept = estimate_mode(mcmc.final$a)
urban.linear = estimate_mode(mcmc.final$aUrban.t2)
  urban.quadratic = estimate_mode(mcmc.final$aUrbanSquared.t2)
  forest.linear = estimate_mode(mcmc.final$aForest.t2)
  forest.quadratic = estimate_mode(mcmc.final$aForestSquared.t2)
  grassland.linear = estimate_mode(mcmc.final$aGrassland.t2)
  grassland.quadratic = estimate_mode(mcmc.final$aGrasslandSquared.t2)
  cropland.linear = estimate_mode(mcmc.final$aCropland.t2)
  cropland.quadratic = estimate_mode(mcmc.final$aCroplandSquared.t2)
  wetland.linear = estimate_mode(mcmc.final$aWetland.t2)
  wetland.quadratic = estimate_mode(mcmc.final$aWetlandSquared.t2)
  temperature.linear = estimate_mode(mcmc.final$aTemperature.t2)
  temperature.quadratic = estimate_mode(mcmc.final$aTemperatureSquared.t2)
urbanforest.interaction = estimate_mode(mcmc.final$aUrbanForest.t2)
  urbangrassland.interaction = estimate_mode(mcmc.final$aUrbanGrassland.t2)
  urbancropland.interaction = estimate_mode(mcmc.final$aUrbanCropland.t2)
  forestgrassland.interaction = estimate_mode(mcmc.final$aForestGrassland.t2)
  forestcropland.interaction = estimate_mode(mcmc.final$aForestCropland.t2)
  grasslandcropland.interaction = estimate_mode(mcmc.final$aGrasslandCropland.t2)
  wetlandurban.interaction = estimate_mode(mcmc.final$aWetlandUrban.t2)
  wetlandgrassland.interaction = estimate_mode(mcmc.final$aWetlandGrassland.t2)
  wetlandforest.interaction = estimate_mode(mcmc.final$aWetlandForest.t2)
  wetlandcropland.interaction = estimate_mode(mcmc.final$aWetlandCropland.t2)
urbansquaredforest.interaction = estimate_mode(mcmc.final$aUrbanSquaredForest.t2)
  urbansquaredgrassland.interaction = estimate_mode(mcmc.final$aUrbanSquaredGrassland.t2) 
  urbansquaredcropland.interaction = estimate_mode(mcmc.final$aUrbanSquaredCropland.t2) 
  urbansquaredwetland.interaction = estimate_mode(mcmc.final$aUrbanSquaredWetland.t2)
forestsquaredurban.interaction = estimate_mode(mcmc.final$aForestSquaredUrban.t2) 
  forestsquaredcropland.interaction = estimate_mode(mcmc.final$aForestSquaredCropland.t2) 
  forestsquaredgrassland.interaction = estimate_mode(mcmc.final$aForestSquaredGrassland.t2) 
  forestsquaredwetland.interaction = estimate_mode(mcmc.final$aForestSquaredWetland.t2) 
grasslandsquaredurban.interaction = estimate_mode(mcmc.final$aGrasslandSquaredUrban.t2) 
  grasslandsquaredforest.interaction = estimate_mode(mcmc.final$aGrasslandSquaredForest.t2) 
  grasslandsquaredcropland.interaction = estimate_mode(mcmc.final$aGrasslandSquaredCropland.t2) 
  grasslandsquaredwetland.interaction = estimate_mode(mcmc.final$aGrasslandSquaredWetland.t2)
croplandsquaredurban.interaction = estimate_mode(mcmc.final$aCroplandSquaredUrban.t2) 
  croplandsquaredforest.interaction = estimate_mode(mcmc.final$aCroplandSquaredForest.t2) 
  croplandsquaredgrassland.interaction = estimate_mode(mcmc.final$aCroplandSquaredGrassland.t2) 
  croplandsquaredwetland.interaction = estimate_mode(mcmc.final$aCroplandSquaredWetland.t2) 
wetlandsquaredurban.interaction = estimate_mode(mcmc.final$aWetlandSquaredUrban.t2) 
  wetlandsquaredforest.interaction = estimate_mode(mcmc.final$aWetlandSquaredForest.t2) 
  wetlandsquaredgrassland.interaction = estimate_mode(mcmc.final$aWetlandSquaredGrassland.t2) 
  wetlandsquaredcropland.interaction = estimate_mode(mcmc.final$aWetlandSquaredCropland.t2)
evenness.linear = estimate_mode(mcmc.final$aEven.t1)

c.pos.urban = estimate_mode(mcmc.final$c.pos.urban)
c.pos.forest = estimate_mode(mcmc.final$c.pos.forest)
c.neg.forest = estimate_mode(mcmc.final$c.neg.forest)
c.pos.grassland = estimate_mode(mcmc.final$c.pos.grassland)
c.neg.grassland = estimate_mode(mcmc.final$c.neg.grassland)
c.pos.cropland = estimate_mode(mcmc.final$c.pos.cropland)
c.neg.cropland = estimate_mode(mcmc.final$c.neg.cropland)
c.pos.temp = estimate_mode(mcmc.final$c.pos.temp)
c.neg.temp = estimate_mode(mcmc.final$c.neg.temp)
c.pos.wetland = estimate_mode(mcmc.final$c.pos.wetland)
c.neg.wetland = estimate_mode(mcmc.final$c.neg.wetland)

attach(usa.lc@data)

for( i in 1:N){
  
  usa.lc@data$q0.eq[i] <- exp(intercept + 
                                urban.linear*log(urban.t2[i]+1) + urban.quadratic*log(urban.t2[i]+1)^2 +  
                                   forest.linear*log(forest.t2[i]+1) + forest.quadratic*log(forest.t2[i]+1)^2 +
                                   grassland.linear*log(grassland.t2[i]+1) + grassland.quadratic*log(grassland.t2[i]+1)^2 + 
                                   cropland.linear*log(cropland.t2[i]+1) + cropland.quadratic*log(cropland.t2[i]+1)^2 +
                                   wetland.linear*log(wetland.t2[i]+1) + wetland.quadratic*log(wetland.t2[i]+1)^2 +
                                   temperature.linear*tmean.t2[i] + temperature.quadratic*(tmean.t2[i]^2) + 
                                urbanforest.interaction*log(urban.t2[i]+1)*log(forest.t2[i]+1) + 
                                   urbangrassland.interaction*log(urban.t2[i]+1)*log(grassland.t2[i]+1) +
                                   urbancropland.interaction*log(urban.t2[i]+1)*log(cropland.t2[i]+1) +
                                   forestgrassland.interaction*log(forest.t2[i]+1)*log(grassland.t2[i]+1) + 
                                   forestcropland.interaction*log(forest.t2[i]+1)*log(cropland.t2[i]+1) +
                                   grasslandcropland.interaction*log(grassland.t2[i]+1)*log(cropland.t2[i]+1) +
                                   wetlandurban.interaction*log(wetland.t2[i]+1)*log(urban.t2[i]+1) +
                                   wetlandgrassland.interaction*log(wetland.t2[i]+1)*log(grassland.t2[i]+1) +
                                   wetlandforest.interaction*log(wetland.t2[i]+1)*log(forest.t2[i]+1) +
                                   wetlandcropland.interaction*log(wetland.t2[i]+1)*log(cropland.t2[i]+1) +
                                urbansquaredforest.interaction*log(urban.t2[i]+1)^2*log(forest.t2[i]+1) +
                                   urbansquaredgrassland.interaction*log(urban.t2[i]+1)^2*log(grassland.t2[i]+1) + 
                                   urbansquaredcropland.interaction*log(urban.t2[i]+1)^2*log(cropland.t2[i]+1) +
                                   urbansquaredwetland.interaction*log(urban.t2[i]+1)^2*log(wetland.t2[i]+1) +
                                forestsquaredurban.interaction*log(forest.t2[i]+1)^2*log(urban.t2[i]+1) +
                                   forestsquaredcropland.interaction*log(forest.t2[i]+1)^2*log(cropland.t2[i]+1) + 
                                   forestsquaredgrassland.interaction*log(forest.t2[i]+1)^2*log(grassland.t2[i]+1) + 
                                   forestsquaredwetland.interaction*log(forest.t2[i]+1)^2*log(wetland.t2[i]+1) +
                                grasslandsquaredurban.interaction*log(grassland.t2[i]+1)^2*log(urban.t2[i]+1) +
                                   grasslandsquaredforest.interaction*log(grassland.t2[i]+1)^2*log(forest.t2[i]+1) +
                                   grasslandsquaredcropland.interaction*log(grassland.t2[i]+1)^2*log(cropland.t2[i]+1) + 
                                   grasslandsquaredwetland.interaction*log(grassland.t2[i]+1)^2*log(wetland.t2[i]+1) +
                                croplandsquaredurban.interaction*log(cropland.t2[i]+1)^2*log(urban.t2[i]+1) + 
                                   croplandsquaredforest.interaction*log(cropland.t2[i]+1)^2*log(forest.t2[i]+1) + 
                                   croplandsquaredgrassland.interaction*log(cropland.t2[i]+1)^2*log(cropland.t2[i]+1) + 
                                   croplandsquaredwetland.interaction*log(cropland.t2[i]+1)^2*log(wetland.t2[i]+1) + 
                                wetlandsquaredurban.interaction*log(wetland.t2[i]+1)^2*log(urban.t2[i]+1) +
                                   wetlandsquaredforest.interaction*log(wetland.t2[i]+1)^2*log(forest.t2[i]+1) +  
                                   wetlandsquaredgrassland.interaction*log(wetland.t2[i]+1)^2*log(grassland.t2[i]+1) + 
                                   wetlandsquaredcropland.interaction*log(wetland.t2[i]+1)^2*log(cropland.t2[i]+1) +
                                evenness.linear*even.t1[i])

  usa.lc@data$q0.lag[i] <- exp(intercept + 
                                (urban.linear*log(urban.t2[i]+1) + urban.quadratic*log(urban.t2[i]+1)^2 +  
                                   forest.linear*log(forest.t2[i]+1) + forest.quadratic*log(forest.t2[i]+1)^2 +
                                   grassland.linear*log(grassland.t2[i]+1) + grassland.quadratic*log(grassland.t2[i]+1)^2 + 
                                   cropland.linear*log(cropland.t2[i]+1) + cropland.quadratic*log(cropland.t2[i]+1)^2 +
                                   wetland.linear*log(wetland.t2[i]+1) + wetland.quadratic*log(wetland.t2[i]+1)^2 +
                                   temperature.linear*(tmean.t2[i]) + temperature.quadratic*(tmean.t2[i]^2) +
                                   urbanforest.interaction*log(urban.t2[i]+1)*log(forest.t2[i]+1) + 
                                   urbangrassland.interaction*log(urban.t2[i]+1)*log(grassland.t2[i]+1) +
                                   urbancropland.interaction*log(urban.t2[i]+1)*log(cropland.t2[i]+1) +
                                   forestgrassland.interaction*log(forest.t2[i]+1)*log(grassland.t2[i]+1) + 
                                   forestcropland.interaction*log(forest.t2[i]+1)*log(cropland.t2[i]+1) +
                                   grasslandcropland.interaction*log(grassland.t2[i]+1)*log(cropland.t2[i]+1) +
                                   wetlandurban.interaction*log(wetland.t2[i]+1)*log(urban.t2[i]+1) +
                                   wetlandgrassland.interaction*log(wetland.t2[i]+1)*log(grassland.t2[i]+1) +
                                   wetlandforest.interaction*log(wetland.t2[i]+1)*log(forest.t2[i]+1) +
                                   wetlandcropland.interaction*log(wetland.t2[i]+1)*log(cropland.t2[i]+1) +
                                   urbansquaredforest.interaction*log(urban.t2[i]+1)^2*log(forest.t2[i]+1) +
                                   urbansquaredgrassland.interaction*log(urban.t2[i]+1)^2*log(grassland.t2[i]+1) + 
                                   urbansquaredcropland.interaction*log(urban.t2[i]+1)^2*log(cropland.t2[i]+1) +
                                   urbansquaredwetland.interaction*log(urban.t2[i]+1)^2*log(wetland.t2[i]+1) +
                                   forestsquaredurban.interaction*log(forest.t2[i]+1)^2*log(urban.t2[i]+1) +
                                   forestsquaredcropland.interaction*log(forest.t2[i]+1)^2*log(cropland.t2[i]+1) + 
                                   forestsquaredgrassland.interaction*log(forest.t2[i]+1)^2*log(grassland.t2[i]+1) + 
                                   forestsquaredwetland.interaction*log(forest.t2[i]+1)^2*log(wetland.t2[i]+1) +
                                   grasslandsquaredurban.interaction*log(grassland.t2[i]+1)^2*log(urban.t2[i]+1) +
                                   grasslandsquaredforest.interaction*log(grassland.t2[i]+1)^2*log(forest.t2[i]+1) +
                                   grasslandsquaredcropland.interaction*log(grassland.t2[i]+1)^2*log(cropland.t2[i]+1) + 
                                   grasslandsquaredwetland.interaction*log(grassland.t2[i]+1)^2*log(wetland.t2[i]+1) +
                                   croplandsquaredurban.interaction*log(cropland.t2[i]+1)^2*log(urban.t2[i]+1) + 
                                   croplandsquaredforest.interaction*log(cropland.t2[i]+1)^2*log(forest.t2[i]+1) + 
                                   croplandsquaredgrassland.interaction*log(cropland.t2[i]+1)^2*log(cropland.t2[i]+1) + 
                                   croplandsquaredwetland.interaction*log(cropland.t2[i]+1)^2*log(wetland.t2[i]+1) + 
                                   wetlandsquaredurban.interaction*log(wetland.t2[i]+1)^2*log(urban.t2[i]+1) +
                                   wetlandsquaredforest.interaction*log(wetland.t2[i]+1)^2*log(forest.t2[i]+1) +  
                                   wetlandsquaredgrassland.interaction*log(wetland.t2[i]+1)^2*log(grassland.t2[i]+1) + 
                                   wetlandsquaredcropland.interaction*log(wetland.t2[i]+1)^2*log(cropland.t2[i]+1))*
                                exp(-c.pos.forest*delta.pos.forest[i] + -c.pos.urban*delta.pos.urban[i] + 
                                      -c.pos.grassland*delta.pos.grassland[i] + -c.pos.cropland*delta.pos.cropland[i] + -c.pos.temp*delta.pos.temp[i] +
                                      -c.neg.forest*delta.neg.forest[i] + -c.neg.grassland*delta.neg.grassland[i] + 
                                      -c.neg.cropland*delta.neg.cropland[i] + -c.neg.temp*delta.neg.temp[i] +
                                      -c.pos.wetland*delta.pos.wetland[i] + -c.neg.wetland*delta.neg.wetland[i]) +
                                (urban.linear*log(urban.t1[i]+1) + urban.quadratic*log(urban.t1[i]+1)^2 +  
                                   forest.linear*log(forest.t1[i]+1) + forest.quadratic*log(forest.t1[i]+1)^2 +
                                   grassland.linear*log(grassland.t1[i]+1) + grassland.quadratic*log(grassland.t1[i]+1)^2 + 
                                   cropland.linear*log(cropland.t1[i]+1) + cropland.quadratic*log(cropland.t1[i]+1)^2 +
                                   wetland.linear*log(wetland.t1[i]+1) + wetland.quadratic*log(wetland.t1[i]+1)^2 +
                                   temperature.linear*tmean.t1[i] + temperature.quadratic*(tmean.t1[i]^2) +
                                   urbanforest.interaction*log(urban.t1[i]+1)*log(forest.t1[i]+1) + 
                                   urbangrassland.interaction*log(urban.t1[i]+1)*log(grassland.t1[i]+1) +
                                   urbancropland.interaction*log(urban.t1[i]+1)*log(cropland.t1[i]+1) +
                                   forestgrassland.interaction*log(forest.t1[i]+1)*log(grassland.t1[i]+1) + 
                                   forestcropland.interaction*log(forest.t1[i]+1)*log(cropland.t1[i]+1) +
                                   grasslandcropland.interaction*log(grassland.t1[i]+1)*log(cropland.t1[i]+1) +
                                   wetlandurban.interaction*log(wetland.t1[i]+1)*log(urban.t1[i]+1) +
                                   wetlandgrassland.interaction*log(wetland.t1[i]+1)*log(grassland.t1[i]+1) +
                                   wetlandforest.interaction*log(wetland.t1[i]+1)*log(forest.t1[i]+1) +
                                   wetlandcropland.interaction*log(wetland.t1[i]+1)*log(cropland.t1[i]+1) +
                                   urbansquaredforest.interaction*log(urban.t1[i]+1)^2*log(forest.t1[i]+1) +
                                   urbansquaredgrassland.interaction*log(urban.t1[i]+1)^2*log(grassland.t1[i]+1) + 
                                   urbansquaredcropland.interaction*log(urban.t1[i]+1)^2*log(cropland.t1[i]+1) +
                                   urbansquaredwetland.interaction*log(urban.t1[i]+1)^2*log(wetland.t1[i]+1) +
                                   forestsquaredurban.interaction*log(forest.t1[i]+1)^2*log(urban.t1[i]+1) +
                                   forestsquaredcropland.interaction*log(forest.t1[i]+1)^2*log(cropland.t1[i]+1) + 
                                   forestsquaredgrassland.interaction*log(forest.t1[i]+1)^2*log(grassland.t1[i]+1) + 
                                   forestsquaredwetland.interaction*log(forest.t1[i]+1)^2*log(wetland.t1[i]+1) +
                                   grasslandsquaredurban.interaction*log(grassland.t1[i]+1)^2*log(urban.t1[i]+1) +
                                   grasslandsquaredforest.interaction*log(grassland.t1[i]+1)^2*log(forest.t1[i]+1) +
                                   grasslandsquaredcropland.interaction*log(grassland.t1[i]+1)^2*log(cropland.t1[i]+1) + 
                                   grasslandsquaredwetland.interaction*log(grassland.t1[i]+1)^2*log(wetland.t1[i]+1) +
                                   croplandsquaredurban.interaction*log(cropland.t1[i]+1)^2*log(urban.t1[i]+1) + 
                                   croplandsquaredforest.interaction*log(cropland.t1[i]+1)^2*log(forest.t1[i]+1) + 
                                   croplandsquaredgrassland.interaction*log(cropland.t1[i]+1)^2*log(cropland.t1[i]+1) + 
                                   croplandsquaredwetland.interaction*log(cropland.t1[i]+1)^2*log(wetland.t1[i]+1) + 
                                   wetlandsquaredurban.interaction*log(wetland.t1[i]+1)^2*log(urban.t1[i]+1) +
                                   wetlandsquaredforest.interaction*log(wetland.t1[i]+1)^2*log(forest.t1[i]+1) +  
                                   wetlandsquaredgrassland.interaction*log(wetland.t1[i]+1)^2*log(grassland.t1[i]+1) + 
                                   wetlandsquaredcropland.interaction*log(wetland.t1[i]+1)^2*log(cropland.t1[i]+1))*
                                (1 - exp(-c.pos.forest*delta.pos.forest[i] + -c.pos.urban*delta.pos.urban[i] + 
                                           -c.pos.grassland*delta.pos.grassland[i] + -c.pos.cropland*delta.pos.cropland[i] + 
                                           -c.neg.forest*delta.neg.forest[i] + -c.pos.temp*delta.pos.temp[i] +
                                           -c.neg.grassland*delta.neg.grassland[i] + -c.neg.cropland*delta.neg.cropland[i] + -c.neg.temp*delta.neg.temp[i] +
                                           -c.pos.wetland*delta.pos.wetland[i] + -c.neg.wetland*delta.neg.wetland[i])) +
                                evenness.linear*even.t1[i])
  
}

detach(usa.lc@data)

#calculate overall change,disturbance in the model
usa.lc@data$change <- (abs(usa.lc@data$delta.urban) + abs(usa.lc@data$delta.forest) + abs(usa.lc@data$delta.grassland) + abs(usa.lc@data$delta.cropland) + abs(usa.lc@data$delta.wetland))/2

max(usa.lc@data$change)
min(usa.lc@data$change)

hist(usa.lc@data$q0.eq)
hist(usa.lc@data$q0.lag)

#round up values slightly
usa.lc@data[,6:ncol(usa.lc@data)] <- round(usa.lc@data[,6:ncol(usa.lc@data)],4)

#calculate distance from eq, i.e. extinction debt and colonization credit
usa.lc@data$debtcredit <- usa.lc@data$q0.eq - usa.lc@data$q0.lag
hist(usa.lc@data$debtcredit)

#'###############################################################################################
#'
#' *Save shapefiles for plotting*
#' 

rgdal::writeOGR(usa.lc, dsn="D:/USBBS_DATA/USA_predict_map", 
                layer="USA_predict_centroid_springtemp", driver = "ESRI Shapefile",
                check_exists = FALSE)

a<-data.frame(usa.lc@data)
summary(aa@data$delta_q)
