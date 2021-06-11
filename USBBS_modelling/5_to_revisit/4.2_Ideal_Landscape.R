library(tidyverse)
library(coda)

###################################################################
#'
#'*SETUP Landscape Matrix of combinations*
#'
#'*they are all symmetric so can just use param mean/mode*
#'

landscape <- expand.grid(urban = seq(from=0, to=100, by=5), 
                         forest= seq(from=0, to=100, by=5),
                         grassland = seq(from=0, to=100, by=5),
                         cropland = seq(from=0, to=100, by=5),
                         wetland = seq(from=0, to=100, by=5))

landscape <- landscape %>% mutate(valid=rowSums(.)) %>% filter(valid==100)

###################################################################
#'
#'*SETUP MODEL*

load("USBBS_models_jags/results.q0.iter.rda")
summary.m.iter <- as_tibble(results.q0.iter$BUGSoutput$summary, rownames=NA) %>% round(5) %>% rownames_to_column %>% slice(-(50):-(n()-1)) 
summary.m.iter

results <- results.q0.iter

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


###################################################################
#'
#'*Compute Species richness across ideal landscapes*
#'

# set number of observations per each var
N <- nrow(landscape)

# set evenness and temp as overall mean 
even <- 0.8417318
temperature <- 20

attach(landscape)
for( i in 1:N){

landscape$q0[i] <- exp(intercept + 
                         urban.linear*log(urban[i]+1) + urban.quadratic*log(urban[i]+1)^2 +  
                         forest.linear*log(forest[i]+1) + forest.quadratic*log(forest[i]+1)^2 +
                         grassland.linear*log(grassland[i]+1) + grassland.quadratic*log(grassland[i]+1)^2 + 
                         cropland.linear*log(cropland[i]+1) + cropland.quadratic*log(cropland[i]+1)^2 +
                         wetland.linear*log(wetland[i]+1) + wetland.quadratic*log(wetland[i]+1)^2 +
                         temperature.linear*temperature + temperature.quadratic*(temperature^2) + 
                         urbanforest.interaction*log(urban[i]+1)*log(forest[i]+1) + 
                         urbangrassland.interaction*log(urban[i]+1)*log(grassland[i]+1) +
                         urbancropland.interaction*log(urban[i]+1)*log(cropland[i]+1) +
                         forestgrassland.interaction*log(forest[i]+1)*log(grassland[i]+1) + 
                         forestcropland.interaction*log(forest[i]+1)*log(cropland[i]+1) +
                         grasslandcropland.interaction*log(grassland[i]+1)*log(cropland[i]+1) +
                         wetlandurban.interaction*log(wetland[i]+1)*log(urban[i]+1) +
                         wetlandgrassland.interaction*log(wetland[i]+1)*log(grassland[i]+1) +
                         wetlandforest.interaction*log(wetland[i]+1)*log(forest[i]+1) +
                         wetlandcropland.interaction*log(wetland[i]+1)*log(cropland[i]+1) +
                         urbansquaredforest.interaction*log(urban[i]+1)^2*log(forest[i]+1) +
                         urbansquaredgrassland.interaction*log(urban[i]+1)^2*log(grassland[i]+1) + 
                         urbansquaredcropland.interaction*log(urban[i]+1)^2*log(cropland[i]+1) +
                         urbansquaredwetland.interaction*log(urban[i]+1)^2*log(wetland[i]+1) +
                         forestsquaredurban.interaction*log(forest[i]+1)^2*log(urban[i]+1) +
                         forestsquaredcropland.interaction*log(forest[i]+1)^2*log(cropland[i]+1) + 
                         forestsquaredgrassland.interaction*log(forest[i]+1)^2*log(grassland[i]+1) + 
                         forestsquaredwetland.interaction*log(forest[i]+1)^2*log(wetland[i]+1) +
                         grasslandsquaredurban.interaction*log(grassland[i]+1)^2*log(urban[i]+1) +
                         grasslandsquaredforest.interaction*log(grassland[i]+1)^2*log(forest[i]+1) +
                         grasslandsquaredcropland.interaction*log(grassland[i]+1)^2*log(cropland[i]+1) + 
                         grasslandsquaredwetland.interaction*log(grassland[i]+1)^2*log(wetland[i]+1) +
                         croplandsquaredurban.interaction*log(cropland[i]+1)^2*log(urban[i]+1) + 
                         croplandsquaredforest.interaction*log(cropland[i]+1)^2*log(forest[i]+1) + 
                         croplandsquaredgrassland.interaction*log(cropland[i]+1)^2*log(cropland[i]+1) + 
                         croplandsquaredwetland.interaction*log(cropland[i]+1)^2*log(wetland[i]+1) + 
                         wetlandsquaredurban.interaction*log(wetland[i]+1)^2*log(urban[i]+1) +
                         wetlandsquaredforest.interaction*log(wetland[i]+1)^2*log(forest[i]+1) +  
                         wetlandsquaredgrassland.interaction*log(wetland[i]+1)^2*log(grassland[i]+1) + 
                         wetlandsquaredcropland.interaction*log(wetland[i]+1)^2*log(cropland[i]+1) +
                         evenness.linear*even)
}

detach(landscape)


###################################################################
#'
#'*Optimization routine ??????????????????????????????????????????????* 
#'

optim(1,landscape_fun)
 
###################################################################
#'
#'*Visualize ideal landscape*
#'

unique(landscape$urban)

ggplot(landscape) + 
  geom_point(aes(x=urban, y= forest, colour=q0)) 

#shape values into matrix
sr.values <- matrix(landscape$q0, 101, 101)
sqrt(176851)
# environmental space
image.plot(1:100, 1:100, sr.values,
           xlab="% urban cover in 2015", ylab="Evenness in 2000", col=viridis(10))

library(plot3D)

example("contour3D")
plot3D::contour3D()

scatter3D(x=landscape$grassland, y=landscape$forest, z=landscape$q0,
                       type="l")
scatter3D(landscape)
plot3D::contour3D(x=landscape$grassland, y=landscape$forest, z=landscape$q0,
                  type="l")
library(rgl)
rgl::plot3d(x=landscape$grassland, y=landscape$forest, z=landscape$q0)

library(car)

car::scatter3d(q0 ~ urban + forest , data = landscape, 
               revolutions=0, surface=F)



