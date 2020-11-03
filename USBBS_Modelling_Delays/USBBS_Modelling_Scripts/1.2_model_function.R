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

#'
#'*parameters for delays need to be sampled from posterior as they are not symmetric and can't just take mode*
#'

for( i in 1:N){
  
  i.q0.lag <- c(0)
  
  for (j in 1:1000) {
    
    pos.sample <- round(runif(n=1, min=1, max=as.numeric(nrow(mcmc.final$c.pos.urban))),0)
    
    c.pos.urban = mcmc.final$c.pos.urban[pos.sample]
    c.pos.forest = mcmc.final$c.pos.forest[pos.sample]
    c.neg.forest = mcmc.final$c.neg.forest[pos.sample]
    c.pos.grassland = mcmc.final$c.pos.grassland[pos.sample]
    c.neg.grassland = mcmc.final$c.neg.grassland[pos.sample]
    c.pos.cropland = mcmc.final$c.pos.cropland[pos.sample]
    c.neg.cropland = mcmc.final$c.neg.cropland[pos.sample]
    #c.pos.temp = mcmc.final$c.pos.temp[pos.sample]
    #c.neg.temp = mcmc.final$c.neg.temp[pos.sample]
    c.pos.wetland = mcmc.final$c.pos.wetland[pos.sample]
    c.neg.wetland = mcmc.final$c.neg.wetland[pos.sample]
    
    data$q0.eq[i] <- exp(intercept + 
                          urban.linear*log(urban.t2[i]+1) + urban.quadratic*log(urban.t2[i]+1)^2 +  
                              forest.linear*log(forest.t2[i]+1) + forest.quadratic*log(forest.t2[i]+1)^2 +
                              grassland.linear*log(grassland.t2[i]+1) + grassland.quadratic*log(grassland.t2[i]+1)^2 + 
                              cropland.linear*log(cropland.t2[i]+1) + cropland.quadratic*log(cropland.t2[i]+1)^2 +
                              wetland.linear*log(wetland.t2[i]+1) + wetland.quadratic*log(wetland.t2[i]+1)^2 +
                              temperature.linear*temp.t2[i] + temperature.quadratic*(temp.t2[i]^2) + 
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
    
    i.q0.lag[j] <- exp(intercept + 
                     (urban.linear*log(urban.t2[i]+1) + urban.quadratic*log(urban.t2[i]+1)^2 +  
                          forest.linear*log(forest.t2[i]+1) + forest.quadratic*log(forest.t2[i]+1)^2 +
                          grassland.linear*log(grassland.t2[i]+1) + grassland.quadratic*log(grassland.t2[i]+1)^2 + 
                          cropland.linear*log(cropland.t2[i]+1) + cropland.quadratic*log(cropland.t2[i]+1)^2 +
                          wetland.linear*log(wetland.t2[i]+1) + wetland.quadratic*log(wetland.t2[i]+1)^2 +
                          temperature.linear*(temp.t2[i]) + temperature.quadratic*(temp.t2[i]^2) +
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
                          -c.pos.grassland*delta.pos.grassland[i] + -c.pos.cropland*delta.pos.cropland[i] + #-c.pos.temp*delta.pos.temp[i] +
                          -c.neg.forest*delta.neg.forest[i] + -c.neg.grassland*delta.neg.grassland[i] + 
                          -c.neg.cropland*delta.neg.cropland[i] + #-c.neg.temp*delta.neg.temp[i] +
                          -c.pos.wetland*delta.pos.wetland[i] + -c.neg.wetland*delta.neg.wetland[i]) +
                    (urban.linear*log(urban.t1[i]+1) + urban.quadratic*log(urban.t1[i]+1)^2 +  
                           forest.linear*log(forest.t1[i]+1) + forest.quadratic*log(forest.t1[i]+1)^2 +
                           grassland.linear*log(grassland.t1[i]+1) + grassland.quadratic*log(grassland.t1[i]+1)^2 + 
                           cropland.linear*log(cropland.t1[i]+1) + cropland.quadratic*log(cropland.t1[i]+1)^2 +
                           wetland.linear*log(wetland.t1[i]+1) + wetland.quadratic*log(wetland.t1[i]+1)^2 +
                           temperature.linear*temp.t1[i] + temperature.quadratic*(temp.t1[i]^2) +
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
                           #-c.pos.temp*delta.pos.temp[i] + -c.neg.temp*delta.neg.temp[i] + 
                           -c.neg.grassland*delta.neg.grassland[i] + -c.neg.cropland*delta.neg.cropland[i]  +
                           -c.pos.wetland*delta.pos.wetland[i] + -c.neg.wetland*delta.neg.wetland[i])) + -c.neg.forest*delta.neg.forest[i] +
                      evenness.linear*even.t1[i])
  }
  
  data$q0.lag[i] <- mean(i.q0.lag)
}
