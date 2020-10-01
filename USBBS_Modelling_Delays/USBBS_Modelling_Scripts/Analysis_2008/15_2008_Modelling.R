##################################################################################################
#' 
#' *4km round buffer around centroid of each route segments, 5 segments per route, 10 counts each* 
#' 
#' *q0 = Species Richness* 
#' 
#' **Bayesian model run, selection and diagnostic**
#'  
######################################################################################################################

library(coda)
library(loo)
library(R2jags)
library(tidyverse)
library(lattice)
library(bayesplot)

options(mc.cores = parallel::detectCores())

#########################################################################################################################
#' **DATA IMPORT/ SETUP FOR JAGS**

load("USBBS_Modelling_Scripts/one more time/data_08.rda")
load("data.segments.4km.rda")

data.t1 <- data.segments.4km[,c("partition", "urban.t1", "forest.t1", "cropland.t1", "grassland.t1", "wetland.t1", "tmean.t1", "even.t1", "q0.t1")] 

data <- merge(data.08, data.t1, by = "partition")

# generate a column in the data with routeid for random effect
data$routeid <- gsub('.{2}$', '', data$partition) # remove last character

data$delta.urban <-  data$urban.t08 - data$urban.t1
data$delta.forest <-  data$forest.t08 - data$forest.t1
data$delta.grassland <-  data$grassland.t08 - data$grassland.t1
data$delta.cropland <-  data$cropland.t08 - data$cropland.t1
data$delta.wetland <-  data$wetland.t08 - data$wetland.t1
data$delta.temp <-  data$t.08 - data$tmean.t1
data$delta.q0 <- data$q0.08 - data$q0.t1
data$delta.even <-  data$even.08 - data$even.t1

save(data, file= "USBBS_Modelling_Scripts/one more time/data_08.rda")
load("USBBS_Modelling_Scripts/one more time/data_08.rda")

# select which variables to extract as vector for JAGS
wanted.vars  <- c("q0.t08", "routeid", "tmean.t1", "tmean.t08", "even.t1",
                  "urban.t08", "forest.t1", "grassland.t1", "cropland.t1", "wetland.t1",
                  "urban.t1", "forest.t08", "grassland.t08", "cropland.t08", "wetland.t08",
                  "delta.urban", "delta.forest", "delta.cropland", "delta.grassland", "delta.temp", "delta.wetland")

#' *Disassemble dataframe in single vector vars for jags use*
# 
for (i in wanted.vars) {
  
  if (i != "q0.t08") assign(i, as.vector(data[[i]])) # assign each var to vector
  else assign(i, data[[i]])
  
}

# set random eff variable as a factor
routeid <- as.factor(routeid)

# set number of observations per each var
N <- as.numeric(length(urban.t1))

# set number of routes per random effect
R <- as.numeric(length(unique(routeid)))


################################################################################################################
#'
#'*log values*
#'

urban.t1 <- log(urban.t1 + 1)
urban.t08 <- log(urban.t08 + 1)

forest.t1 <- log(forest.t1 + 1)
forest.t08 <- log(forest.t08 + 2)

grassland.t1 <- log(grassland.t1 + 1)
grassland.t08 <- log(grassland.t08 + 1)

cropland.t1 <- log(cropland.t1 + 1)
cropland.t08 <- log(cropland.t08 + 1)

wetland.t1 <- log(wetland.t1 + 1)
wetland.t08 <- log(wetland.t08 + 1)

tmean.t08 <- as.vector(tmean.t08)
tmean.t1 <- as.vector(tmean.t1)

##############################################################################################################
#' **Generate separate vectors for positive and negative change of each environmental covariate**
#' 

####
delta.pos.urban <- vector("numeric",length = N)
delta.neg.urban <- vector("numeric",length = N)

for(i in 1:N){
  if(delta.urban[i]>0)
    delta.pos.urban[i] <- abs(delta.urban[i])
  if(delta.urban[i]<=0)
    delta.neg.urban[i] <- abs(delta.urban[i])
}

####
delta.pos.forest<- vector("numeric",length = N)
delta.neg.forest <- vector("numeric",length = N)

for(i in 1:N){
  if(delta.forest[i]>0)
    delta.pos.forest[i] <- abs(delta.forest[i])
  if(delta.forest[i]<=0)
    delta.neg.forest[i] <- abs(delta.forest[i])
}

####
delta.pos.grassland <- vector("numeric",length = N)
delta.neg.grassland <- vector("numeric",length = N)

for(i in 1:N){
  if(delta.grassland[i]>0)
    delta.pos.grassland[i] <- abs(delta.grassland[i])
  if(delta.grassland[i]<=0)
    delta.neg.grassland[i] <- abs(delta.grassland[i])
}

####
delta.pos.cropland <- vector("numeric",length = N)
delta.neg.cropland <- vector("numeric",length = N)

for(i in 1:N){
  if(delta.cropland[i]>0)
    delta.pos.cropland[i] <- abs(delta.cropland[i])
  if(delta.cropland[i]<=0)
    delta.neg.cropland[i] <- abs(delta.cropland[i])
}

####
delta.pos.wetland <- vector("numeric",length = N)
delta.neg.wetland <- vector("numeric",length = N)

for(i in 1:N){
  if(delta.wetland[i]>0)
    delta.pos.wetland[i] <- abs(delta.wetland[i])
  if(delta.wetland[i]<=0)
    delta.neg.wetland[i] <- abs(delta.wetland[i])
}

####
delta.pos.temp <- vector("numeric",length = N)
delta.neg.temp <- vector("numeric",length = N)

for(i in 1:N){
  if(delta.temp[i]>0)
    delta.pos.temp[i] <- abs(delta.temp[i])
  if(delta.temp[i]<=0)
    delta.neg.temp[i] <- abs(delta.temp[i])
}


####################################################################################################################
#' 
#' **Text of JAGS model**
#' 
#' note: q0 refers to species richness, under Hill' number framework
#'
####################################################################################################################
#
#'  * Top model, directional change delay plus all interactions and quadratic*
#

m.q0.iter <- 'model{

for(n in 1:N){
    
    q0.t08[n] ~ dpois(lambda[n])

################ Main model

    log(lambda[n]) <- a + 
# linear & quadratic terms t08
                            (aUrban.t08*urban.t08[n] + aUrbanSquared.t08*pow(urban.t08[n], 2) +
                              aForest.t08*forest.t08[n] + aForestSquared.t08*pow(forest.t08[n], 2) +
                              aGrassland.t08*grassland.t08[n] + aGrasslandSquared.t08*pow(grassland.t08[n], 2) +  
                              aCropland.t08*cropland.t08[n] + aCroplandSquared.t08*pow(cropland.t08[n], 2) +
                              aWetland.t08*wetland.t08[n] + aWetlandSquared.t08*pow(wetland.t08[n], 2) +
                              aTemperature.t08*tmean.t08[n] + aTemperatureSquared.t08*pow(tmean.t08[n], 2) +
# linear*linear interactions t08
                            aUrbanForest.t08*urban.t08[n]*forest.t08[n] +               
                              aUrbanGrassland.t08*urban.t08[n]*grassland.t08[n] +        
                              aUrbanCropland.t08*urban.t08[n]*cropland.t08[n] +          
                              aForestGrassland.t08*forest.t08[n]*grassland.t08[n] +      
                              aForestCropland.t08*forest.t08[n]*cropland.t08[n] +        
                              aGrasslandCropland.t08*grassland.t08[n]*cropland.t08[n] +  
                              aWetlandUrban.t08*wetland.t08[n]*urban.t08[n] +
                              aWetlandForest.t08*wetland.t08[n]*forest.t08[n] +
                              aWetlandGrassland.t08*wetland.t08[n]*grassland.t08[n] +
                              aWetlandCropland.t08*wetland.t08[n]*cropland.t08[n] +
# quadratic*linear interactions t08
                            aUrbanSquaredForest.t08*pow(urban.t08[n], 2)*forest.t08[n] +               
                              aUrbanSquaredGrassland.t08*pow(urban.t08[n], 2)*grassland.t08[n] +        
                              aUrbanSquaredCropland.t08*pow(urban.t08[n], 2)*cropland.t08[n] + 
                              aUrbanSquaredWetland.t08*pow(urban.t08[n], 2)*wetland.t08[n] +
                            aForestSquaredUrban.t08*pow(forest.t08[n], 2)*urban.t08[n] + 
                              aForestSquaredCropland.t08*pow(forest.t08[n], 2)*cropland.t08[n] +
                              aForestSquaredGrassland.t08*pow(forest.t08[n], 2)*grassland.t08[n] + 
                              aForestSquaredWetland.t08*pow(forest.t08[n], 2)*wetland.t08[n] +
                            aGrasslandSquaredUrban.t08*pow(grassland.t08[n], 2)*urban.t08[n] + 
                              aGrasslandSquaredForest.t08*pow(grassland.t08[n], 2)*forest.t08[n] +
                              aGrasslandSquaredCropland.t08*pow(grassland.t08[n], 2)*cropland.t08[n] + 
                              aGrasslandSquaredWetland.t08*pow(grassland.t08[n], 2)*wetland.t08[n] +
                            aCroplandSquaredUrban.t08*pow(cropland.t08[n], 2)*urban.t08[n] + 
                              aCroplandSquaredForest.t08*pow(cropland.t08[n], 2)*forest.t08[n] +
                              aCroplandSquaredGrassland.t08*pow(cropland.t08[n], 2)*grassland.t08[n] + 
                              aCroplandSquaredWetland.t08*pow(cropland.t08[n], 2)*wetland.t08[n] +
                            aWetlandSquaredUrban.t08*pow(wetland.t08[n], 2)*urban.t08[n] + 
                              aWetlandSquaredForest.t08*pow(wetland.t08[n], 2)*forest.t08[n] +
                              aWetlandSquaredGrassland.t08*pow(wetland.t08[n], 2)*grassland.t08[n] + 
                              aWetlandSquaredCropland.t08*pow(wetland.t08[n], 2)*cropland.t08[n]) *
# temporal lag function t08
                            (exp((-c.pos.forest*delta.pos.forest[n]) + (-c.neg.forest*delta.neg.forest[n]) +
                                   (-c.pos.urban*delta.pos.urban[n]) +
                                   (-c.pos.grassland*delta.pos.grassland[n]) + (-c.neg.grassland*delta.neg.grassland[n]) +
                                   (-c.pos.cropland*delta.pos.cropland[n]) + (-c.neg.cropland*delta.neg.cropland[n]) +
                                   (-c.pos.wetland*delta.pos.wetland[n]) + (-c.neg.wetland*delta.neg.wetland[n]) +
                                   (-c.pos.temp*delta.pos.temp[n]) + (-c.neg.temp*delta.neg.temp[n]))) + 
# linear and quadratic terms t1
                            (aUrban.t1*urban.t1[n] + aUrbanSquared.t1 * pow(urban.t1[n], 2) +
                              aForest.t1*forest.t1[n] + aForestSquared.t1 * pow(forest.t1[n], 2) +
                              aGrassland.t1*grassland.t1[n] + aGrasslandSquared.t1 * pow(grassland.t1[n], 2) +  
                              aCropland.t1*cropland.t1[n] + aCroplandSquared.t1 * pow(cropland.t1[n], 2) +
                              aWetland.t1*wetland.t1[n] + aWetlandSquared.t1 * pow(wetland.t1[n], 2) +
                              aTemperature.t1*tmean.t1[n] + aTemperatureSquared.t1*pow(tmean.t1[n],2) +
# linear*linear interactions t1
                            aUrbanForest.t1*urban.t1[n]*forest.t1[n] +               
                              aUrbanGrassland.t1*urban.t1[n]*grassland.t1[n] +        
                              aUrbanCropland.t1*urban.t1[n]*cropland.t1[n] +          
                              aForestGrassland.t1*forest.t1[n]*grassland.t1[n] +     
                              aForestCropland.t1*forest.t1[n]*cropland.t1[n] +        
                              aGrasslandCropland.t1*grassland.t1[n]*cropland.t1[n]+   
                              aWetlandUrban.t1 * wetland.t1[n]*urban.t1[n] +
                              aWetlandForest.t1 * wetland.t1[n]*forest.t1[n] +
                              aWetlandGrassland.t1 * wetland.t1[n]*grassland.t1[n] +
                              aWetlandCropland.t1 * wetland.t1[n]*cropland.t1[n] +
# quadratic*linear interactions t1
                            aUrbanSquaredForest.t1*pow(urban.t1[n], 2)*forest.t1[n] +               
                              aUrbanSquaredGrassland.t1*pow(urban.t1[n], 2)*grassland.t1[n] +        
                              aUrbanSquaredCropland.t1*pow(urban.t1[n], 2)*cropland.t1[n] + 
                              aUrbanSquaredWetland.t1*pow(urban.t1[n], 2)*wetland.t1[n] +
                            aForestSquaredUrban.t1*pow(forest.t1[n], 2)*urban.t1[n] + 
                              aForestSquaredCropland.t1*pow(forest.t1[n], 2)*cropland.t1[n] +
                              aForestSquaredGrassland.t1*pow(forest.t1[n], 2)*grassland.t1[n] + 
                              aForestSquaredWetland.t1*pow(forest.t1[n], 2)*wetland.t1[n] +
                            aGrasslandSquaredUrban.t1*pow(grassland.t1[n], 2)*urban.t1[n] + 
                              aGrasslandSquaredForest.t1*pow(grassland.t1[n], 2)*forest.t1[n] +
                              aGrasslandSquaredCropland.t1*pow(grassland.t1[n], 2)*cropland.t1[n] + 
                              aGrasslandSquaredWetland.t1*pow(grassland.t1[n], 2)*wetland.t1[n] +
                            aCroplandSquaredUrban.t1*pow(cropland.t1[n], 2)*urban.t1[n] + 
                              aCroplandSquaredForest.t1*pow(cropland.t1[n], 2)*forest.t1[n] +
                              aCroplandSquaredGrassland.t1*pow(cropland.t1[n], 2)*grassland.t1[n] + 
                              aCroplandSquaredWetland.t1*pow(cropland.t1[n], 2)*wetland.t1[n] +
                            aWetlandSquaredUrban.t1*pow(wetland.t1[n], 2)*urban.t1[n] + 
                              aWetlandSquaredForest.t1*pow(wetland.t1[n], 2)*forest.t1[n] +
                              aWetlandSquaredGrassland.t1*pow(wetland.t1[n], 2)*grassland.t1[n] + 
                              aWetlandSquaredCropland.t1*pow(wetland.t1[n], 2)*cropland.t1[n]) *
# temporal lag function t1
                            (1 - (exp((-c.pos.forest*delta.pos.forest[n]) + (-c.neg.forest*delta.neg.forest[n]) +
                                   (-c.pos.urban*delta.pos.urban[n]) +
                                   (-c.pos.grassland*delta.pos.grassland[n]) + (-c.neg.grassland*delta.neg.grassland[n]) +
                                   (-c.pos.cropland*delta.pos.cropland[n]) + (-c.neg.cropland*delta.neg.cropland[n]) +
                                   (-c.pos.wetland*delta.pos.wetland[n]) + (-c.neg.wetland*delta.neg.wetland[n]) +
                                   (-c.pos.temp*delta.pos.temp[n]) + (-c.neg.temp*delta.neg.temp[n])))) + 
# additional terms
                            Arouteid[routeid[n]] + OLET[n] +
                              aEven.t1*even.t1[n]

################ log-Likelihood calculation for future WAIC-loo analysis
    
    log.like[n] <- logdensity.pois(q0.t08[n], lambda[n])
    
  }

###############
# Random Effect

for(r in 1:R){
Arouteid[r] ~ dnorm(0, routeid.tau)
}

#####################
for (j in 1:N){
  OLET[j] ~ dnorm(0, tau)
} 

###############
# Priors

####
# temporal delay params priors

  c.pos.urban ~ dunif(0, 1)
  c.pos.grassland ~ dunif(0, 1)
    c.neg.grassland ~ dunif(0, 1)
  c.pos.cropland ~ dunif(0, 1)
    c.neg.cropland ~ dunif(0, 1)
  c.pos.forest ~ dunif(0, 1)
    c.neg.forest ~ dunif(0, 1)
  c.pos.wetland ~ dunif(0, 1)
    c.neg.wetland ~ dunif(0, 1)
  c.pos.temp ~ dunif(0,1)
    c.neg.temp ~ dunif(0,1)

####
# linear and quadratic params priors
  
  a ~ dnorm(0, 10^-6)
  
  aUrban.t08 ~ dnorm(0, 10^-6)
    aUrbanSquared.t08 ~ dnorm(0, 10^-6)
    aUrban.t1 <- aUrban.t08
    aUrbanSquared.t1 <- aUrbanSquared.t08
  aForest.t08 ~ dnorm(0, 10^-6)
    aForestSquared.t08 ~ dnorm(0, 10^-6)
    aForest.t1 <- aForest.t08
    aForestSquared.t1 <- aForestSquared.t08
  aGrassland.t08 ~ dnorm(0, 10^-6)
    aGrasslandSquared.t08 ~ dnorm(0, 10^-6)
    aGrassland.t1 <- aGrassland.t08
    aGrasslandSquared.t1 <- aGrasslandSquared.t08
  aCropland.t08 ~ dnorm(0, 10^-6)
    aCroplandSquared.t08 ~ dnorm(0, 10^-6)
    aCropland.t1 <- aCropland.t08
    aCroplandSquared.t1 <- aCroplandSquared.t08
  aWetland.t08 ~ dnorm(0, 10^-6)
    aWetlandSquared.t08 ~ dnorm(0, 10^-6)
    aWetland.t1 <- aWetland.t08
    aWetlandSquared.t1 <- aWetlandSquared.t08
  aTemperature.t08 ~ dnorm(0, 10^-6)
    aTemperatureSquared.t08 ~ dnorm(0, 10^-6)
    aTemperature.t1 <- aTemperature.t08
    aTemperatureSquared.t1 <- aTemperatureSquared.t08

####
# linear*linear interaction params priors

  aUrbanForest.t08 ~ dnorm(0, 10^-6)
    aUrbanForest.t1 <- aUrbanForest.t08
  aUrbanGrassland.t08 ~ dnorm(0, 10^-6)
    aUrbanGrassland.t1 <- aUrbanGrassland.t08
  aUrbanCropland.t08 ~ dnorm(0, 10^-6)
    aUrbanCropland.t1 <- aUrbanCropland.t08
  aForestGrassland.t08 ~ dnorm(0, 10^-6)
    aForestGrassland.t1 <- aForestGrassland.t08
  aForestCropland.t08 ~ dnorm(0, 10^-6)
    aForestCropland.t1 <- aForestCropland.t08
  aGrasslandCropland.t08 ~ dnorm(0, 10^-6)
    aGrasslandCropland.t1 <- aGrasslandCropland.t08
  aWetlandUrban.t08 ~ dnorm(0, 10^-6)
    aWetlandUrban.t1 <- aWetlandUrban.t08
  aWetlandForest.t08 ~ dnorm(0, 10^-6)
    aWetlandForest.t1 <- aWetlandForest.t08
  aWetlandGrassland.t08 ~ dnorm(0, 10^-6)
    aWetlandGrassland.t1 <- aWetlandGrassland.t08
  aWetlandCropland.t08 ~ dnorm(0, 10^-6)
    aWetlandCropland.t1 <- aWetlandCropland.t08

####
# quadratic*linear interaction params priors

  aUrbanSquaredForest.t08 ~ dnorm(0, 10^-6)              
  aUrbanSquaredForest.t1 <- aUrbanSquaredForest.t08
  aUrbanSquaredGrassland.t08 ~ dnorm(0, 10^-6)       
  aUrbanSquaredGrassland.t1 <- aUrbanSquaredGrassland.t08
  aUrbanSquaredCropland.t08 ~ dnorm(0, 10^-6)
  aUrbanSquaredCropland.t1 <- aUrbanSquaredCropland.t08
  aUrbanSquaredWetland.t08 ~ dnorm(0, 10^-6)
  aUrbanSquaredWetland.t1 <- aUrbanSquaredWetland.t08
  aForestSquaredUrban.t08 ~ dnorm(0, 10^-6) 
  aForestSquaredUrban.t1 <- aForestSquaredUrban.t08
  aForestSquaredCropland.t08 ~ dnorm(0, 10^-6)
  aForestSquaredCropland.t1 <- aForestSquaredCropland.t08
  aForestSquaredGrassland.t08 ~ dnorm(0, 10^-6)
  aForestSquaredGrassland.t1 <- aForestSquaredGrassland.t08
  aForestSquaredWetland.t08 ~ dnorm(0, 10^-6)
  aForestSquaredWetland.t1 <- aForestSquaredWetland.t08
  aGrasslandSquaredUrban.t08 ~ dnorm(0, 10^-6)
  aGrasslandSquaredUrban.t1 <- aGrasslandSquaredUrban.t08
  aGrasslandSquaredForest.t08 ~ dnorm(0, 10^-6)
  aGrasslandSquaredForest.t1 <- aGrasslandSquaredForest.t08
  aGrasslandSquaredCropland.t08 ~ dnorm(0, 10^-6)
  aGrasslandSquaredCropland.t1 <- aGrasslandSquaredCropland.t08
  aGrasslandSquaredWetland.t08 ~ dnorm(0, 10^-6)
  aGrasslandSquaredWetland.t1 <- aGrasslandSquaredWetland.t08
  aCroplandSquaredUrban.t08 ~ dnorm(0, 10^-6) 
  aCroplandSquaredUrban.t1 <- aCroplandSquaredUrban.t08
  aCroplandSquaredForest.t08 ~ dnorm(0, 10^-6)
  aCroplandSquaredForest.t1 <- aCroplandSquaredForest.t08
  aCroplandSquaredGrassland.t08 ~ dnorm(0, 10^-6) 
  aCroplandSquaredGrassland.t1 <- aCroplandSquaredGrassland.t08
  aCroplandSquaredWetland.t08 ~ dnorm(0, 10^-6)
  aCroplandSquaredWetland.t1 <- aCroplandSquaredWetland.t08
  aWetlandSquaredUrban.t08 ~ dnorm(0, 10^-6) 
  aWetlandSquaredUrban.t1 <- aWetlandSquaredUrban.t08
  aWetlandSquaredForest.t08 ~ dnorm(0, 10^-6)
  aWetlandSquaredForest.t1 <- aWetlandSquaredForest.t08
  aWetlandSquaredGrassland.t08 ~ dnorm(0, 10^-6) 
  aWetlandSquaredGrassland.t1 <- aWetlandSquaredGrassland.t08
  aWetlandSquaredCropland.t08 ~ dnorm(0, 10^-6)
  aWetlandSquaredCropland.t1 <- aWetlandSquaredCropland.t08
  
####
# additional params priors

  tau ~ dgamma(0.001, 0.001)
  routeid.tau ~ dgamma(0.001, 0.001)
  aEven.t1 ~ dnorm(0, 10^-6) 

}'

write(m.q0.iter, "USBBS_model_txts/model_q0_txt/m.q0.iter_2008.txt")

#######################################################################################

#m.q0.iter
model.params <- c(# additional params
  "log.like", "tau", "routeid.tau", "aEven.t1",
  # temporal lag params
  "c.pos.urban", "c.pos.grassland", "c.neg.grassland", 
  "c.pos.cropland", "c.neg.cropland", "c.pos.forest", "c.neg.forest", 
  "c.pos.wetland", "c.neg.wetland", "c.pos.temp", "c.neg.temp",
  # linear quadratic params
  "a", "aUrban.t08", "aUrbanSquared.t08", "aForest.t08", "aForestSquared.t08", 
  "aGrassland.t08", "aGrasslandSquared.t08", "aCropland.t08", "aCroplandSquared.t08", 
  "aWetland.t08", "aWetlandSquared.t08", "aTemperature.t08", "aTemperatureSquared.t08",
  # linear*linear params
  "aUrbanForest.t08", "aUrbanGrassland.t08", "aUrbanCropland.t08", "aForestGrassland.t08",
  "aForestCropland.t08", "aGrasslandCropland.t08", 
  "aWetlandUrban.t08", "aWetlandForest.t08", "aWetlandGrassland.t08", "aWetlandCropland.t08",
  # quadratic*linear params
  "aUrbanSquaredForest.t08", "aUrbanSquaredGrassland.t08", "aUrbanSquaredCropland.t08", "aUrbanSquaredWetland.t08",
  "aForestSquaredUrban.t08", "aForestSquaredCropland.t08", "aForestSquaredGrassland.t08", "aForestSquaredWetland.t08", 
  "aGrasslandSquaredUrban.t08", "aGrasslandSquaredForest.t08", "aGrasslandSquaredCropland.t08", "aGrasslandSquaredWetland.t08",
  "aCroplandSquaredUrban.t08", "aCroplandSquaredForest.t08", "aCroplandSquaredGrassland.t08", "aCroplandSquaredWetland.t08", 
  "aWetlandSquaredUrban.t08", "aWetlandSquaredForest.t08", "aWetlandSquaredGrassland.t08", "aWetlandSquaredCropland.t08")

##############################################################
data.names  <- c("q0.t08", "N", "R", "routeid", "even.t1", 
                 "delta.pos.urban", "delta.pos.forest", "delta.neg.forest", 
                 "delta.pos.cropland", "delta.neg.cropland", "delta.pos.grassland", "delta.neg.grassland", 
                 "delta.pos.temp", "delta.neg.temp", "delta.pos.wetland", "delta.neg.wetland",
                 "urban.t1", "forest.t1", "grassland.t1", "cropland.t1", "wetland.t1", "tmean.t1",
                 "urban.t08", "forest.t08", "grassland.t08", "cropland.t08", "wetland.t08", "tmean.t08")

#' *JAGS model run*
#'
timestamp() # start time
results.q0.iter_2008 <- jags.parallel(data = data.names, # data to use
                                 #progress.bar='text',
                                 parameters.to.save = model.params, # parameters to monitor
                                 n.chains=4, # number of chains
                                 n.iter=10000, # number of iteractions
                                 n.burnin=5000, #burn in size, good to have half iteractions
                                 #n.thin=3, # thinning rate
                                 jags.seed = set.seed(as.numeric(Sys.time())), # seriously random, take time at which run starts as RNG seed
                                 model.file = "USBBS_model_txts/model_q0_txt/m.q0.iter_2008.txt")
timestamp() # end time

save(results.q0.iter_2008, file="USBBS_models_jags/results.q0.iter_2008.rda") 

#'####################################################################  
#'  *Summary of model results and diagnostics*
#'  
# insert what model object you want the summary result to be printed
#

load("USBBS_models_jags/results.q0.iter_2008.rda")
summary.m.iter <- as_tibble(results.q0.iter_2008$BUGSoutput$summary, rownames=NA) %>% round(5) %>% rownames_to_column %>% slice(-(68):-(n()-1)) 
summary.m.iter

#######################################################################################
#' 
#' **MODEL DIAGNOSTICS**
#'
#'  **Traceplots**
#'

results <- results.q0.iter_2008

R2jags::traceplot(results, mfrow=c(3,3), ask=F,
                  varname=model.params[-1]) # removing likelihood estimates from the plotting

#extract posteriors as tibble 
posteriors <- as_tibble(results$BUGSoutput$sims.list) %>% select(-log.like) %>% as.data.frame()

mcmc_dens(posteriors,
          pars = model.params[-1])

#######################################################################################
#' 
#' **MODEL SELECTION**
#'

loo.iter <- loo(mcmc(results.q0.iter_2008$BUGSoutput$sims.list)$log.like)
plot(loo.iter)
