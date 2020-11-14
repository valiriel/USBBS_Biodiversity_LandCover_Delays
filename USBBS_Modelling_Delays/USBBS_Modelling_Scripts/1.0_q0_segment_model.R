##################################################################################################
#' 
#' *4km round buffer around each route segments, 5 segments per route, 10 counts each* 
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
library(stringr)

options(mc.cores = parallel::detectCores())

#########################################################################################################################
#' *DATA IMPORT & MERGE of Diversity and Covariates*

load("USBBS_DataProcessing/covariates.centroid.springtemp.rda")
load("USBBS_DataProcessing/alpha.01.16.rda")

alphas$partition <- str_replace(alphas$partition, pattern=" ", replacement="_")

data <- merge(covariates, alphas, by="partition") 

save(data, file="data.centroid_springtemp.rda") # final version of the data to analyze, 5690 segments, 1138 routes

#########################################################################################################################
#' *SETUP FOR JAGS*

# generate a column in the data with routeid for random effect
data$routeid <- gsub('.{2}$', '', data$partition) # remove last character

# select which variables to extract as vectors for JAGS
wanted.vars  <- c("q0.t2", "routeid", "temp.t1", "temp.t2", "even.t1", "rain.t1", "rain.t2",
                  "urban.t2", "forest.t1", "grassland.t1", "cropland.t1", "wetland.t1",
                  "urban.t1", "forest.t2", "grassland.t2", "cropland.t2", "wetland.t2",
                  "delta.urban", "delta.forest", "delta.cropland", "delta.grassland", "delta.temp", "delta.wetland")

#' *Disassemble dataframe in single vector vars for jags use*
# 
for (i in wanted.vars) {
  if (i != "q0.t2") assign(i, as.vector(data[[i]])) # assign each var to vector
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
urban.t2 <- log(urban.t2 + 1)

forest.t1 <- log(forest.t1 + 1)
forest.t2 <- log(forest.t2 + 2)

grassland.t1 <- log(grassland.t1 + 1)
grassland.t2 <- log(grassland.t2 + 1)

cropland.t1 <- log(cropland.t1 + 1)
cropland.t2 <- log(cropland.t2 + 1)

wetland.t1 <- log(wetland.t1 + 1)
wetland.t2 <- log(wetland.t2 + 1)

tmean.t2 <- as.vector(temp.t2)
tmean.t1 <- as.vector(temp.t1)

##############################################################################################################
#' **Generate separate vectors for positive and negative change of each environmental covariates**
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

#######################################################################################
#' *load model texts*
#'

source("USBBS_Modelling_Delays/USBBS_Modelling_Scripts/1.1_model_txts.R")

#m.q0.top
model.params <- c(# additional params
  "log.like", "tau", "routeid.tau", "aEven.t1",
  # temporal lag params
  "c.pos.urban", "c.pos.grassland", "c.neg.grassland", 
  "c.pos.cropland", "c.neg.cropland", "c.pos.forest", "c.neg.forest", 
  "c.pos.wetland", "c.neg.wetland", #"c.pos.temp", "c.neg.temp",
  # linear quadratic params
  "a", "aUrban.t2", "aUrbanSquared.t2", "aForest.t2", "aForestSquared.t2", 
  "aGrassland.t2", "aGrasslandSquared.t2", "aCropland.t2", "aCroplandSquared.t2", 
  "aWetland.t2", "aWetlandSquared.t2", "aTemperature.t2", "aTemperatureSquared.t2",
  # linear*linear params
  "aUrbanForest.t2", "aUrbanGrassland.t2", "aUrbanCropland.t2", "aForestGrassland.t2",
  "aForestCropland.t2", "aGrasslandCropland.t2", 
  "aWetlandUrban.t2", "aWetlandForest.t2", "aWetlandGrassland.t2", "aWetlandCropland.t2",
  # quadratic*linear params
  "aUrbanSquaredForest.t2", "aUrbanSquaredGrassland.t2", "aUrbanSquaredCropland.t2", "aUrbanSquaredWetland.t2",
  "aForestSquaredUrban.t2", "aForestSquaredCropland.t2", "aForestSquaredGrassland.t2", "aForestSquaredWetland.t2", 
  "aGrasslandSquaredUrban.t2", "aGrasslandSquaredForest.t2", "aGrasslandSquaredCropland.t2", "aGrasslandSquaredWetland.t2",
  "aCroplandSquaredUrban.t2", "aCroplandSquaredForest.t2", "aCroplandSquaredGrassland.t2", "aCroplandSquaredWetland.t2", 
  "aWetlandSquaredUrban.t2", "aWetlandSquaredForest.t2", "aWetlandSquaredGrassland.t2", "aWetlandSquaredCropland.t2")

##############################################################
data.names  <- c("q0.t2", "N", "R", "routeid", "even.t1", 
                 "delta.pos.urban", "delta.pos.forest", "delta.neg.forest", 
                 "delta.pos.cropland", "delta.neg.cropland", "delta.pos.grassland", "delta.neg.grassland", 
                 "delta.pos.temp", "delta.neg.temp", "delta.pos.wetland", "delta.neg.wetland",
                 "urban.t1", "forest.t1", "grassland.t1", "cropland.t1", "wetland.t1", "tmean.t1",
                 "urban.t2", "forest.t2", "grassland.t2", "cropland.t2", "wetland.t2", "tmean.t2")

#' *JAGS model run*
#'
timestamp() # start time
results.q0.notemplag <- ?jags.parallel(data = data.names, # data to use
                                      #progress.bar='text',
                                        parameters.to.save = model.params, # parameters to monitor
                                        n.chains=4, # number of chains
                                        n.iter=5000, # number of iterations
                                        n.burnin=2500, #burn in size, good to have half iterations
                                        #n.thin=3, # thinning rate
                                        jags.seed = set.seed(as.numeric(Sys.time())), # seriously random, take time at which run starts as RNG seed
                                        model.file = "USBBS_Modelling_Delays/USBBS_model_txts/m.q0.notemplag.txt")
timestamp() # end time

save(results.q0.notemplag, file="USBBS_Modelling_Delays/USBBS_models_jags/results.q0.notemplag.rda") 

#'####################################################################  
#'  *Summary of model results and diagnostics*
#'  
# insert what model object you want the summary result to be printed

load("USBBS_Modelling_Delays/USBBS_models_jags/results.q0.notemplag.rda")
summary.m.top <- as.data.frame(results.q0.notemplag$BUGSoutput$summary, rownames=NA)%>% round(5) %>% rownames_to_column()   %>% slice(-(37):-(n()-1)) 

#######################################################################################
#' **MODEL DIAGNOSTICS --- Traceplots**

results <- results.q0.notemplag

R2jags::traceplot(results, mfrow=c(3,3), ask=F,
                  varname=model.params[-1]) # removing likelihood estimates from the plotting

#extract posteriors as tibble 
posteriors <- as_tibble(results$BUGSoutput$sims.list) %>% select(-log.like) %>% as.data.frame()

mcmc_dens(posteriors, pars = model.params[-1])
          
#######################################################################################
#' **MODEL DIAGNOSTICS --- leave one out cross-validation + WAIC**

loo.top <- loo(mcmc(results.q0.notemplag$BUGSoutput$sims.list)$log.like)
plot(loo.top)

loo.top.RAIN <- loo(mcmc(results.q0.centroidspringtemp$BUGSoutput$sims.list)$log.like)
plot(loo.top)
