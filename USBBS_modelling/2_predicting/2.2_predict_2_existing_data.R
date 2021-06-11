library(tidyverse); library(furrr); library(ggthemes); library(tictoc); library(foreach); library(doParallel)

#'-----------------------------------------------------------------------------------------------------
#' * Setup data to predict over*
#'

#' *load in data*
f <- "mean"; buffer.size <- "500"; buffer.type <- "segment"; file <- 3
load(paste0("USBBS_data/whole_data/", file, "years_x_timepoint/data_", f, "_", buffer.type,"_",buffer.size,".rda"))
names(data) <- c("partition", "route", "q0_t1", "q1_t1", "q2_t1", "qInf_t1", "berger_t1", "even_t1", 
                 "q0_t2", "q1_t2", "q2_t2", "qInf_t2", "berger_t2", "even_t2", "delta_q0", "delta_q1", "delta_q2", "delta_qInf",  
                 "delta_berger", "delta_even", "time_t2", "observer_t2", "urban_t1", "urban_t2", "delta_urban",
                 "forest_t1", "forest_t2", "delta_forest", "grass_t1", "grass_t2", "delta_grass",
                 "crop_t1", "crop_t2", "delta_crop", "wet_t1", "wet_t2", "delta_wet", "water_t1", "water_t2", "delta_water",
                 "barren_t1", "barren_t2", "delta_barren", "lc_q0_t1", "lc_q0_t2", "delta_lc_q0", "lc_q1_t1", "lc_q1_t2",
                 "delta_lc_q1", "temp_t1", "temp_t2", "temp_sd_t1", "temp_sd_t2", "prec_t1", "prec_t2", "prec_sd_t1", "prec_sd_t2",
                 "elev", "elev_sd", "delta_temp", "delta_prec", "segmentID", "routeID", "observerID")

data <- data %>% # filter(segmentID==1 | segmentID==3 | segmentID==5) %>%
  mutate(urban_t2 = log(urban_t2+1), forest_t2 = log(forest_t2+1), grass_t2 = log(grass_t2+1), crop_t2 = log(crop_t2+1), wet_t2 = log(wet_t2+1),
         urban_t1 = log(urban_t1+1), forest_t1 = log(forest_t1+1), grass_t1 = log(grass_t1+1), crop_t1 = log(crop_t1+1), wet_t1 = log(wet_t1+1), temp_t2 = log(temp_t2+1)) %>%
  mutate(urban_squared_t2 = urban_t2^2, forest_squared_t2 = forest_t2^2, grass_squared_t2 = grass_t2^2,
         crop_squared_t2 = crop_t2^2, wet_squared_t2 = wet_t2^2,
         urban_squared_t1 = urban_t1^2, forest_squared_t1 = forest_t1^2, grass_squared_t1 = grass_t1^2,
         crop_squared_t1 = crop_t1^2, wet_squared_t1 = wet_t1^2, temp_squared_t2 = temp_t2^2)
data 

# add time as a constant, mean sampling time for segments
data$time <- 330

# generate vectors of positive & negative land cover change

data$delta_pos_urban <- 0; data$delta_neg_urban <- 0
for(i in 1:nrow(data)){ if(data$delta_urban[i]>0) data$delta_pos_urban[i] <- abs(data$delta_urban[i])
if(data$delta_urban[i]<=0) data$delta_neg_urban[i] <- abs(data$delta_urban[i]); print(i)}

data$delta_pos_forest<- 0; data$delta_neg_forest <- 0
for(i in 1:nrow(data)){ if(data$delta_forest[i]>0) data$delta_pos_forest[i] <- abs(data$delta_forest[i])
if(data$delta_forest[i]<=0) data$delta_neg_forest[i] <- abs(data$delta_forest[i]); print(i)}

data$delta_pos_grass <- 0; data$delta_neg_grass <- 0
for(i in 1:nrow(data)){ if(data$delta_grass[i]>0) data$delta_pos_grass[i] <- abs(data$delta_grass[i])
if(data$delta_grass[i]<=0) data$delta_neg_grass[i] <- abs(data$delta_grass[i]); print(i)}

data$delta_pos_crop <- 0; data$delta_neg_crop <- 0
for(i in 1:nrow(data)){ if(data$delta_crop[i]>0) data$delta_pos_crop[i] <- abs(data$delta_crop[i])
if(data$delta_crop[i]<=0) data$delta_neg_crop[i] <- abs(data$delta_crop[i]); print(i)}

data$delta_pos_wet <- 0; data$delta_neg_wet <- 0
for(i in 1:nrow(data)){ if(data$delta_wet[i]>0) data$delta_pos_wet[i] <- abs(data$delta_wet[i])
if(data$delta_wet[i]<=0) data$delta_neg_wet[i] <- abs(data$delta_wet[i]); print(i)}

#'-----------------------------------------------------------------------------------------------------

# make a list of lists
landscape_list <- purrr::transpose(data)

# load model result
result <- readRDS(paste0("USBBS_modelling/1_model_fitting/stan_model_fitted/sampled_", f, "_", buffer.type,"_",buffer.size,".RDS"))

# extract draws for delay params
params <- result$metadata()$stan_variables; params <- params[! params %in% c("lp__", "mu", "log_lik", "random_observer")]  

# 4000 total draws from 4 chains with 1000_iter + 500_warmup each
draws <- result$draws(params, inc_warmup=F, format="draws_df")
draws <- purrr::transpose(draws)

# extract length of posterior sample
sample.length <- as.numeric(length(draws))

#' *load in functions to calculate equilibrium and lagged effective number of species*
source("USBBS_modelling/2_predicting/2.0_model_function.R")

# number of times to sample posterior distribution
n.samples <- 1000

# select posterior sampling positions, to be repeated between eq. & delay function
sampling.positions <- round(runif(n=n.samples, max=sample.length, min = 1))

#'-----------------------------------------------------------------------------------------------------
#' * Predict equilibrium species*
#'

registerDoParallel(cores=10) ; tic()

prediction.matrix <- foreach(s = 1:n.samples, .combine='rbind') %dopar% {

  # sample a value from the parameter posterior distro
  draws.sample <- draws[sampling.positions[s]][[1]]

  predicted <- purrr::map_dbl(.x = landscape_list, .f = equilibrium_function, param_list = draws.sample) 

}
toc()

data$eq_predicted_mean <- round(apply(prediction.matrix, 2, mean), 3)
data$eq_predicted_sd <- round(apply(prediction.matrix, 2, sd), 3)
data$eq_predicted_cv <- round(data$eq_predicted_sd/data$eq_predicted_mean, 3) # coefficient of variation, sd/mean, https://en.wikipedia.org/wiki/Coefficient_of_variation
rm(prediction.matrix)

#'-----------------------------------------------------------------------------------------------------
#' * Predict delay species *
#'

tic()
prediction.matrix <- foreach(s = 1:n.samples, .combine='rbind') %dopar% {

  # sample a value from the parameter posterior distro
  draws.sample <- draws[sampling.positions[s]][[1]]

  predicted <- purrr::map_dbl(.x = landscape_list, .f = delay_function, param_list = draws.sample) 

}
toc(); registerDoSEQ()

data$delay_predicted_mean <- round(apply(prediction.matrix, 2, mean), 3)
data$delay_predicted_sd <- round(apply(prediction.matrix, 2, sd), 3)
data$delay_predicted_cv <- round(data$delay_predicted_sd/data$delay_predicted_mean, 3) 
rm(prediction.matrix)

#'-----------------------------------------------------------------------------------------------------
#' * Calculate extinction debts & colonization credits, difference between eq and lag *
#'

data$debtcredit_t2_eq <- data$eq_predicted_mean - data$q1_t2 
data$debtcredit_delay_eq <- data$eq_predicted_mean - data$delay_predicted_mean 
save(data, file = "USBBS_data/map_predict_data/segment_predicted_debtcredit.rda")

hist(data$eq_predicted_mean, 100)
hist(data$delay_predicted_mean, 100)
hist(data$q1_t2, 100)

hist(data$debtcredit, 100)





