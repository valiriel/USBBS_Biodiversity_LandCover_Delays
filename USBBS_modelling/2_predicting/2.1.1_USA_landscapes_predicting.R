library(tidyverse); library(sf); library(stringr); library(exactextractr); library(raster); library(mapview); library(dplyr); 
library(furrr); library(ggthemes); library(tictoc); library(foreach); library(doParallel)

#'-----------------------------------------------------------------------------------------------------
#' * Setup data to predict over*
#'

# load in USA landscape, 10x10km hexagon sampled
load(file = "USBBS_data/map_predict_data/USA_landscape_predict_ready.rda")

# make a list of lists
landscape_list <- purrr::transpose(usa_landscape)

#'*load model result*
f <- "mean"; buffer.size <- "500"; buffer.type <- "segment"; file <- 3
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

#'-----------------------------------------------------------------------------------------------------
#' * Setup data to predict over*
#'

# initialize storage for each sample parameter estimate
#prediction.matrix <- matrix(data=0, nrow=n.samples, ncol=length(landscape_list))

# select posterior sampling positions, to be repeated between eq. & delay function
sampling.positions <- round(runif(n=n.samples, max=sample.length, min = 1))
save(sampling.positions, file = "USBBS_data/map_predict_data/sampling_positions.rda")
#'-----------------------------------------------------------------------------------------------------
#' * Predict for delay effective number of species *
#'

registerDoParallel(cores=10) ; tic()

prediction.matrix <- foreach(s = 1:n.samples, .combine='rbind') %dopar% {

#for (s in 1:n.samples) {
  
  # sample a value from the parameter posterior distro
  draws.sample <- draws[sampling.positions[s]][[1]]
  #tic()
  delay.predicted <- purrr::map_dbl(.x = landscape_list, .f = delay_function, param_list = draws.sample) 
  #toc()
  #prediction.matrix[s,] <- round(delay.predicted, 3)
  
  #print(paste0(s,"/",n.samples))
  
}
toc(); registerDoSEQ()

save(prediction.matrix, file = "USBBS_data/map_predict_data/USA_predicted_delay_draws_matrix.rda")

usa_landscape$delay_mean <- round(apply(prediction.matrix, 2, mean), 3)
usa_landscape$delay_sd <- round(apply(prediction.matrix, 2, sd), 3)
usa_landscape$delay_cv <- round(usa_landscape$delay_sd/usa_landscape$delay_mean, 3) # coefficient of variation, sd/mean, https://en.wikipedia.org/wiki/Coefficient_of_variation

rm(prediction.matrix)
save(usa_landscape, file = "USBBS_data/map_predict_data/USA_landscape_predicted.rda")

#'-----------------------------------------------------------------------------------------------------
#' * Predict for equilibrium effective number of species *
#'

registerDoParallel(cores=10) ; tic()

prediction.matrix <- foreach(s = 1:n.samples, .combine='rbind') %dopar% {

  draws.sample <- draws[sampling.positions[s]][[1]]

  eq.predicted <- purrr::map_dbl(.x = landscape_list, .f = equilibrium_function, param_list = draws.sample)

}  
toc(); registerDoSEQ()

save(prediction.matrix, file = "USBBS_data/map_predict_data/USA_predicted_eq_draws_matrix.rda")

usa_landscape$eq_mean <- round(apply(prediction.matrix, 2, mean), 3)
usa_landscape$eq_sd <- round(apply(prediction.matrix, 2, sd), 3)
usa_landscape$eq_cv <- round(usa_landscape$eq_sd/usa_landscape$eq_mean, 3)

rm(prediction.matrix)
save(usa_landscape, file = "USBBS_data/map_predict_data/USA_landscape_predicted.rda")

#'-----------------------------------------------------------------------------------------------------
#' * Calculate extinction debts & colonization credits, difference between eq and lag *
#'

load("USBBS_data/map_predict_data/USA_predicted_delay_draws_matrix.rda"); delay_matrix <- prediction.matrix
load("USBBS_data/map_predict_data/USA_predicted_eq_draws_matrix.rda"); eq_matrix <- prediction.matrix
load("USBBS_data/map_predict_data/USA_landscape_predicted.rda"); rm(prediction.matrix)

debtcredit_matrix <- eq_pred - delay_pred

usa_landscape$debtcredit_mean <- round(apply(debtcredit_matrix, 2, mean), 3) 
usa_landscape$debtcredit_sd <- round(apply(debtcredit_matrix, 2, sd), 3)  
usa_landscape$debtcredit_cv <- round(usa_landscape$debtcredit_sd/usa_landscape$debtcredit_mean, 3) 

save(usa_landscape, file = "USBBS_data/map_predict_data/USA_landscape_predicted.rda")


