# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library("rstan");library("loo"); library("cmdstanr"); library(posterior); library(bayesplot)
options(mc.cores = parallel::detectCores()); color_scheme_set("viridis")
cmdstan_path(); cmdstan_version()

#'-----------------------------------------------------------------------------------------------------
#'* compile model into C++ for future fitting*

# ignore the warning
#model.q1 <- cmdstan_model(stan_file="USBBS_modelling/1_model_fitting/stan_model_scripts/reduce_sum.stan",
#                          cpp_options = list(stan_threads = TRUE))
model.q1 <- cmdstan_model(stan_file="USBBS_modelling/1_model_fitting/stan_model_scripts/q1_main_model.stan")
model.q1$print()
model.q1$exe_file()

#'-----------------------------------------------------------------------------------------------------
#'* load modeling ready data and model formula*

# alpha diversities calculated for min, mean, max abundances
#fun.names <- c("min", "mean", "max")
fun.names <- c("min", "mean")

buffer.types <- c("segment","centroid") 

# use 3 years aggregate per timepoint, other option is 2 which contains more routes
file <- 3

# some try values
f <- "mean"; buffer.size <- "4000"; buffer.type <- "segment"; 
#c(500, 1000, 2000, 4000, 6000)

for (f in fun.names) {

  for (buffer.type in buffer.types) {
    
    if(buffer.type == "centroid") buffer.sizes <- c(4000)
    if(buffer.type == "segment") buffer.sizes <- c(4000)
    
    for(buffer.size in buffer.sizes) {
    
    # load modeling ready data
    load(paste0("USBBS_data/model_ready_data/", file, "years_x_timepoint/data_", f, "_", buffer.type,"_",buffer.size,".rda"))
    
    # setup inits for chains
    inits.list <- purrr::map(
      1:4, ~ list("intercept" = runif(1, 0, 20), "sigma" = runif(1, 0, 5),
                                  
                  # linear param
                  "b_urban" = runif(1, 0, 2), "b_forest" = runif(1, 0, 2), "b_grass" = runif(1, 0, 2),
                  "b_crop"= runif(1, 0, 2), "b_wet" = runif(1, 0, 2),
                  
                  # quadratic params
                  "b2_urban" = runif(1, -2, 0), "b2_forest" = runif(1, -2, 0), "b2_grass" = runif(1, -2, 0),
                  "b2_crop"= runif(1, -2, 0), "b2_wet" = runif(1, -2, 0), 
                  
                  # linear*linear interaction params
                  "b_urban_forest" = runif(1, -2, 2), "b_urban_grass" = runif(1, -2, 2), "b_urban_crop" = runif(1, -2, 2),
                  "b_forest_grass" = runif(1, -2, 2), "b_forest_crop" = runif(1, -2, 2), "b_grass_crop" = runif(1, -2, 2),
                  "b_wet_urban" = runif(1, -2, 2), "b_wet_forest" = runif(1, -2, 2), "b_wet_grass" = runif(1, -2, 2),
                  "b_wet_crop" = runif(1, -2, 2),
                  
                  # linear*quadratic interaction params
                  "b_urban2_forest" = runif(1, -1, 1), "b_urban2_grass" = runif(1, -1, 1), "b_urban2_wet" = runif(1, -1, 1),
                  "b_urban2_crop" = runif(1, -1, 1), "b_forest2_urban" = runif(1, -1, 1), "b_forest2_crop" = runif(1, -1, 1),
                  "b_forest2_grass" = runif(1, -1, 1), "b_forest2_wet" = runif(1, -1, 1), "b_grass2_urban" = runif(1, -1, 1),
                  "b_grass2_forest" = runif(1, -1, 1), "b_grass2_crop" = runif(1, -1, 1), "b_grass2_wet" = runif(1, -1, 1),
                  "b_crop2_urban" = runif(1, -1, 1), "b_crop2_forest" = runif(1, -1, 1), "b_crop2_grass" = runif(1, -1, 1), 
                  "b_crop2_wet" = runif(1, -1, 1), "b_wet2_urban" = runif(1, -1, 1), "b_wet2_forest" = runif(1, -1, 1),
                  "b_wet2_grass" = runif(1, -1, 1), "b_wet2_crop" = runif(1, -1, 1),
                  
                  # delay params
                  "c_pos_urban" = runif(1, 0, 1), "c_pos_forest" = runif(1, 0, 1), "c_pos_grass" = runif(1, 0, 1),
                  "c_pos_crop" = runif(1, 0, 1), "c_pos_wet" = runif(1, 0, 1), #"c_pos_lc_q1" = runif(1, 0, 1),
                   "c_neg_forest" = runif(1, 0, 1), "c_neg_grass" = runif(1, 0, 1), #"c_neg_urban"  = runif(1, 0, 1), 
                  "c_neg_crop" = runif(1, 0, 1), "c_neg_wet" = runif(1, 0, 1), #"c_neg_lc_q1" = runif(1, 0, 1),
                  
                  # random effects and other params
                  "sigma_observer" = runif(1, 0, 2), #"sigma_route" = runif(1, 0, 2),
                  "b_time" = runif(1, -2, 2),   "b_lc_q1" = runif(1, 0, 2), #"b2_lc_q1" = runif(1, -2, 0),
                  "b_temp" = runif(1, 0, 2), "b2_temp" = runif(1, -2, 0) 
                  )) 
    
    
    result <- model.q1$sample(data = data.list,
                              init = inits.list,
                              refresh = 10,
                              chains = 4, parallel_chains = 4, #threads_per_chain = 2,
                              iter_warmup = 500,
                              iter_sampling = 1000,
                              adapt_delta = 0.8, 
                              max_treedepth = 10,
                              save_warmup = TRUE)
    
    result$save_output_files(dir = "USBBS_modelling/1_model_fitting/stan_model_fitted", basename = paste0("files_", f, "_", buffer.type,"_",buffer.size), random=F, timestamp=F)
    result$save_object(file = paste0("USBBS_modelling/1_model_fitting/stan_model_fitted/sampled_", f, "_", buffer.type,"_",buffer.size,".RDS"))
    
    }
  }  
}

