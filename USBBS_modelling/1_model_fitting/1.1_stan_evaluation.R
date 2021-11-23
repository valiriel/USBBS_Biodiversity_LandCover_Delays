library("rstan");library("loo"); library("cmdstanr"); library(posterior); library(bayesplot); library(ggthemes); library(gridExtra)
options(mc.cores = parallel::detectCores()); color_scheme_set("viridisE")
bayesplot_theme_set(new = theme_clean());

#'-----------------------------------------------------------------------------------------------------
#'* EVALUATING model performance, convergence etc...*
#'

#load the model in
# 500 seems to be the best
f <- "mean"; buffer.size <- "500"; buffer.type <- "segment"; file <- 3
result <- readRDS(paste0("USBBS_modelling/1_model_fitting/stan_model_fitted/sampled_", f, "_", buffer.type,"_",buffer.size,".RDS"))


f <- "mean"; buffer.size <- "2000"; buffer.type <- "segment"; file <- 3
result2 <- readRDS(paste0("USBBS_modelling/1_model_fitting/stan_model_fitted/sampled_", f, "_", buffer.type,"_",buffer.size,".RDS"))

f <- "mean"; buffer.size <- "4000"; buffer.type <- "segment"; file <- 3
result3 <- readRDS(paste0("USBBS_modelling/1_model_fitting/stan_model_fitted/sampled_", f, "_", buffer.type,"_",buffer.size,".RDS"))

f <- "mean"; buffer.size <- "4000"; buffer.type <- "centroid"; file <- 3
result4 <- readRDS(paste0("USBBS_modelling/1_model_fitting/stan_model_fitted/sampled_", f, "_", buffer.type,"_",buffer.size,".RDS"))

params <- names(result$init()[[1]])[-55]

summary1 <- result1$summary(params)
summary2 <- result$summary(params)
summary3 <- result3$summary(names(result3$init()[[1]]))
summary4 <- result4$summary(names(result4$init()[[1]]))

# calculate WAIC-LOO
loo <- result$loo(cores=11)

#' *sampling.landscape* |  *looic*
#' segment.500          |  17981.91
#' segment.2000         |  18112.72
#' segment.4000         |  18274.86
#' centroid.4000        |  18198.96

# see sampling times
result$time()

# posterior distros
mcmc_dens_overlay(result$draws(params))

# parameters intervals
mcmc_intervals(result$draws(params))

# trace plots
mcmc_trace(result$draws(params))

# chains autocorrelation 
mcmc_acf(result$draws("sigma_observer"), lags = 10) #params

mcmc_rhat(rhat(result, pars=params))
mcmc_neff(neff_ratio(result, pars=params))

mcmc_nuts_treedepth(nuts_params(result), log_posterior(result))
mcmc_nuts_acceptance(nuts_params(result), log_posterior(result))
mcmc_nuts_divergence(nuts_params(result), log_posterior(result))
mcmc_nuts_energy(nuts_params(result), log_posterior(result))
mcmc_nuts_stepsize(nuts_params(result), log_posterior(result))

