library(tidyverse); library(ggthemes); library(foreach); library(tictoc); library(doParallel)

# load in debt credit predicted on usa landscape from 1000 posterior draws
load("USBBS_data/map_predict_data/USA_predicted_delay_draws_matrix.rda"); delay_matrix <- prediction.matrix
load("USBBS_data/map_predict_data/USA_predicted_eq_draws_matrix.rda"); eq_matrix <- prediction.matrix
load("USBBS_data/map_predict_data/USA_landscape_predicted.rda")

debtcredit_matrix <- eq_matrix - delay_matrix; rm(prediction.matrix, eq_matrix, delay_matrix)

dim(debtcredit_matrix) # 1000 rows = draws, 92027 cols = landscapes

# keep what's needed, to be joined back with the draw
usa_landscape <- usa_landscape %>% transmute(debtcredit_mean,
                                             delta_urban, delta_forest, delta_grass, delta_crop, delta_wet)

usa_landscape <- usa_landscape %>% transmute(debtcredit_mean,
                                              delta_urban = abs(delta_urban), delta_forest = abs(delta_forest), 
                                              delta_grass = abs(delta_grass), delta_crop = abs(delta_crop), 
                                              delta_wet = abs(delta_wet))

# explore what the model would be with mean values (i.e. the mapped ones)
model_mean <- glm(debtcredit_mean ~ delta_urban + delta_forest + delta_grass + delta_crop + delta_wet, data=usa_landscape)
model_mean <- step(model_mean); summary_model_mean <- summary(model_mean); summary(model_mean); anova(model_mean)

#'---------------------------------------------------------------------------------------------
#' * 1 - draw from deb_credit credible intervals, each row of matrix joined back to change vars *
#' * 2 - fit model to those values, then draw a 100 times from parameter distro *
#' * 3 - get mean estimates and credible intervals for those parameters *

n_draws_response <- nrow(debtcredit_matrix)
n_draws_param <- 1000

registerDoParallel(cores=10)
tic()
draws <- foreach(i = 1:n_draws_response, .combine='rbind') %dopar% {
  
  #' * draw from sd of each debt and credit *
  usa_landscape$draw <- debtcredit_matrix[i,]

  #' * fit model *
  model <- glm(draw ~ delta_urban + delta_forest + delta_grass + delta_crop + delta_wet, data=usa_landscape)
  summary <- summary(model)
  
  #' * draw from parameter distro *
  draws_now <- data.frame(intercept = rnorm(n_draws_param, summary$coefficients[1, 1], summary$coefficients[1, 2]),
                      delta_urban = rnorm(n_draws_param, summary$coefficients[2, 1], summary$coefficients[2, 2]),
                      delta_forest = rnorm(n_draws_param, summary$coefficients[3, 1], summary$coefficients[3, 2]),
                      delta_grass = rnorm(n_draws_param, summary$coefficients[4, 1], summary$coefficients[4, 2]),
                      delta_crop = rnorm(n_draws_param, summary$coefficients[5, 1], summary$coefficients[5, 2]),
                      delta_wet = rnorm(n_draws_param, summary$coefficients[6, 1], summary$coefficients[6, 2]))
}

toc()
registerDoSEQ()

#'---------------------------------------------------------------------------------------------
#' * plotting *

parameter_distro <- tibble(param = c("Intercept", "Urban change", "Forest change", "Grassland change", "Cropland change", "Wetland change"),
                           estimate = c(mean(draws$intercept), mean(draws$delta_urban), mean(draws$delta_forest), mean(draws$delta_grass),
                                        mean(draws$delta_crop), mean(draws$delta_wet)),
                           sd = c(sd(draws$intercept), sd(draws$delta_urban), sd(draws$delta_forest), sd(draws$delta_grass),
                                  sd(draws$delta_crop), sd(draws$delta_wet)),
                           lower_CI = c(mean(draws$intercept) - 1.96* sd(draws$intercept), mean(draws$delta_urban) - 1.96* sd(draws$delta_urban),
                                        mean(draws$delta_forest) - 1.96* sd(draws$delta_forest), mean(draws$delta_grass) - 1.96* sd(draws$delta_grass),
                                        mean(draws$delta_crop) - 1.96* sd(draws$delta_crop), mean(draws$delta_wet) - 1.96* sd(draws$delta_wet)),
                           upper_CI = c(mean(draws$intercept) + 1.96* sd(draws$intercept), mean(draws$delta_urban) + 1.96* sd(draws$delta_urban),
                                        mean(draws$delta_forest) + 1.96* sd(draws$delta_forest), mean(draws$delta_grass) + 1.96* sd(draws$delta_grass),
                                        mean(draws$delta_crop) + 1.96* sd(draws$delta_crop), mean(draws$delta_wet) + 1.96* sd(draws$delta_wet)))

p1 <- parameter_distro %>% 
      mutate(param = fct_reorder(param, estimate)) %>%
          ggplot(aes(y = param, x = estimate, colour = param)) +
          geom_point(size=4) + 
          #geom_text(aes(label= round(estimate,2)),hjust=0, vjust=-1) +
          geom_linerange(aes(xmax = upper_CI, xmin= lower_CI), size=1.25) +
          #scale_colour_manual(values = c("gray38", "goldenrod2", "limegreen", "darkgreen", "black", "cornflowerblue")) +
          scale_colour_manual(values = c("gray38", "darkgreen", "goldenrod2", "cornflowerblue", "limegreen","black")) +
          geom_vline(xintercept=0) +
            labs(y="", x="Estimate") +
            theme_clean(base_size = 15) + 
            theme(legend.position='none',
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  axis.text=element_text(size=15), axis.title=element_text(size=15),
                  plot.background = element_rect(color = "white"),
                  panel.grid.major.y = element_line(size=0.2),
                  axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave(plot=p1, file = "USBBS_output/modelling_output/understanding_delays/nodirection_abs_model.svg",
       units = "cm", dpi = "retina", width =20 , height = 21)


