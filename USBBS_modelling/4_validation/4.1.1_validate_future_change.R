library(tidyverse); library(ggthemes)

theme <- theme_clean() + 
  theme(axis.title=element_text(size=16), axis.text=element_text(size=13),
        legend.text=element_text(size=12), legend.title=element_text(size=13), 
        plot.background = element_rect(color = "white"), 
        panel.border= element_blank(), plot.title=element_text(size=14),
        panel.grid.major.y = element_line(size=1, linetype = "dotted", colour = "grey90"),  
        panel.grid.major.x = element_line(size=1, linetype = "dotted", colour = "grey90"),
        legend.background = element_rect(color = NA),
        strip.background.y = element_rect(color = "grey30"))

load("USBBS_data/map_predict_data/segment_predicted_debtcredit.rda")
data <- data %>% transmute(partition, q0_t2, q1_t2, delay_predicted_mean, eq_predicted_mean, debtcredit_t2_eq, debtcredit_delay_eq); data

load("USBBS_data/diversity_data/validation_data.rda"); validation_alpha

data <- left_join(validation_alpha, data, by = "partition"); data

#'-----------------------------------------------------------------------------------------------------
#' * validation based on q1_valid - q1_t2 against debtcredit from delay_pred - eq_pred  *
#' 

data$delta_q1_validation <- data$q1_validation - data$q1_t2; data
data$debtcredit <- data$eq_predicted_mean - data$q1_t2; data
#data$debtcredit <- data$eq_predicted_mean - data$delay_predicted_mean; data

t <- cor.test(data$debtcredit, data$delta_q1_validation); t
round(as.numeric(t$estimate)^2,2) # explained variance

p <- ggplot(data, aes(x = debtcredit, y = delta_q1_validation)) +
  geom_point(alpha = 0.25, colour = "black", size=1) +  
  geom_smooth(size=1.5, colour="black", alpha=0.1, method="glm") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  labs(x="Forecasted debts and credits in 2016", y="Observed \u0394 effective number of species between 2019 and 2016") +
  #scale_y_continuous(limits = c(-30, 30)) + 
  #scale_x_continuous(limits = c(-30, 30)) +
  theme
ggsave(plot = p, file = "USBBS_output/modelling_output/supplementary_figures/debtcredit_obs_change_validation.svg",
       units = "cm", dpi = "retina", width =20, height = 20)
ggsave(plot = p, file = "USBBS_output/modelling_output/supplementary_figures/debtcredit_obs_change_validation.png",
       units = "cm", dpi = "retina", width =20, height = 20)

