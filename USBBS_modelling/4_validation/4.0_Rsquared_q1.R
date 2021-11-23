library(tidyverse); library(ggthemes); library(summarytools)

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

cor.test(data$q1_t2, data$eq_predicted_mean)
cor.test(data$q1_t2, data$delay_predicted_mean)

summary(lm(q1_t2 ~ eq_predicted_mean, data))

round(as.numeric(cor.test(data$q1_t2, data$delay_predicted_mean)$estimate)^2,2) # explained variance

p <- ggplot(data, aes(x = delay_predicted_mean, y = q1_t2)) +
      geom_point(alpha = 0.25, colour = "black", size=1) +  
      geom_smooth(size=1.5, colour="black", alpha=0.1, method="glm") +
        #geom_abline(intercept=0, slope=1) +
        #geom_rug(alpha=0.3) +
        labs(x="Predicted effective number of species in 2016", y="Observed effective number of species in 2016") +
        scale_y_continuous(limits = c(0, 55)) + 
        scale_x_continuous(limits = c(0, 55)) +
        theme
ggsave(plot = p, file = "USBBS_output/modelling_output/supplementary_figures/pred_obs_corr.svg",
       units = "cm", dpi = "retina", width =20, height = 20)
ggsave(plot = p, file = "USBBS_output/modelling_output/supplementary_figures/pred_obs_corr.png",
       units = "cm", dpi = "retina", width =20, height = 20)

load("USBBS_data/whole_data/3years_x_timepoint/data_mean_segment_500.rda")

sd(data$delta_neg_grass)

view(descr(data))
summary(data)
