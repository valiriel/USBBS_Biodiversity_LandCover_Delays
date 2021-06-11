library(tidyverse); library(ggthemes)

# load in debt credit predicted on usa landscape with credible intervals from 1000 posterior draws
load("USBBS_data/map_predict_data/USA_landscape_predicted.rda"); 
usa_landscape$debtcredit_mean[is.na(usa_landscape$debtcredit_mean)] <-0; usa_landscape$debtcredit_sd[is.na(usa_landscape$debtcredit_sd)] <- 0

#'----------------------------------------------------
#' *scatterplots*

p1 <- ggplot(usa_landscape, aes(x=delta_urban, y = debtcredit_mean)) +
        geom_point(color="grey50") +
          geom_smooth(color="black", method="glm") + 
          theme_clean()
ggsave(plot = p1, file = "USBBS_output/modelling_output/understanding_delays/delay_urban_scatter.png", units = "cm", dpi = "retina", width =20, height = 20)

p1 <- ggplot(usa_landscape, aes(x=delta_forest, y = debtcredit_mean)) +
        geom_point(color="forestgreen") +
          geom_smooth(color="black", method="glm") + 
          theme_clean()
ggsave(plot = p1, file = "USBBS_output/modelling_output/understanding_delays/delay_forest_scatter.png", units = "cm", dpi = "retina", width =20, height = 20)

p1 <- ggplot(usa_landscape, aes(x=delta_grass, y = debtcredit_mean)) +
        geom_point(color="limegreen") +
          geom_smooth(color="black", method="glm") + 
          theme_clean()
ggsave(plot = p1, file = "USBBS_output/modelling_output/understanding_delays/delay_grass_scatter.png", units = "cm", dpi = "retina", width =20, height = 20)

p1 <- ggplot(usa_landscape, aes(x=delta_crop, y = debtcredit_mean)) +
        geom_point(color="goldenrod3") +
          geom_smooth(color="black", method="glm") + 
          theme_clean()
ggsave(plot = p1, file = "USBBS_output/modelling_output/understanding_delays/delay_crop_scatter.png", units = "cm", dpi = "retina", width =20, height = 20)

p1 <- ggplot(usa_landscape, aes(x=delta_wet, y = debtcredit_mean)) +
        geom_point(color="dodgerblue3") +
          geom_smooth(color="black", method="glm") + 
          theme_clean()
ggsave(plot = p1, file = "USBBS_output/modelling_output/understanding_delays/delay_wet_scatter.png", units = "cm", dpi = "retina", width =20, height = 20)

#'----------------------------------------------------
#' *counts*

lower_debtcredit <- usa_landscape %>%
  filter(debtcredit_mean < as.numeric(quantile(usa_landscape$debtcredit_mean, prob=0.25))) %>%
  arrange(debtcredit_mean)

upper_debtcredit <- usa_landscape %>%
  filter(debtcredit_mean > as.numeric(quantile(usa_landscape$debtcredit_mean, prob=0.75))) %>%
  arrange(desc(debtcredit_mean))

upper_debtcredit$debtcredit_mean
lower_debtcredit$debtcredit_mean

