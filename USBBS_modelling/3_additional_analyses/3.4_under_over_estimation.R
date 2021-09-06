library(tidyverse)

load("USBBS_data/map_predict_data/segment_predicted_debtcredit.rda")

#'-----------------------------------------------------------------------
#' *Calculate percentage over/under estimation*
#'

data <- data %>% select(q1_t2, eq_predicted_mean, delay_predicted_mean, debtcredit, debtcredit_delay_eq, debtcredit_t2_eq) %>%
  mutate(debtcredit = eq_predicted_mean - delay_predicted_mean) %>%
  mutate(overunder_estim = debtcredit*100/delay_predicted_mean) %>% na.omit() 

#'*debt estimate*
summary(data[data$debtcredit<0,])
sd(data$overunder_estim[data$debtcredit<0])

#'*credit estimate*
summary(data[data$debtcredit>0,])
sd(data$overunder_estim[data$debtcredit>0],)

install.packages("installr")
library(installr)
installr::updateR()

