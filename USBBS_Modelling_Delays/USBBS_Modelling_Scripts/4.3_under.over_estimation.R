library(tidyverse)
library(rgdal)

shp.xy <- readOGR("D:/USBBS_DATA/USBBS_LandCover/usaPredict_centroid.shp")
data <- shp.xy@data

#############################################################################
#'
#' *Calculate percentage over/under estimation*
#'

data <- data %>% transmute(partitn, 
                           q0.eq.t2 = q0_eq,
                           q0.lag.t2 = q0_lag) %>%
  mutate(delta.debtcredit = q0.eq.t2 - q0.lag.t2) %>%
  mutate(overunder.estim = delta.debtcredit*100/q0.lag.t2) %>% na.omit() 
  #mutate(overunder.estim = q0.eq.t2/q0.obs.t2) 

mean(data$q0.obs.t2)
sd(data$q0.obs.t2)

#'*debt estimate*
summary(data[data$delta.debtcredit<0,])
sd(data$overunder.estim[data$delta.debtcredit<0])

#'*credit estimate*
summary(data[data$delta.debtcredit>0,])
sd(data$overunder.estim[data$delta.debtcredit>0],)

install.packages("installr")
library(installr)
installr::updateR()
