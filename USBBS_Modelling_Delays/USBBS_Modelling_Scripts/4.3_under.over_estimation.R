library(tidyverse)
library(stringr)

load("USBBS_Modelling_Delays/data.withpred.notemplag.rda")
load("USBBS_Modelling_Delays/alpha_18.rda")
alpha.18$partition <- str_replace(alpha.18$partition, pattern=" ", replacement="_")

data <- merge(data, alpha.18, by="partition")

#############################################################################
#'
#' *Calculate percentage over underestimation*
#'

data <- data %>% transmute(partition, 
                           q0.eq.t2 = q0.eq,
                           q0.obs.t2 = q0.t2) %>%
  mutate(delta.debtcredit = q0.eq.t2 - q0.obs.t2) %>%
  mutate(overunder.estim = delta.debtcredit*100/q0.obs.t2) %>% 
  mutate(overunder.estim = q0.eq.t2/q0.obs.t2) %>% na.omit()

mean(data$q0.obs.t2)
sd(data$q0.obs.t2)

#'*debt estimate*
summary(data[data$delta.debtcredit<0,])

#'*credit estimate*
summary(data[data$delta.debtcredit>0,])
