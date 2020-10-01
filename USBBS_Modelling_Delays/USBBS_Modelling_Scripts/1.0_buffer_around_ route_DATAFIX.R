library(tidyverse)
library(stringr)

options(mc.cores = parallel::detectCores())

#########################################################################################################################
#' *DATA IMPORT & MERGE of Diversity and Covariates*

load("USBBS_DataProcessing/covariates.rda")
# data from 4km buffer around route, remove temp as it is whole year 
data.buffer <- covariates %>% select(-temp.t1, - temp.t2, -rain.t1, -rain.t2, 
                                     -heatmoist.t1, -heatmoist.t2, -delta.heatmoist,
                                     -delta.temp, - delta.rain)

load("USBBS_DataProcessing/alpha.01.16.rda")
alphas$partition <- str_replace(alphas$partition, pattern=" ", replacement="_")

load("USBBS_DataProcessing/climate.buffer.springtemp.rda")
climate.buffer.springtemp <-climate.buffer.springtemp[,-1]

data <- merge(data.buffer, alphas, by="partition") 

data <- merge(data, climate.buffer.springtemp, by="partition") 

data$delta.temp <- data$tmean.t2 - data$tmean.t1

save(data, file="data.buffer_springtemp.rda") # final version of the data to analyze, 5690 segments, 1138 routes
