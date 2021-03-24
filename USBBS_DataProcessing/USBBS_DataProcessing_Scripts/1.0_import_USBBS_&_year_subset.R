#'
#' *Import all USBBS data raw and keep only routes with values around*
#' *timepoints and excellent survey quality RPID 101*
#'

library(readxl)
library(foreach)
library(tidyverse)

# Import the multiple xlxs sheets containing all the USBBS data from 1966 to 2018
usbbs.all.1 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=1)
usbbs.all.2 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=2)
usbbs.all.3 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=3)
usbbs.all.4 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=4)
usbbs.all.5 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=5)
usbbs.all.6 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=6)

# make a list with all the usbbs files
usbbs.all <- list(usbbs.all.1, usbbs.all.2, usbbs.all.3, usbbs.all.4, usbbs.all.5, usbbs.all.6)

# setup empty df
usbbs.data <- head(usbbs.all.1,0)

# pick years we want to extract
yrs <- c(2000, 2001, 2002, 2015, 2016, 2017)

for (usbbs.temp in usbbs.all) {
  
  t.usbbs <- usbbs.temp %>% filter(Year==yrs[1] | Year==yrs[2] | Year==yrs[3] | 
                                     Year==yrs[4] | Year==yrs[5] | Year==yrs[6])
                      
  usbbs.data <- rbind(usbbs.data, t.usbbs)
  
}

# keep only observation of high quality, (good weather etc.), standard set by USBBS guidelines
usbbs.data <- usbbs.data %>%  filter(RPID==101)

usbbs.data # have a look at that tibble

#write.csv(usbbs.data, "USBBS_DataProcessing/USBBS_subset.csv", row.names=F)
save(usbbs.data, file="USBBS_DataProcessing/USBBS_subset.rda")
