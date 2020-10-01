#'
#' *COMPARE EXPECTED DEBTs/CREDITs WITH SR IN 2018*
#'
#'
#' *Import USBBS data for 2018 and clean it*
#'

library(readxl)
library(foreach)
library(tidyverse)
library(reshape2)
library(vegan)
library(RColorBrewer)

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
usbbs.18 <- head(usbbs.all.1,0)

for (usbbs.temp in usbbs.all) {
  t.usbbs <- usbbs.temp %>% filter(Year==2018 )
  usbbs.18<- rbind(usbbs.18, t.usbbs)
}

rm(usbbs.temp, t.usbbs, usbbs.all, usbbs.all.1, usbbs.all.2, usbbs.all.3, usbbs.all.4, usbbs.all.5, usbbs.all.6)

# keep only observation of high quality
usbbs.18 <- usbbs.18 %>% 
  filter(RPID==101) %>%
  filter(StopTotal >= 0 & StopTotal <= 50)

summary(usbbs.18)           

# set route and state string value to same length as shapefile for matching
usbbs.18$StateNum <- str_pad(usbbs.18$StateNum, width=2, side="left", pad="0")  
usbbs.18$Route <- str_pad(usbbs.18$Route, width=3, side="left", pad="0")

usbbs.18$U_S_R_I <- paste0(usbbs.18$StateNum, "_", usbbs.18$Route)

usbbs.18 # have a look at that tibble

#'#####################################################
#'  *keep only routes with data in both t2 and 2018*

load("USBBS_Modelling_Delays/data.withpred.centroid_springtemp.rda")

routeid <- unique(gsub('.{2}$', '', data$partition)) # remove last character and unique route name

usbbs.18 <- usbbs.18 %>% filter(U_S_R_I %in% routeid)

#write.csv(usbbs.18, "USBBS_Modelling_Delays/USBBS.18.csv", row.names=F)
usbbs.18 <- read.csv("USBBS_Modelling_Delays/USBBS.18.csv")

#'###############################################
#'
#' *Clean raw sp data for sp matrix*
#'

usbbs.18 <- usbbs.18 %>%
  filter(SpeciesTotal>-1) %>% # make sure they are all positive
  rename(species=AOU, year=Year, count=SpeciesTotal, partition=U_S_R_I) %>%
  mutate_if(is.factor, as.character) %>%
  select(-CountryNum, -StateNum, -RPID, -RouteDataID, -Route)

# set up loop
segments <- c("Count10", "Count20", "Count30", "Count40", "Count50")
i <- 1
alpha.18<- data.frame("partition"=0, "q0.18"=0)

##########################################
#
#' *generate sp matrices for segments* 
#' *chance years between 01 and 16*
#

segment.now <- "Count10" 

for(segment.now in segments) {
  
  sp.matrix.now <- usbbs.18 %>%
    dcast(partition ~ species, value.var = segment.now, fill = 0) %>% 
    as_tibble() 
  
  # modify partition name
  sp.matrix.now$partition <- paste(sp.matrix.now$partition,i)
  
  alpha.18.now <- data.frame("partition" = sp.matrix.now$partition, 
                          "q0.18"=rowSums(sp.matrix.now[,-1]>0))
    alpha.18 <- rbind(alpha.18, alpha.18.now)
  
  #update partition segment number 
  i <- i+1
  
}

#' Write the species matrices out into R files
#' 

alpha.18 <- alpha.18[-1,] # remove initializing 00 line

#save(alpha.18, file="USBBS_Modelling_Delays/alpha_18.rda")
load("USBBS_Modelling_Delays/alpha_18.rda")

alpha.18$partition <- str_replace(alpha.18$partition, pattern=" ", replacement="_")

data <- merge(data, alpha.18, by="partition")


save(data, file="USBBS_Modelling_Delays/data.withpred.18.centroid_springtemp.rda")


###################################################################################################################################
###################################################################################################################################



