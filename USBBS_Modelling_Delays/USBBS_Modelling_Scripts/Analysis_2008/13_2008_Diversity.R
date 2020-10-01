#'
#' *Process and calculate diversities for USA BBS at middle timepoint 2008*
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
usbbs.08 <- head(usbbs.all.1,0)

for (usbbs.temp in usbbs.all) {
  
  t.usbbs <- usbbs.temp %>% filter(Year==2008 | Year==2007 | Year==2009 )
  
  usbbs.08 <- rbind(usbbs.08, t.usbbs)
  
}

# keep only observation of high quality
usbbs.08 <- usbbs.08 %>% 
  filter(RPID==101) %>%
  filter(StopTotal >= 0 & StopTotal <= 50)

summary(usbbs.08)           

usbbs.18 # have a look at that tibble

write.csv(usbbs.08, "USBBS_Modelling_Scripts/one more time/USBBS.08.csv", row.names=F)

usbbs.routes <- read.csv("D:/USBBS_DATA/USBBS_Diversity/routes.csv") 

usbbs.routes <- usbbs.routes %>% select(StateNum, Route, RouteName, RouteTypeDetailID) %>%
  mutate_if(is.factor, as.character)

usbbs.08$RouteName <- 0

for(i in 1:nrow(usbbs.08)){
  
  for(j in 1:nrow(usbbs.routes)){
    
    if(usbbs.08$StateNum[i] == usbbs.routes$StateNum[j])
      if(usbbs.08$Route[i] == usbbs.routes$Route[j])
        usbbs.08$RouteName[i] <- usbbs.routes$RouteName[j]
  }
  
  # fancy progress bar
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

route <- data.frame(name = usbbs.routes$RouteName, RouteName = as.numeric(usbbs.routes$RouteName) ) 

a <- usbbs.08

a <- merge(a, route, by= "RouteName" )

a$RouteName <- a$name 

usbbs.08 <- a %>% select(-RouteNamea, -name) 


#' *keep only routes with data in both 2001,2016 timepoints and data in 2008 middle timepoint*

load("data.withpred.rda")

routeid <- unique(gsub('.{2}$', '', data$partition)) # remove last character and unique route name

usbbs.08 <- usbbs.08 %>% filter(RouteName %in% routeid)

unique(usbbs.08$RouteName)

write.csv(usbbs.08, "USBBS_Modelling_Scripts/one more time/USBBS.08.csv")

usbbs.08 <- read.csv("USBBS_Modelling_Scripts/one more time/USBBS.08.csv")

#'###############################################
#'
#' *Clean raw sp data for sp matrix*
#'

usbbs.08 <- usbbs.08 %>%
  filter(SpeciesTotal>-1) %>% # make sure they are all positive
  rename(species=AOU, year=Year, count=SpeciesTotal, partition=RouteName) %>%
  mutate_if(is.factor, as.character) %>%
  select(-CountryNum, -StateNum, -RPID, -RouteDataID, -Route, -X)

# set up loop
segments <- c("Count10", "Count20", "Count30", "Count40", "Count50")
i <- 1
alpha.08<- data.frame("partition"=0, "q0.08"=0, "even.08"=0)

##########################################
#
#' *generate sp matrices for segments* 
#' *chance years between 01 and 16*
#

segment.now <- "Count10" 

for(segment.now in segments) {
  
  sp.matrix.now <- usbbs.08 %>%
    dcast(partition ~ species, fun.aggregate = max, value.var = segment.now, fill = 0) %>% 
    as_tibble() 
  
  # modify partition name
  sp.matrix.now$partition <- paste(sp.matrix.now$partition,i)
  rownames(sp.matrix.now) <- sp.matrix.now$partition
  
  sp.matrix.now <- sp.matrix.now %>% select(-partition) %>% as.matrix()
  
  alpha.08.now <- data.frame("partition" = rownames(sp.matrix.now), 
                             "q0.08" = rowSums(sp.matrix.now>0),
                             "even.08" = vegan::diversity(sp.matrix.now, "shannon")/log(rowSums(sp.matrix.now>0)))
    
  alpha.08 <- rbind(alpha.08, alpha.08.now)
  
  #update partition segment number 
  i <- i+1
  
}

#' Write the species matrices out into R files
#' 

alpha.08 <- alpha.08[-1,] # remove intializing 00 line
rownames(alpha.08) <- 1:nrow(alpha.08)

save(alpha.08, file="USBBS_Modelling_Scripts/one more time/alpha_08.rda")
load("USBBS_Modelling_Scripts/one more time/alpha_08.rda")


