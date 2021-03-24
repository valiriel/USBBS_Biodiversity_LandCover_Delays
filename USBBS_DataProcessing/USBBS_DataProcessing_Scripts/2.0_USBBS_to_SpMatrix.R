library(tidyverse)
library(reshape2)
library(magrittr)
library(summarytools)

#'###############################################
#'
#' *Clean raw sp data for sp matrix*
#'
load("USBBS_DataProcessing/USBBS_00.01_15.16.rda")

usbbs.data <- as_tibble(usbbs.data) %>%
  filter(SpeciesTotal>-1) %>% # make sure they are all positive
  rename(species=AOU, year=Year, count=SpeciesTotal, partition=U_S_R_I) %>%
  mutate_if(is.factor, as.character)

# Check descriptive stats values 
view(summarytools::descr(usbbs.data))

##########################################
#
#' *generate sp matrices for segments* 
#' *change years between 01 and 16*
#

years <- c(2001, 2016) # insert your timepoint
fun.vector <- c(min, mean, max)
fun.names <- c("min", "mean", "max")
n <- 1 # position in function aggregate naming vector

for (fun.now in fun.vector) {

  for(y in years){
  
  # set up loop
  segments <- c("Count10", "Count20", "Count30", "Count40", "Count50")
  i <- 1
  new.matrix <- TRUE
    
  for(segment.now in segments) {
  
  sp.matrix.now <- usbbs.data %>%
                filter( year >= (y-1) & year < (y+1)) %>% # select timepoint and year before
                      dcast(partition ~ species, fun.aggregate = fun.now,  value.var = segment.now, fill = 0) %>% 
                       as_tibble() 
  
  # modify partition name
  sp.matrix.now$partition <- paste(sp.matrix.now$partition,i)
  
  if(new.matrix==T)
    sp.matrix <- sp.matrix.now else
      sp.matrix <- rbind(sp.matrix, sp.matrix.now) # add on each other
  
  new.matrix <- FALSE
  
  #update partition segment number 
  i <- i+1
  
  }
  if(y == 2001) {
    spmatrix.t1 <- as.matrix(sp.matrix[,-1])
    rownames(spmatrix.t1) <- sp.matrix$partition
    save(spmatrix.t1, file=paste0("USBBS_DataProcessing/vegan_spmatrix_t1_", fun.names[n], ".rda"))
  } else{
    spmatrix.t2 <- as.matrix(sp.matrix[,-1])
    rownames(spmatrix.t2) <- sp.matrix$partition
      save(spmatrix.t2, file=paste0("USBBS_DataProcessing/vegan_spmatrix_t2_", fun.names[n], ".rda"))
  }
  }
  
  n <- n+1 # increment in naming vector
  
}
