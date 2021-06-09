library(tidyverse); library(reshape2); library(magrittr); library(summarytools); library(sf); library(stringr)

##########################################
#
#' *generate species matrices for segments* 
#

for (file in 2:3) {

  load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/USBBS_years_subset.rda"))
  
  # get list of available partitions from routes/segment shp
  shp <- as.data.frame(read_sf(paste0("USBBS_data/routes_data/5_segments_final_", file, "years.shp"))) %>% 
    transmute(partition = str_sub(partition,1, 6)) %>% unique() 
  
  usbbs.data <- as_tibble(usbbs.data) %>% 
    filter(SpeciesTotal>-1) %>% filter(RPID==101) %>% # make sure they are all positive
    transmute(year=Year, species=AOU, partition = U_S_R_I,
              Count10, Count20, Count30, Count40, Count50) %>% 
    filter(partition %in% shp$partition)
  
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
      
      if(y==2001)
        usbbs.temp <- usbbs.data %>% filter( year < 2007 )
      if(y==2016)
        usbbs.temp <- usbbs.data %>% filter( year > 2007 )  
        
      for(segment.now in segments) {
      
      sp.matrix.now <- usbbs.temp %>%
                          dcast(partition ~ species, fun.aggregate = fun.now,  value.var = segment.now, fill = 0) %>% 
                           as_tibble() 
      
      # modify partition name
      sp.matrix.now$partition <- paste0(sp.matrix.now$partition,"_",i)
      
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
        save(spmatrix.t1, file=paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/vegan_spmatrix_t1_", fun.names[n], ".rda"))
      } else{
        spmatrix.t2 <- as.matrix(sp.matrix[,-1])
        rownames(spmatrix.t2) <- sp.matrix$partition
          save(spmatrix.t2, file=paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/vegan_spmatrix_t2_", fun.names[n], ".rda"))
      }
    }
    
    n <- n+1 # increment in naming vector
    
  }

}
