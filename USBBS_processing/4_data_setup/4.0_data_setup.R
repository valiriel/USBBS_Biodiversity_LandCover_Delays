
library(tidyverse); library(stringr); library(dplyr)

# alpha diversities calculated for min, mean, max abundances
fun.names <- c("min", "mean", "max")
# buffer sizes and centered around segment centroid or line
buffer.sizes <- c(500, 1000, 2000, 4000, 6000) 
buffer.types <- c("centroid", "segment")

f <- "min"; buffer.size <- "500"; buffer.type <- "centroid"; file <- 3

#'------------------------------------------------------------------------------------------------------------------

for (file in 2:3) {
  
  for (f in fun.names) {
    
    for (buffer.size in buffer.sizes) {
      
      for(buffer.type in buffer.types) {
        
        #'-----------------------------------------------------------------------------------------------------
        #'* data import and joins*
        
        # load in diversity measures
        load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/alpha_", f,".rda"))
        alphas <- alphas %>% filter(route != "53_800")
        
        # load in landcover percentages and change
        load(paste0("USBBS_data/landscape_data/landcover_data/landcover_%_&_delta_extracted/lc_", buffer.type, "_", buffer.size, ".rda"))
        lc <- as.data.frame(lc)[-c(651:670),] ; lc$partition[lc$route=="17_222"] <- paste0("17_222_",1:5) 
        lc <- lc %>% dplyr::select(-geometry, -route) %>% as_tibble()
        
        # load in climatic variables
        load(paste0("USBBS_data/landscape_data/climate_elev_data/climate_elev_extracted/climate_elev_", buffer.type,"_",buffer.size,".rda")); 
        climate.data[is.na(climate.data)] <- 0 
        
        data <- left_join(alphas, lc, by = "partition")
        data <- left_join(data, climate.data, by = "partition")
        
        data <- as_tibble(data) %>% arrange(partition)
        data[is.na(data)] <- 0

        # add segment ID 1 to 5
        data$segmentID <- as.factor(str_sub(data$partition, 8, 8))
        
        # vector for route random effect 
        data$routeID <- as.factor(data$route)

        # vector for observer random effect, as routes are sampled by different individuals but some by the same
        data$observerID <- as.factor(data$observer.t2)
        
        # save whole dataset for future
        save(data, file = paste0("USBBS_data/whole_data/", file, "years_x_timepoint/data_", f, "_", buffer.type,"_",buffer.size,".rda"))
        
        #'-------------------------------------------------------------------------------
        #'*Keep only observation for segment 1 _ 3 _ 5*
        #'
        
        data <- data %>% filter(segmentID==1 | segmentID==3 | segmentID==5)
        
        #data <- data %>% sample_n(size=500)
        
        # get total number of observations
        N <- as.numeric(nrow(data)); R <- as.numeric(nlevels(data$routeID)); O <- as.numeric(nlevels(data$observerID))
        
        # extract diversity variables
        vars1  <- c("q0.t2", "q1.t2", "q2.t2", "qInf.t2", "berger.t2", "even.t2")
        for (i in vars1){ assign(i, as.vector(data[[i]])) }
        
        # extract landscape variables that need transform
        vars2  <- c("urban.t1", "urban.t2", "forest.t1", "forest.t2", "grass.t1", "grass.t2", 
                   "crop.t1", "crop.t2", "wet.t1", "wet.t2", "lc.q1.t1", "lc.q1.t2", "elev", "elev.sd")
        #for (i in vars){ assign(i, as.vector( data[[i]])) } # you can get the raw data
        for (i in vars2){ assign(i, as.vector( log(data[[i]]+1))) } # you can log the data here
        #for (i in vars){ assign(i, as.vector( (data[[i]] - mean(data[[i]]))/sd(data[[i]]) )) } # you can scale the data here
    
        # extract landscape changes that do not need transform
        vars3  <- c("delta.urban", "delta.forest", "delta.grass", "delta.crop", 
                   "delta.wet", "delta.lc.q1", "delta.temp", "delta.prec",
                   "temp.t1", "temp.t2", "prec.t1", "prec.t2")
        for (i in vars3){ assign(i, as.vector(data[[i]])) }
        
        #'-----------------------------------------------------------------------------------------------------
        #'* generate vectors of positive & negative land cover change*
        
        delta.pos.urban <- vector("numeric",length = N); delta.neg.urban <- vector("numeric",length = N)
            for(i in 1:N){ if(delta.urban[i]>0) delta.pos.urban[i] <- abs(delta.urban[i])
                            if(delta.urban[i]<=0) delta.neg.urban[i] <- abs(delta.urban[i])}
    
        delta.pos.forest<- vector("numeric",length = N); delta.neg.forest <- vector("numeric",length = N)
            for(i in 1:N){ if(delta.forest[i]>0) delta.pos.forest[i] <- abs(delta.forest[i])
                            if(delta.forest[i]<=0) delta.neg.forest[i] <- abs(delta.forest[i])}
    
        delta.pos.grassland <- vector("numeric",length = N); delta.neg.grassland <- vector("numeric",length = N)
            for(i in 1:N){ if(delta.grass[i]>0) delta.pos.grassland[i] <- abs(delta.grass[i])
                            if(delta.grass[i]<=0) delta.neg.grassland[i] <- abs(delta.grass[i])}
    
        delta.pos.cropland <- vector("numeric",length = N); delta.neg.cropland <- vector("numeric",length = N)
            for(i in 1:N){ if(delta.crop[i]>0) delta.pos.cropland[i] <- abs(delta.crop[i])
                            if(delta.crop[i]<=0) delta.neg.cropland[i] <- abs(delta.crop[i])}
    
        delta.pos.wetland <- vector("numeric",length = N); delta.neg.wetland <- vector("numeric",length = N)
            for(i in 1:N){ if(delta.wet[i]>0) delta.pos.wetland[i] <- abs(delta.wet[i])
                            if(delta.wet[i]<=0) delta.neg.wetland[i] <- abs(delta.wet[i])}
        
        delta.pos.temp <- vector("numeric",length = N); delta.neg.temp <- vector("numeric",length = N)
            for(i in 1:N){ if(delta.temp[i]>0) delta.pos.temp[i] <- abs(delta.temp[i])
                            if(delta.temp[i]<=0) delta.neg.temp[i] <- abs(delta.temp[i])}
        
        delta.pos.prec <- vector("numeric",length = N); delta.neg.prec <- vector("numeric",length = N)
            for(i in 1:N){ if(delta.prec[i]>0) delta.pos.prec[i] <- abs(delta.prec[i])
                            if(delta.prec[i]<=0) delta.neg.prec[i] <- abs(delta.prec[i])}
        
        delta.pos.lc.q1 <- vector("numeric",length = N); delta.neg.lc.q1 <- vector("numeric",length = N)
            for(i in 1:N){ if(delta.lc.q1[i]>0) delta.pos.lc.q1[i] <- abs(delta.lc.q1[i])
                            if(delta.lc.q1[i]<=0) delta.neg.lc.q1[i] <- abs(delta.lc.q1[i])}

        data.list  <- list(
          # response and number data points
          "q1_t2" = q1.t2, "N" = N, 
          
          # land cover variables  
          "urban_t1"=urban.t2, "forest_t1"=forest.t1, "grass_t1"=grass.t1, "crop_t1"=crop.t1, "wet_t1"=wet.t1, "lc_q1_t1"=lc.q1.t1,
          "urban_t2"=urban.t2, "forest_t2"=forest.t2, "grass_t2"=grass.t2, "crop_t2"=crop.t2, "wet_t2"=wet.t2, "lc_q1_t2"=lc.q1.t2,
            "urban_squared_t1"= urban.t2^2, "forest_squared_t1"=forest.t1^2, "grass_squared_t1"=grass.t1^2, 
            "crop_squared_t1"=crop.t1^2, "wet_squared_t1"=wet.t1^2, "lc_q1_squared_t1"=lc.q1.t1^2,
            "urban_squared_t2"=urban.t2^2, "forest_squared_t2"=forest.t2^2, "grass_squared_t2"=grass.t2^2, 
            "crop_squared_t2"=crop.t2^2, "wet_squared_t2"=wet.t2^2, "lc_q1_squared_t2"=lc.q1.t2^2,
          
          # land cover change variables
          "delta_pos_forest"=delta.pos.forest, "delta_pos_crop"=delta.pos.cropland, "delta_pos_grass"=delta.pos.grassland, 
          "delta_pos_wet"=delta.pos.wetland, "delta_pos_lc_q1"=delta.pos.lc.q1, #"delta.pos.temp"=delta.pos.temp,  
          "delta_neg_forest"=delta.neg.forest, "delta_neg_crop"=delta.neg.cropland, "delta_neg_grass"=delta.neg.grassland,
          "delta_neg_wet"=delta.neg.wetland, "delta_neg_lc_q1"=delta.neg.lc.q1, #"delta.neg.temp"=delta.neg.temp, 
          "delta_pos_urban"=delta.pos.urban, 
          
          # random effects
          "R"=R, "routeID"=as.numeric(data$routeID), "O"=O, "observerID"=as.numeric(data$observerID), 
          #"S"=S, "segmentID"=segmentID, 
          
          # climatic, elevation & other variables
          "time"=data$time.t2,
          #"temp_t1"=temp.t1, "prec_t1", "prec_t2", "elev",
          "temp_t2"=temp.t2, "temp_squared_t2"=temp.t2^2 
        )
        
        save(data.list, file = paste0("USBBS_data/model_ready_data/", file, "years_x_timepoint/data_", f, "_", buffer.type,"_",buffer.size,".rda"))
        print(paste0("USBBS_data/model_ready_data/", file, "years_x_timepoint/data_", f, "_", buffer.type,"_",buffer.size,".rda"))
        # clean up before image save
        #rm(list=ls()[! ls() %in% c("data.list", "f", "buffer.size", "file")])
        
        # alpha diversities calculated for min, mean, max abundances
        fun.names <- c("min", "mean", "max"); buffer.sizes <- c(500, 1000, 2000, 4000, 6000); buffer.types <- c("centroid", "segment")
        
      }
      
    }
    
  }

}


