library(tidyverse); library(reshape2); library(magrittr); library(vegan)

states <- list.files("USBBS_data/diversity_data/raw_USBBS")

#'------------------------------------------------------------------------------------
#' * Import and extract 2019 data *
#'

for (f in states) {
  
  temp_state <- read.csv(paste0("USBBS_data/diversity_data/raw_USBBS/", f))
  
  temp_state <- as_tibble(temp_state) %>% filter(CountryNum == 840) %>% filter(Year == 2019) %>% filter(RPID == 101)
  
  if(f == states[1]) data <- temp_state

  if(f != states[1]) data <- rbind(data, temp_state)
  
}

# set route and state string value to same length as shapefile for matching
data$StateNum <- str_pad(data$StateNum, width=2, side="left", pad="0")  
unique(data$StateNum)
data$Route <- str_pad(data$Route, width=3, side="left", pad="0")
unique(data$Route)

# generate unique state, route index
data$U_S_R_I <- paste0(data$StateNum, "_", data$Route); validation_data <- data

#'------------------------------------------------------------------------------------
#' * Keep partitions within range of our data *
#'

load("USBBS_data/whole_data/3years_x_timepoint/data_mean_centroid_500.rda")
routes <- unique(data$route)

validation_data <- validation_data %>% 
                    filter(U_S_R_I %in% routes) %>% 
                    filter(SpeciesTotal>-1) %>% 
                      transmute(year=Year, species=AOU, partition = U_S_R_I,
                                Count10, Count20, Count30, Count40, Count50) 
                    
#'------------------------------------------------------------------------------------
#' * Get species x partition matrix *
#'

segments <- c("Count10", "Count20", "Count30", "Count40", "Count50")
i <- 1
new.matrix <- TRUE

for(segment.now in segments) {
  
  sp.matrix.now <- validation_data %>%
    dcast(partition ~ species, fun.aggregate = mean,  value.var = segment.now, fill = 0) %>% 
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

a <- sp.matrix; sp.matrix <- as.matrix(sp.matrix[,-1])
rownames(sp.matrix) <- a$partition

#'------------------------------------------------------------------------------------
#' * Get diversity measures*
#'

# setup dataframe
validation_alpha <- tibble("partition" = rownames(sp.matrix), "q0_validation" = 0, "q1_validation" = 0)

validation_alpha$q0_validation <- vegan::specnumber(sp.matrix)
validation_alpha$q1_validation <- exp(vegan::diversity(sp.matrix, "shannon"))

# just fix all those decimals
validation_alpha[,2:ncol(validation_alpha)] <- round(validation_alpha[,2:ncol(validation_alpha)], 2)

validation_alpha[is.na(validation_alpha)] <- 0; validation_alpha[validation_alpha == Inf] <- 0

save(validation_alpha, file="USBBS_data/diversity_data/validation_data.rda")


