library(tidyverse)
library(vegan)

#'
#'*NB* *THE PAPER ANALYSIS USES ONLY THE DIVERSITY MEASURES FOR q1*
#'

# load in berger parker index function
source("USBBS_processing/2_biodiversity_processing/function_berger_parker.R")

#'##########################################################################
#'
#' *Import sp matrices and setup diversity dataframe*
#'

fun.names <- c("min", "mean", "max") # f <- "min" ; file <-2

for (file in 2:3) {
  
  for (f in fun.names) {
   
    # load in species matrices for than aggregate function
    load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/vegan_spmatrix_t1_", f, ".rda"))
    load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/vegan_spmatrix_t2_", f, ".rda"))  
     
    # setup dataframe
    alphas <- data.frame("partition" = rownames(spmatrix.t1), "route" = str_sub(rownames(spmatrix.t1), 0, 6), "q0.t1"=0, "q1.t1"=0, "q2.t1"=0, "qInf.t1"=0, 
                         "berger.t1"=0, "even.t1"=0, "q0.t2"=0, "q1.t2"=0, "q2.t2"=0, "qInf.t2"=0, "berger.t2"=0, "even.t2"=0)
  
    #############################################################################################
    #
    #' *ALPHA DIVERSITY with VEGAN package to HILL NUMBER FRAMEWORK*
    #' 
    #' *q0 =* Species richness
    #' *q1 =* Exponential of Shannon entropy
    #' *q2 =* Inverse of Simpson index
    #' *qInf =* Berger Parker Index
    #' *even =* Pielou's evenness, Shannon entropy/log(species richness)
    #'
    
    # Alpha diversity at timepoint 1, year 2001
    #
    alphas$q0.t1 <- vegan::specnumber(spmatrix.t1)
    alphas$q1.t1 <- exp(vegan::diversity(spmatrix.t1, "shannon"))
    alphas$q2.t1 <- vegan::diversity(spmatrix.t1, "invsimpson")
    alphas$qInf.t1 <- berger_parker_hill(spmatrix.t1)
    alphas$berger.t1 <- berger_parker(spmatrix.t1)
    alphas$even.t1 <- log(alphas$q1.t1)/log(alphas$q0.t1) # need to retransform shannon from hill framework exp()
    
    # Alpha diversity at timepoint 2, year 2016
    #
    alphas$q0.t2 <- vegan::specnumber(spmatrix.t2)
    alphas$q1.t2 <- exp(vegan::diversity(spmatrix.t2, "shannon"))
    alphas$q2.t2 <- vegan::diversity(spmatrix.t2, "invsimpson")
    alphas$qInf.t2 <- berger_parker_hill(spmatrix.t2)
    alphas$berger.t2 <- berger_parker(spmatrix.t2)
    alphas$even.t2 <- log(alphas$q1.t2)/log(alphas$q0.t2)
    
    # Alpha diversity difference between the two timepoints
    #
    alphas$delta.q0 <- alphas$q0.t2 - alphas$q0.t1
    alphas$delta.q1 <- alphas$q1.t2 - alphas$q1.t1
    alphas$delta.q2 <- alphas$q2.t2 - alphas$q2.t1
    alphas$delta.qInf <- alphas$qInf.t2 - alphas$qInf.t1
    alphas$delta.berger <- alphas$berger.t2 - alphas$berger.t1
    alphas$delta.even <- alphas$even.t2 - alphas$even.t1
    
    # just fix all those decimals
    alphas[,3:ncol(alphas)] <- round(alphas[,3:ncol(alphas)], 2)
  
    # this route escaped quality control is returning NAs
    #alphas <- alphas %>% filter(route != "53_800")
    # fix for segments with 0 or 1 species observed
    alphas[is.na(alphas)] <- 0; alphas[alphas == Inf] <- 0
    
    save(alphas, file=paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/alpha_", f, ".rda"))
  
  }
  
}

