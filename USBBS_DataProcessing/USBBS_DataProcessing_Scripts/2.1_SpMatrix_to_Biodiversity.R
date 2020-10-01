library(tidyverse)
library(vegan)

#'
#'*NB* *THE PAPER ANALYSIS USES ONLY THE DIVERSITY MEASURES FOR q0 & EVENNESS*
#'

# load in berger parker index function
source("USBBS_DataProcessing/USBBS_DataProcessing_Scripts/fun_berger_parker.R")

#'##########################################################################
#'
#' *Import sp matrices and setup diversity dataframe*
#'

load("USBBS_DataProcessing/vegan_spmatrix_t1.rda") # 7385 communities, 536 species
load("USBBS_DataProcessing/vegan_spmatrix_t2.rda") # 7385 communities, 543 species

# setup dataframe
alphas <- data.frame("partition" = rownames(spmatrix.t1), "q0.t1"=0, "q1.t1"=0, "q2.t1"=0, "qInf.t1"=0, "berger.t1"=0,
                     "even.t1"=0, "q0.t2"=0, "q1.t2"=0, "q2.t2"=0, "qInf.t2"=0, "berger.t2"=0, "even.t2"=0)

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
alphas[,2:ncol(alphas)] <- round(alphas[,2:ncol(alphas)], 2)

save(alphas, file="USBBS_DataProcessing/alpha.01.16.rda")

