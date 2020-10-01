library(summarytools)
st_options(footnote=NA, style="grid", descr.transpose=T, 
           descr.stats=c("mean", "sd", "med", "min", "max"))
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(viridis)
library(ggpmisc)

load("USBBS_DataProcessing/covariates.rda")
data <- covariates

#'##########################################################################
#'
#' *explore correlation between the data*
#'
corr.matrix <- cor(data[3:ncol(data)])

cor.test <- cor.mtest(corr.matrix, conf.level = .95) # run a correlation significance test, Pearson by default
diag(corr.matrix) <- NA # set diagonal to 0 instead of 1 for plotting reasons

corrplot(corr.matrix, method = "color",  
         na.label = ".", #set symbols for diagonal easier to visualize
         col = brewer.pal(n = 10, name = "RdBu"), # set color blue and red, positive negative
         p.mat = cor.test$p, insig="blank") # assign significance values and set insignificant to be white

####################################################################################################################
#'
#' **OBSOLETE**
#'
#' *Relationship between diversity measures and land cover from observed data*
#'

divs.t1 <- c('q0.t1', 'q1.t1', 'q2.t1', 'qInf.t1', 'berger.t1', 'even.t1') 
divs.t2 <- c('q0.t2', 'q1.t2', 'q2.t2', 'qInf.t2', 'berger.t2', 'even.t2') 
divs.name <- c('Species richness', 'log(Shannon entropy)', '1/Simpson index', 'Berger-Parker Hills', 'Berger-Parker dominance', 'Evenness')
divs.increment <- c(10, 5, 5, 2.5, 0.1, 0.1)
divs.limits <- c(80, 50, 40, 20, 1, 1)
div.colours <- c('violetred3', 'orange', 'dodgerblue3', 'brown3', 'darkturquoise', 'limegreen')

# Select which measure you want from 1 to 5
i <- 5

# prepare data for log transform
data$urban.t1 <- data$urban.t1 + 1 
data$urban.t2 <- data$urban.t2 + 1 

#' *DO NOT TRUST THE SMOOTH LINE it's there just for getting a visual idea*
#' *Change data between t1 and t2 in the code*
#' 
ggplot(data, aes_string(data$forest.t2, divs.t2[i])) +  
  geom_point(inherit.aes = TRUE, size = 1.8, alpha = 0.4, colour='brown3') +
  geom_smooth(formula= y ~ x + x^2, colour="black", alpha=0.5, method="loess") + # loess = Local Polynomial Regression Fitting
  theme_bw() + 
  scale_x_continuous( limits = c(-5, 100), breaks = seq(from=0, to=100, by=10)) +
  #scale_x_log10(limits = c(-5, 100), breaks = seq(from=0, to=100, by=10)) +
  scale_y_continuous( limits = c(0, divs.limits[i]), breaks = seq(from = 0, to = divs.limits[i], by = divs.increment[i]) ) + 
  labs (x = "% forest land cover in 2016", y = paste(divs.name[i]," in 2016"), title = '') + 
  geom_rug(colour="black", alpha=0.05)

ggsave("USBBS_Modelling_Output/segment_output_scatters/berger_forest_t2_4km.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

