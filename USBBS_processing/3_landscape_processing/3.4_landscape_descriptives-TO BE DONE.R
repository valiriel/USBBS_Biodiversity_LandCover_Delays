library(summarytools)
st_options(footnote=NA, style="grid", descr.transpose=T, 
           descr.stats=c("mean", "sd", "med", "min", "max"))
library(tidyverse)

#' *Import the land cover files*
load("USBBS_DataProcessing/covariates.centroid.springtemp.rda")

#' *Descriptive Statistics*
print(descr(covariates),
      file = "USBBS_DataProcessing/USBBS_LandCover_Output/USBBS_centroid_springtemp_Descriptive_Stats.html", 
      report.title = "USA LAND COVER - 4km buffer around USBBS routes",
      footnote = "<b>USA LAND COVER - 4km buffer around USBBS routes</b><br/>")

####################################################################################################################
#'
#' *Scatter plot of land cover between the two timepoints* 
#'
# switch between 3km and 23km and segment.4km

div.colours <- c('violetred3', 'orange', 'dodgerblue3', 'brown3', 'darkturquoise', 'limegreen')

ggplot(covariates, aes(wetland.t1, wetland.t2)) +  
  geom_point(inherit.aes = TRUE, size = 1.8, alpha = 0.25, colour='purple') +
  theme_bw() + 
  scale_x_continuous(limits = c(-5, 100), breaks = seq(from = 0, to = 100, by = 10)) +
  scale_y_continuous(limits = c(-5, 100), breaks = seq(from = 0, to = 100, by = 10)) + 
  labs (x = "wetland land cover in 2001", y = "wetland land cover in 2016", title = '4km round buffer around survey route segments') + 
  geom_rug(colour="black", alpha=0.05 ) + geom_abline(size=0.5)

ggsave("USBBS_DataProcessing/USBBS_LandCover_Output/urban_4km_scatter_01_16.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)

####################################################################################################################
#'
#' *CHANGE histograms of urban land cover between the two years*
#'
# switch between 3km and 23km
ggplot(data.segment.4km, aes(x = delta.urban)) +  # violetred3, limegreen
  geom_histogram(inherit.aes=T, binwidth = 1, fill="limegreen", col = "black", center = 0, size=0.5) +
  theme_bw() +
  labs(title='4km round buffer around survey route segments', x = "\u0394 urban land cover between 2001 and 2016", y = "Frequency") +
  #scale_x_continuous(limits = c(-10, 10), breaks = seq(from = -200, to = 200, by = 10))  +
  #scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 200)) 

ggsave("USBBS_LandCover_Output/delta.urban_34km_hist_01_16.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

