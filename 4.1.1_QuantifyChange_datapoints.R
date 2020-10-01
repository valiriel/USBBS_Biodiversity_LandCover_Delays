#'#################################################################################
#'
#'*Quantify the overall change of each land cover across the USA and plot it out*
#'

library(tidyverse)
library(rgdal)
library(mapview)

load("USBBS_Modelling_Delays/data.withpred.centroid.springtemp.rda")

#'#################################################################################
#'
#'*Check area of a single buffer* 4km radius
#'
area <- pi * (4^2) 

#'#################################################################################
#'
#'*Calculate change in km² for each land cover*
#'

urban.pos <- sum((data$delta.pos.urban/100)*area)
urban.neg <- -sum((data$delta.neg.urban/100)*area)

forest.pos <- sum((data$delta.pos.forest/100)*area)
forest.neg <- -sum((data$delta.neg.forest/100)*area)

grassland.pos <- sum((data$delta.pos.grassland/100)*area)
grassland.neg <- -sum((data$delta.neg.grassland/100)*area)

cropland.pos <- sum((data$delta.pos.cropland/100)*area)
cropland.neg <- -sum((data$delta.neg.cropland/100)*area)

wetland.pos <- sum((data$delta.pos.wetland/100)*area)
wetland.neg <- -sum((data$delta.neg.wetland/100)*area)

#'#################################################################################
#'*Barplot over and under*

plot.data <- tibble("value"=c(urban.pos, urban.neg, forest.pos, forest.neg, grassland.pos, grassland.neg, 
                              cropland.pos, cropland.neg, wetland.pos, wetland.neg),
                    "LC"=c("Urban", "Urban", "Forest", "Forest", "Grassland", "Grassland", 
                           "Cropland", "Cropland", "Wetland", "Wetland"),
                    "Directionality"=c("positive", "negative", "positive", "negative", "positive", "negative", 
                                       "positive", "negative", "positive", "negative"))

lc_change <- ggplot(plot.data, aes(x=LC, y=value, fill=LC)) +
              geom_bar(data=plot.data[plot.data$value>0,], stat="identity") +
              geom_bar(data=plot.data[plot.data$value<0,], stat="identity", alpha=0.6) +
              scale_y_continuous(limits = c(-(3500), 3500), breaks = seq(from = -(3500), to=3500, by =1000)) +
              scale_x_discrete(limits=c("Grassland",  "Cropland", "Urban", "Forest", "Wetland")) + # order types on x axys
              scale_fill_manual(values=c("Grassland"='limegreen', "Forest"='darkgreen', "Cropland"='darkgoldenrod2', 
                                         "Urban"='gray38', "Wetland"='cornflowerblue')) +
              labs (x = "", y = "Change between 2001 and 2016 (km²)", title = "") +
              geom_hline(yintercept=0) +
              theme_minimal() + 
              theme(panel.grid.major = element_blank(), legend.position='none', panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    axis.text=element_text(size=18), axis.title=element_text(size=18))

ggsave(plot = lc_change, "USBBS_Modelling_Delays/USBBS_Modelling_Output/DATAPOINT_km2_LC_change.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = lc_change, "USBBS_Modelling_Delays/USBBS_Modelling_Output/DATAPOINT_km2_LC_change.tiff",
       units = "cm", dpi = "retina", width =29.7, height = 21)

#'#################################################################################
#' *Barplot one next to each other*

plot.data$value[plot.data$value<0] <- -plot.data$value[plot.data$value<0]

ggplot(plot.data, aes(x=LC, y=value, fill=Directionality)) +
  geom_bar(data=plot.data, stat="identity", position=position_dodge()) +
  #geom_bar(data=plot.data[plot.data$change<0,], stat="identity", alpha=0.6) +
  scale_y_continuous(limits = c(0, 1e+05), breaks = seq(from = -(1e+05), to=1e+05, by =25000)) +
  scale_x_discrete(limits=c("Grassland",  "Cropland", "Urban", "Forest", "Wetland")) + # order types on x axys
  #scale_fill_manual(values=c("Grassland"='limegreen', "Forest"='darkgreen', "Cropland"='darkgoldenrod2', 
  #"Urban"='gray38', "Wetland"='purple')) +
  labs (x = "", y = "Change between 2001 and 2016 (km²)", title = "") +
  theme_minimal()

ggsave("USBBS_Modelling_Output/km2_change_dodge.pdf",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave("USBBS_Modelling_Output/km2_change_dodge.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
