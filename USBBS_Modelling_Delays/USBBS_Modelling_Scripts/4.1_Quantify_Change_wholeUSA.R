#'#################################################################################
#'
#'*Quantify the overall change of each land cover across the USA and plot it out*
#'

library(tidyverse)
library(rgdal)
library(mapview)

# IMport shapefiles grid
usa.lc <- readOGR("D:/USBBS_DATA/USA_predict_map/USA_q0_predict_iter.shp")

data <- usa.lc@data

#'#################################################################################
#'
#'*Check area of a single hexagon*
#'

# According to QGIS measures, approximately : #87182620.329 m² #87.183 km²

#select a random hexagon and get area in km²
one.hex <- subset(usa.lc, usa.lc$partitn == 46771)
hex.area <- rgeos::gArea(one.hex) * 1e-6
hex.area <- 86.60254

#'#################################################################################
#'
#'*Calculate change in km² for each land cover*
#'

urban.pos <- sum((data$dlt_ps_r/100)*hex.area)
urban.neg <- -sum((data$dlt_ng_r/100)*hex.area)

forest.pos <- sum((data$dlt_ps_f/100)*hex.area)
forest.neg <- -sum((data$dlt_ng_f/100)*hex.area)

grassland.pos <- sum((data$dlt_ps_g/100)*hex.area)
grassland.neg <- -sum((data$dlt_ng_g/100)*hex.area)

cropland.pos <- sum((data$dlt_ps_c/100)*hex.area)
cropland.neg <- -sum((data$dlt_ng_c/100)*hex.area)

wetland.pos <- sum((data$dlt_ps_w/100)*hex.area)
wetland.neg <- -sum((data$dlt_ng_w/100)*hex.area)

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
  scale_y_continuous(limits = c(-(1e+05), 1e+05), breaks = seq(from = -(1e+05), to=1e+05, by =25000),
                     labels=c("-10", "-7.5", "-5", "-2.5", "0", "2.5", "5", "7.5", "10" )) +
    scale_x_discrete(limits=c("Urban", "Forest", "Wetland", "Grassland", "Cropland")) + # order types on x axys
    scale_fill_manual(values=c("Grassland"='limegreen', "Forest"='darkgreen', "Cropland"='darkgoldenrod2', 
                               "Urban"='gray38', "Wetland"='cornflowerblue')) +
  labs (x = "", y = "Change 2001-2016 (10\u2074 km²) ", title = "") +
  geom_hline(yintercept=0) +
  theme_clean(base_size = 20) + 
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        plot.background = element_rect(color = "white"),
        panel.grid.major.y = element_line(size=0.2)) +
  guides(fill=FALSE) #+ expression(paste('Change 2001-2016 (', 10^{4}, 'km²)'))
  coord_flip() 

ggsave(plot = lc_change, "USBBS_Modelling_Delays/USBBS_Modelling_Output/km2_LC_change.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = lc_change, "USBBS_Modelling_Delays/USBBS_Modelling_Output/km2_LC_change.tiff",
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
