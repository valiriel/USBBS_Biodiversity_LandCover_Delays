#'#################################################################################
#'
#'*Quantify the overall change of each land cover across the USA and plot it out*
#'

library(tidyverse)
library(rgdal)
library(mapview)
library(ggthemes)

usa.lc.t2 <- readOGR("D:/USBBS_DATA/USBBS_LandCover/USA_Example/USA_LC2.shp")

data <- usa.lc.t2@data

head(usa.lc.t2@data)

LandCover.categories <- c("partition", "null", "water", "icesnow", 
                          "urban0to20", "urban20to49", "urban50to79", "urban80to100",
                          "barrenland", "deciduousforest", "evergreenforest", "mixedforest",
                          "shrubland", "grasslandherbaceous", "pasture", "cropland", "woodywetlands", "herbaceouswetlands")

colnames(usa.lc.t2@data)[-(1:4)] <- LandCover.categories

clean.fun <- function(x){ 
  
  x$totpixels <- rowSums(x[2:ncol(x)]) # calculate total pixels and proportions
  
  for (i in 2:ncol(x)) {
    
    x[,i] <- x[,i]/x$totpixels * 100
    
  } 
  
  # remove areas of no data
  # group up subclasses of land cover categories
  x %>% 
    transmute(partition = partition,
              water = water,
              icesnow = icesnow,
              urban = urban0to20 + urban20to49 + urban50to79 + urban80to100,
                urban0to20 = urban0to20,
                urban20to49 = urban20to49,
                urban50to79 = urban50to79, 
                urban80to100 = urban80to100,
              forest = deciduousforest + evergreenforest + mixedforest,
                deciduousforest = deciduousforest,
                evergreenforest = evergreenforest,
                mixedforest = mixedforest,
              grassland = grasslandherbaceous + pasture + shrubland,
                grasslandherbaceous = grasslandherbaceous,
                pasture = pasture,
                shrubland = shrubland,
              cropland = cropland,
              wetland = woodywetlands + herbaceouswetlands,
                woodywetlands = woodywetlands,
                herbaceouswetlands = herbaceouswetlands,
              barrenland = barrenland)
  
  
}

data <- clean.fun(usa.lc.t2@data[,-(1:4)])

#'#################################################################################
#'
#'*Check area of a single hexagon*
#'

# According to QGIS measures, approximately : #87182620.329 m² #87.183 km²
#select a random hexagon and get area in km²
hex.area <- 87

#'#################################################################################
#'
#'*Calculate area in km² for each land cover in 2016*
#'

Urban <- sum((data$urban/100)*hex.area)
  Developed.OpenSpace <- sum((data$urban0to20/100)*hex.area)
  Developed.Low <- sum((data$urban20to49/100)*hex.area)
  Developed.Medium <- sum((data$urban50to79/100)*hex.area)
  Developed.High <- sum((data$urban80to100/100)*hex.area)

Forest <- sum((data$forest/100)*hex.area)
  Deciduos.Forest <- sum((data$deciduousforest/100)*hex.area)
  Evergreen.Forest <- sum((data$evergreenforest/100)*hex.area)
  Mixed.Forest <- sum((data$mixedforest/100)*hex.area)

Grassland <- sum((data$grassland/100)*hex.area)
  Grassland.Herbaceous <- sum((data$grasslandherbaceous/100)*hex.area)
  Pasture.Hay <- sum((data$pasture/100)*hex.area)
  Shrubland <- sum((data$shrubland/100)*hex.area)

Cropland <- sum((data$cropland/100)*hex.area)

Wetland <- sum((data$wetland/100)*hex.area)
  Wetland.Woody <- sum((data$woodywetlands/100)*hex.area)
  Wetland.Herbaceous <- sum((data$herbaceouswetlands/100)*hex.area)

Barrenland <- sum((data$barrenland/100)*hex.area)

Perennial.Ice.Snow  <- sum((data$icesnow/100)*hex.area)
  
Open.Water <- sum((data$water/100)*hex.area)

#'#################################################################################
#' *Barplot one next to each other*
#' 

plot.data <- tibble("value"=c(Urban, Forest, Grassland, Cropland, Wetland),
"LC.subclass"=c("Urban", "Forest", "Grassland", "Cropland", "Wetland"),
"LC.aggregate"=c("Urban", "Forest", "Grassland", "Cropland", "Wetland"))

ggplot(plot.data, aes(x=LC.subclass, y=value, fill=LC.aggregate)) +
  geom_bar(data=plot.data, stat="identity", position=position_dodge()) +
  #geom_bar(data=plot.data[plot.data$change<0,], stat="identity", alpha=0.6) +
  #scale_y_continuous(limits = c(0, 1e+05), breaks = seq(from = -(1e+05), to=1e+05, by =25000)) +
  scale_x_discrete(limits=c("Urban", "Forest", "Grassland", "Cropland", "Wetland")) + # order types on x axys
  scale_y_continuous(limits = c(0, 3500000), breaks = seq(from = 0, to=3000000, by = 1000000),
                     labels = c( "0", "1", "2", "3")) +
  scale_fill_manual(values=c("Grassland"='limegreen', "Forest"='darkgreen', "Cropland"='darkgoldenrod2', 
                             "Urban"='gray38', "Wetland"='purple', "other"='black')) +
  labs (x = "", y = "Land cover area in 2016 (10^6 km²)", title = "") + 
  theme_clean(base_size = 25) + 
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=28),
        plot.background = element_rect(color = "white"),
        panel.grid.major.y = element_line(size=0.2),
        legend.position='none',
        axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5))

ggsave("USBBS_Modelling_Delays/USBBS_Modelling_Output/LC_km2_classes.tiff",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave("USBBS_Modelling_Delays/USBBS_Modelling_Output/LC_km2_classes.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)



#' 
plot.data <- tibble("value"=c(Urban, Developed.OpenSpace, Developed.Low, Developed.Medium, Developed.High,
                              Forest, Deciduos.Forest, Evergreen.Forest, Mixed.Forest,
                              Grassland, Grassland.Herbaceous, Pasture.Hay, Shrubland,
                              Cropland,
                              Wetland, Wetland.Woody, Wetland.Herbaceous,
                              Barrenland, Perennial.Ice.Snow, Open.Water),
                    "LC.subclass"=c("Urban", "Developed.OpenSpace", "Developed.Low", "Developed.Medium", "Developed.High",
                                    "Forest", "Deciduos.Forest", "Evergreen.Forest", "Mixed.Forest",
                                    "Grassland", "Grassland.Herbaceous", "Pasture.Hay", "Shrubland",
                                    "Cropland",
                                    "Wetland", "Wetland.Woody", "Wetland.Herbaceous",
                                    "Barrenland", "Perennial.Ice.Snow", "Open.Water"),
                    "LC.aggregate"=c("Urban", "Urban", "Urban", "Urban", "Urban",
                                     "Forest", "Forest", "Forest", "Forest",
                                     "Grassland", "Grassland", "Grassland", "Grassland", 
                                     "Cropland", 
                                     "Wetland", "Wetland", "Wetland",
                                     "other", "other", "other" ))


ggplot(plot.data, aes(x=LC.subclass, y=value, fill=LC.aggregate)) +
  geom_bar(data=plot.data, stat="identity", position=position_dodge()) +
  #geom_bar(data=plot.data[plot.data$change<0,], stat="identity", alpha=0.6) +
  #scale_y_continuous(limits = c(0, 1e+05), breaks = seq(from = -(1e+05), to=1e+05, by =25000)) +
  scale_x_discrete(limits=c("Urban", "Developed.OpenSpace", "Developed.Low", "Developed.Medium", "Developed.High",
                            "Forest", "Deciduos.Forest", "Evergreen.Forest", "Mixed.Forest",
                            "Grassland", "Grassland.Herbaceous", "Pasture.Hay", "Shrubland",
                            "Cropland",
                            "Wetland", "Wetland.Woody", "Wetland.Herbaceous",
                            "Barrenland", "Perennial.Ice.Snow", "Open.Water")) + # order types on x axys
  scale_y_continuous(limits = c(0, 3500000), breaks = seq(from = 0, to=3000000, by = 1000000),
                     labels = c( "0", "1", "2", "3")) +
  scale_fill_manual(values=c("Grassland"='limegreen', "Forest"='darkgreen', "Cropland"='darkgoldenrod2', 
                             "Urban"='gray38', "Wetland"='purple', "other"='black')) +
  labs (x = "", y = "Land cover area in 2016 (10^6 km²)", title = "") + 
  theme_clean(base_size = 15) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13),
        plot.background = element_rect(color = "white"),
        panel.grid.major.y = element_line(size=0.2),
        legend.position='none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("USBBS_Modelling_Delays/USBBS_Modelling_Output/LC_km2_subclasses.tiff",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave("USBBS_Modelling_Delays/USBBS_Modelling_Output/LC_km2_subclasses.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)