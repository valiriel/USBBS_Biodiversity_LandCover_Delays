library(ggthemes);  library(tidyverse); library(sf)

theme <- theme_clean() + 
  theme(axis.title=element_text(size=16), axis.text=element_text(size=14),
        legend.text=element_text(size=12), legend.title=element_text(size=16), 
        plot.background = element_rect(color = "white"), 
        panel.border= element_blank(), plot.title=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(color = NA),
        strip.background.y = element_rect(color = "grey30"), legend.position="none")

load("USBBS_data/map_predict_data/USA_landscape_predicted.rda")
usa_borders <- read_sf("USBBS_data/map_predict_data/USA_border.shp") %>% st_transform(crs=5070) %>% group_by(ID) %>% summarise()

#------------------------------------------------------------------------------------

usa_landscape$delta_urban[usa_landscape$delta_urban > 10] <- 10
usa_landscape$delta_urban[usa_landscape$delta_urban < -10] <- -10

p <- ggplot() + theme +
      geom_sf(data=usa_landscape, aes(fill=delta_urban), color=NA) +
      geom_sf(data=usa_borders, color="black") +
      scale_fill_gradient2(low="slategray4", mid="white", high="darkgoldenrod1", midpoint = 0, limits = c(-10, 10)) +
      #scale_fill_manual(values =c("gray16", "slategray4", "white", "goldenrod3", "darkgoldenrod4")) + 
      theme(axis.line = element_blank(), axis.text=element_blank(), axis.title=element_blank(),
            axis.ticks=element_blank(), legend.key = element_rect(colour = "black"),
            legend.text=element_text(size=10), legend.title=element_text(size=12))
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_urban.png", units = "cm", dpi = "retina", width =29, height = 21)
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_urban.svg", units = "cm", dpi = "screen", width =29, height = 21)

#----
usa_landscape$delta_forest[usa_landscape$delta_forest > 10] <- 10
usa_landscape$delta_forest[usa_landscape$delta_forest < -10] <- -10

p <- ggplot() + theme +
  geom_sf(data=usa_landscape, aes(fill=delta_forest), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_gradient2(low="slategray4", mid="white", high="darkgoldenrod1", midpoint = 0, limits = c(-10, 10)) +
  #scale_fill_manual(values =c("gray16", "slategray4", "white", "goldenrod3", "darkgoldenrod4")) + 
  theme(axis.line = element_blank(), axis.text=element_blank(), axis.title=element_blank(),
        axis.ticks=element_blank(), legend.key = element_rect(colour = "black"),
        legend.text=element_text(size=10), legend.title=element_text(size=12))
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_forest.png", units = "cm", dpi = "retina", width =29, height = 21)
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_forest.svg", units = "cm", dpi = "screen", width =29, height = 21)

#----
usa_landscape$delta_grass[usa_landscape$delta_grass > 10] <- 10
usa_landscape$delta_grass[usa_landscape$delta_grass < -10] <- -10

p <- ggplot() + theme +
  geom_sf(data=usa_landscape, aes(fill=delta_grass), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_gradient2(low="slategray4", mid="white", high="darkgoldenrod1", midpoint = 0, limits = c(-10, 10)) +
  #scale_fill_manual(values =c("gray16", "slategray4", "white", "goldenrod3", "darkgoldenrod4")) + 
  theme(axis.line = element_blank(), axis.text=element_blank(), axis.title=element_blank(),
        axis.ticks=element_blank(), legend.key = element_rect(colour = "black"),
        legend.text=element_text(size=10), legend.title=element_text(size=12))
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_grass.png", units = "cm", dpi = "retina", width =29, height = 21)
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_grass.svg", units = "cm", dpi = "screen", width =29, height = 21)

#----
usa_landscape$delta_crop[usa_landscape$delta_crop > 10] <- 10
usa_landscape$delta_crop[usa_landscape$delta_crop < -10] <- -10

p <- ggplot() + theme +
  geom_sf(data=usa_landscape, aes(fill=delta_crop), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_gradient2(low="slategray4", mid="white", high="darkgoldenrod1", midpoint = 0, limits = c(-10, 10)) +
  #scale_fill_manual(values =c("gray16", "slategray4", "white", "goldenrod3", "darkgoldenrod4")) + 
  theme(axis.line = element_blank(), axis.text=element_blank(), axis.title=element_blank(),
        axis.ticks=element_blank(), legend.key = element_rect(colour = "black"),
        legend.text=element_text(size=10), legend.title=element_text(size=12))
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_crop.png", units = "cm", dpi = "retina", width =29, height = 21)
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_crop.svg", units = "cm", dpi = "screen", width =29, height = 21)

#----
usa_landscape$delta_wet[usa_landscape$delta_wet > 10] <- 10
usa_landscape$delta_wet[usa_landscape$delta_wet < -10] <- -10

p <- ggplot() + theme +
  geom_sf(data=usa_landscape, aes(fill=delta_wet), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_gradient2(low="slategray4", mid="white", high="darkgoldenrod1", midpoint = 0, limits = c(-10, 10)) +
  #scale_fill_manual(values =c("gray16", "slategray4", "white", "goldenrod3", "darkgoldenrod4")) + 
  theme(axis.line = element_blank(), axis.text=element_blank(), axis.title=element_blank(),
        axis.ticks=element_blank(), legend.key = element_rect(colour = "black"),
        legend.text=element_text(size=10), legend.title=element_text(size=12))
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_wet.png", units = "cm", dpi = "retina", width =29, height = 21)
ggsave(p, file = "USBBS_output/landscape_output/landcover_change_fig3/delta_wet.svg", units = "cm", dpi = "screen", width =29, height = 21)

