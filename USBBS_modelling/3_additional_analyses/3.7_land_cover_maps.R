library(ggthemes);  library(tidyverse); library(sf)

theme <- theme_clean() + 
  theme(axis.title=element_text(size=10), axis.text=element_text(size=9),
        legend.text=element_text(size=11), legend.title=element_text(size=12), 
        plot.background = element_rect(color = "white"), 
        panel.border= element_blank(), plot.title=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(color = NA),
        strip.background.y = element_rect(color = "grey30"))

load("USBBS_data/map_predict_data/USA_landscape_predicted.rda"); usa_landscape$debtcredit_mean[is.na(usa_landscape$debtcredit_mean)] <-0
usa_borders <- read_sf("USBBS_data/map_predict_data/USA_border.shp") %>% st_transform(crs=5070) %>% group_by(ID) %>% summarise()


#'--------------------------------------------------------------------------------------------------
#' *t1 land cover maps*
#'
p1 <- ggplot(data = usa_landscape, aes(fill=exp(urban_t1))) + theme +
        geom_sf(color=NA) + 
        geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
        theme + labs(title = "", fill="") +
        scale_fill_gradient(low="white", high="dimgray") + 
        theme(legend.position="none")
ggsave(plot = p1, file = "USBBS_output/landscape_output/landcover_maps/urban_t1.png",units = "cm", dpi = "retina", width =27, height = 19)

p2 <- ggplot(data = usa_landscape, aes(fill=exp(forest_t1))) + theme +
        geom_sf(color=NA) + 
        geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
        theme + labs(title = "", fill="") +
        scale_fill_gradient(low="white", high="forestgreen") + 
        theme(legend.position="none")
ggsave(plot = p2, file = "USBBS_output/landscape_output/landcover_maps/forest_t1.png",units = "cm", dpi = "retina", width =27, height = 19)

p3 <- ggplot(data = usa_landscape, aes(fill=exp(grass_t1))) + theme +
      geom_sf(color=NA) + 
      geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
      theme + labs(title = "", fill="") +
      scale_fill_gradient(low="white", high="green") + 
      theme(legend.position="none")
ggsave(plot = p3, file = "USBBS_output/landscape_output/landcover_maps/grass_t1.png",units = "cm", dpi = "retina", width =27, height = 19)

p4 <- ggplot(data = usa_landscape, aes(fill=exp(crop_t1))) + theme +
        geom_sf(color=NA) + 
        geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
        theme + labs(title = "", fill="") +
        scale_fill_gradient(low="white", high="darkgoldenrod1") + 
        theme(legend.position="none")
ggsave(plot = p4, file = "USBBS_output/landscape_output/landcover_maps/crop_t1.png",units = "cm", dpi = "retina", width =27, height = 19)

p5 <- ggplot(data = usa_landscape, aes(fill=exp(wet_t1))) + theme +
        geom_sf(color=NA) + 
        geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
        theme + labs(title = "", fill="") +
        scale_fill_gradient(low="white", high="navy") + 
        theme(legend.position="none")
ggsave(plot = p5, file = "USBBS_output/landscape_output/landcover_maps/wet_t1.png",units = "cm", dpi = "retina", width =27, height = 19)

# landcover histogram
hex.area <- 87

usa_landscape[is.na(usa_landscape)]

data <- usa_landscape %>%
  mutate(urban_t1 = coalesce(urban_t1, 0),
         forest_t1 = coalesce(forest_t1, 0),
         grass_t1 = coalesce(grass_t1, 0),
         crop_t1 = coalesce(crop_t1, 0),
         wet_t1 = coalesce(wet_t1, 0),
          urban_t2 = coalesce(urban_t2, 0),
          forest_t2 = coalesce(forest_t2, 0),
          grass_t2 = coalesce(grass_t2, 0),
          crop_t2 = coalesce(crop_t2, 0),
          wet_t2 = coalesce(wet_t2, 0))

sum(usa_landscape$urban_t1)

plot.data <- tibble("value"=c(sum(((exp(data$urban_t1) - 1)/100)*hex.area),
                              sum(((exp(data$forest_t1) - 1)/100)*hex.area),
                              sum(((exp(data$grass_t1) - 1)/100)*hex.area),
                              sum(((exp(data$crop_t1) - 1)/100)*hex.area),
                              sum(((exp(data$wet_t1) - 1)/100)*hex.area)),
                    "LC.class"=c("Urban", "Forest", "Grassland", "Cropland", "Wetland"))

hist_t1 <- ggplot(plot.data, aes(x=LC.class, y=value, fill=LC.class)) +
              geom_bar(data=plot.data, stat="identity", position=position_dodge()) +
              #geom_bar(data=plot.data[plot.data$change<0,], stat="identity", alpha=0.6) +
              #scale_y_continuous(limits = c(0, 1e+05), breaks = seq(from = -(1e+05), to=1e+05, by =25000)) +
              scale_x_discrete(limits=c("Urban", "Forest", "Grassland", "Cropland", "Wetland")) + # order types on x axys
              scale_y_continuous(limits = c(0, 3500000), breaks = seq(from = 0, to=3000000, by = 1000000),
                                 labels = c( "0", "1", "2", "3")) +
              scale_fill_manual(values=c("Grassland"='limegreen', "Forest"='darkgreen', "Cropland"='darkgoldenrod2', 
                                         "Urban"='gray38', "Wetland"='purple', "other"='black')) +
              labs (x = "", y = "Land cover area in 2001 (10^6 km²)", title = "") + 
              theme_clean(base_size = 8) + 
              theme(axis.text=element_text(size=10),
                    axis.title=element_text(size=11),
                    plot.background = element_rect(color = "white"),
                    panel.grid.major.y = element_line(size=0.2),
                    legend.position='none',
                    axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5))

ggsave(plot = hist_t1, file = "USBBS_output/landscape_output/landcover_maps/hist_t1.png",units = "cm", dpi = "retina", width =27, height = 19)

# landcover histogram

library(cowplot)

panel <- plot_grid(p1, p2, p3, p4, p5, hist_t1,
                   labels=c("A - Urban 2001","B - Forest 2001","C - Grassland 2001",
                            "D - Cropland 2001", "E - Wetland 2001", "F - Total area"),
                   label_size = 13, #align=c("hv"),
                   ncol=2, nrow=3)
ggsave(plot = panel, file = "USBBS_output/landscape_output/landcover_maps/panel_t1.png",units = "cm", dpi = "retina", width =17.5, height = 18.5)

#'--------------------------------------------------------------------------------------------------
#' *t2 land cover maps*
  #'
  p1 <- ggplot(data = usa_landscape, aes(fill=exp(urban_t2))) + theme +
    geom_sf(color=NA) + 
    geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
    theme + labs(title = "", fill="") +
    scale_fill_gradient(low="white", high="dimgray") + 
    theme(legend.position="none")
  ggsave(plot = p1, file = "USBBS_output/landscape_output/landcover_maps/urban_t2.png",units = "cm", dpi = "retina", width =27, height = 19)
  
  p2 <- ggplot(data = usa_landscape, aes(fill=exp(forest_t2))) + theme +
    geom_sf(color=NA) + 
    geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
    theme + labs(title = "", fill="") +
    scale_fill_gradient(low="white", high="forestgreen") + 
    theme(legend.position="none")
  ggsave(plot = p2, file = "USBBS_output/landscape_output/landcover_maps/forest_t2.png",units = "cm", dpi = "retina", width =27, height = 19)
  
  p3 <- ggplot(data = usa_landscape, aes(fill=exp(grass_t2))) + theme +
    geom_sf(color=NA) + 
    geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
    theme + labs(title = "", fill="") +
    scale_fill_gradient(low="white", high="green") + 
    theme(legend.position="none")
  ggsave(plot = p3, file = "USBBS_output/landscape_output/landcover_maps/grass_t2.png",units = "cm", dpi = "retina", width =27, height = 19)
  
  p4 <- ggplot(data = usa_landscape, aes(fill=exp(crop_t2))) + theme +
    geom_sf(color=NA) + 
    geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
    theme + labs(title = "", fill="") +
    scale_fill_gradient(low="white", high="darkgoldenrod1") + 
    theme(legend.position="none")
  ggsave(plot = p4, file = "USBBS_output/landscape_output/landcover_maps/crop_t2.png",units = "cm", dpi = "retina", width =27, height = 19)
  
  p5 <- ggplot(data = usa_landscape, aes(fill=exp(wet_t2))) + theme +
    geom_sf(color=NA) + 
    geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5, fill=NA) +
    theme + labs(title = "", fill="") +
    scale_fill_gradient(low="white", high="navy") + 
    theme(legend.position="none")
  ggsave(plot = p5, file = "USBBS_output/landscape_output/landcover_maps/wet_t2.png",units = "cm", dpi = "retina", width =27, height = 19)
  
  # landcover histogram
  hex.area <- 87
  
  usa_landscape[is.na(usa_landscape)]
  
  data <- usa_landscape %>%
    mutate(urban_t1 = coalesce(urban_t1, 0),
           forest_t1 = coalesce(forest_t1, 0),
           grass_t1 = coalesce(grass_t1, 0),
           crop_t1 = coalesce(crop_t1, 0),
           wet_t1 = coalesce(wet_t1, 0),
           urban_t2 = coalesce(urban_t2, 0),
           forest_t2 = coalesce(forest_t2, 0),
           grass_t2 = coalesce(grass_t2, 0),
           crop_t2 = coalesce(crop_t2, 0),
           wet_t2 = coalesce(wet_t2, 0))
  
  plot.data <- tibble("value"=c(sum(((exp(data$urban_t2) - 1)/100)*hex.area),
                                sum(((exp(data$forest_t2) - 1)/100)*hex.area),
                                sum(((exp(data$grass_t2) - 1)/100)*hex.area),
                                sum(((exp(data$crop_t2) - 1)/100)*hex.area),
                                sum(((exp(data$wet_t2) - 1)/100)*hex.area)),
                      "LC.class"=c("Urban", "Forest", "Grassland", "Cropland", "Wetland"))
  
  hist_t2 <- ggplot(plot.data, aes(x=LC.class, y=value, fill=LC.class)) +
    geom_bar(data=plot.data, stat="identity", position=position_dodge()) +
    #geom_bar(data=plot.data[plot.data$change<0,], stat="identity", alpha=0.6) +
    #scale_y_continuous(limits = c(0, 1e+05), breaks = seq(from = -(1e+05), to=1e+05, by =25000)) +
    scale_x_discrete(limits=c("Urban", "Forest", "Grassland", "Cropland", "Wetland")) + # order types on x axys
    scale_y_continuous(limits = c(0, 3500000), breaks = seq(from = 0, to=3000000, by = 1000000),
                       labels = c( "0", "1", "2", "3")) +
    scale_fill_manual(values=c("Grassland"='limegreen', "Forest"='darkgreen', "Cropland"='darkgoldenrod2', 
                               "Urban"='gray38', "Wetland"='purple', "other"='black')) +
    labs (x = "", y = "Land cover area in 2016 (10^6 km²)", title = "") + 
    theme_clean(base_size = 8) + 
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=11),
          plot.background = element_rect(color = "white"),
          panel.grid.major.y = element_line(size=0.2),
          legend.position='none',
          axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5))
  
  ggsave(plot = hist_t2, file = "USBBS_output/landscape_output/landcover_maps/hist_t2.png",units = "cm", dpi = "retina", width =27, height = 19)
  
  # landcover histogram
  
  library(cowplot)
  
  panel <- plot_grid(p1, p2, p3, p4, p5, hist_t2,
                     labels=c("A - Urban 2016","B - Forest 2016","C - Grassland 2016",
                              "D - Cropland 2016", "E- Wetland 2016", "F - Total area"),
                     label_size = 13, vjust = 2,
                     ncol=2, nrow=3)
  ggsave(plot = panel, file = "USBBS_output/landscape_output/landcover_maps/panel_t2.png",units = "cm", dpi = "retina", width =21, height = 23)
  
