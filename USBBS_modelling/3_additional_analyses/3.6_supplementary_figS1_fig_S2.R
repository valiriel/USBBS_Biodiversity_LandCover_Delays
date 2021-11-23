library(sf); library(ggthemes); library(ggplot2); library(raster); library(rasterVis); library(dplyr)

usa_borders <- read_sf("USBBS_data/map_predict_data/USA_border.shp") %>% st_transform(crs=5070) %>% group_by(ID) %>% summarise()
bbs_routes <- read_sf("USBBS_data/routes_data/5_segments_final_3years.shp") %>% st_transform(crs=5070)

p <- ggplot(usa_borders) +
      geom_sf(color="grey50", size=0.5) +
      geom_sf(data=bbs_routes, size=1.2, color="darkred") +
      theme_minimal()
ggsave(p, file = "USBBS_output/modelling_output/supplementary_figures/Fig_S1_USBBS_routes.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(p, file = "USBBS_output/modelling_output/supplementary_figures/Fig_S1_USBBS_routes.png",
       units = "cm", dpi = "retina", width =29.7, height = 21)

#'---------------------------------------------------------------------------
#' * keep only one route for example figure *

mapview::mapview(bbs_routes)
bbs_routes$route <- substr(bbs_routes$partition, 0, 6)

route2plot <- bbs_routes %>% dplyr::filter(route == "92_051")
segment2plot <- route2plot[c(1,3,5),]
buffer2plot <- st_buffer(segment2plot, 500)

p <- ggplot(buffer2plot) +
        geom_sf(size=1, color="palegoldenrod", fill="palegoldenrod") + 
          geom_sf(data=route2plot, color="grey50", alpha=1, size=1) +
            geom_sf(data=segment2plot, size=2, color="black") +
      theme_minimal()

ggsave(p, file = "USBBS_output/modelling_output/supplementary_figures/Fig_S2_USBBS_segment_buffer.svg",
       units = "cm", dpi = "retina", width =18, height = 20)
ggsave(p, file = "USBBS_output/modelling_output/supplementary_figures/Fig_S2_USBBS_segment_buffer.png",
       units = "cm", dpi = "retina", width =18, height = 20)
