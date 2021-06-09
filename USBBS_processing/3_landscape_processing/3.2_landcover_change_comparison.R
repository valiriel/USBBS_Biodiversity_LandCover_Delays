library(tidyverse); library(sf); library(ggpubr); library(ggthemes)

shp <- read_sf("USBBS_data/routes_data/5_segments_final_2years.shp") %>%  
        mutate(route = str_sub(partition,1, 6)) %>%  st_transform(5070)
shp$partition[shp$route == "53_800"] <- paste0("53_800_", 1:5)

theme <- theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=10), 
                               #axis.ticks=element_blank(), axis.line=element_blank(), 
                               legend.text=element_text(size=14), legend.title=element_text(size=15), 
                               plot.background = element_rect(color = "white"), 
                               #panel.grid = element_blank(), panel.border= element_blank(),
                               strip.text=element_text(size=15))

#'------------------------------------------------------------------------------------------------------------
#'*Comparison between land cover change calculated as difference between timepoint and land cover change product*
#'

size.buffer <- 500
b <- "centroid"

buffers <- c(500, 1000, 2000, 4000, 6000) 
buffer.type <- c("centroid", "segment")

# setup loop across buffer sizes
for (size.buffer in buffers) {
  
  # setup loop across lc years   
  for(b in buffer.type) {
    
    load(paste0("USBBS_data/landscape_data/landcover_data/landcover_change_product_extracted/lc_change_", b, "_", size.buffer, ".rda"))
    lc <- lc[-c(651:670),] ; lc$partition[lc$route=="17_222"] <- paste0("17_222_",1:5) 
    changes <-  as.data.frame(lc) %>% dplyr::select(partition, route, starts_with("change.")) %>% as_tibble()
    
    load(paste0("USBBS_data/landscape_data/landcover_data/landcover_%_&_delta_extracted/lc_", b, "_", size.buffer, ".rda"))      
    lc <- lc[-c(651:670),] ; lc$partition[lc$route=="17_222"] <- paste0("17_222_",1:5) 
    deltas <- as.data.frame(lc) %>% dplyr::select(partition, route, starts_with("delta.")) %>% as_tibble()
    
    data <- left_join(deltas, changes, by="partition")
  
    p1 <- ggplot(data) + geom_point(aes(x=delta.urban, y= change.urban), color="grey50") + theme + labs(x= "calculated delta", y="product delta", title=paste(b, 500, "m")) 
    p2 <- ggplot(data) + geom_point(aes(x=delta.forest, y= change.forest), color="darkgreen") + theme + labs(x= "calculated delta", y="product delta")
    p3 <- ggplot(data) + geom_point(aes(x=delta.grass, y= change.grass), color="lawngreen") + theme + labs(x= "calculated delta", y="product delta")
    p4 <- ggplot(data) + geom_point(aes(x=delta.wet, y= change.wet), color="darkmagenta") + theme + labs(x= "calculated delta", y="product delta")
    p5 <- ggplot(data) + geom_point(aes(x=delta.crop, y= change.crop), color="darkgoldenrod1") + theme + labs(x= "calculated delta", y="product delta")
    p6 <- ggplot(data) + geom_point(aes(x=delta.water, y= change.water), color="navy") + theme + labs(x= "calculated delta", y="product delta")
    #ggplot(data) + geom_point(aes(x=delta.barren, y= change.barren), color="burlywood1") + theme + labs(x= "calculated delta", y="product delta")
    
    p7 <- ggarrange(p1 + rremove("xlab"), p2 + rremove("ylab") + rremove("xlab"), 
                    p3 + rremove("ylab") + rremove("xlab"), p4, p5 + rremove("ylab") , 
                    p6 + rremove("ylab"))
    
    ggsave(plot=p7,  paste0("USBBS_output/landscape_output/landcover_change_comparison/comaparison_", b, "_", size.buffer,".svg"),
           units = "cm", dpi = "retina", width =30 , height = 20)
    
  }
}


# something went wrong with route 17_222, fixed
which(deltas$route=="17_222") 
lc <- lc[-c(651:670),] ; lc$partition[lc$route=="17_222"] <- paste0("17_222_",1:5) 

