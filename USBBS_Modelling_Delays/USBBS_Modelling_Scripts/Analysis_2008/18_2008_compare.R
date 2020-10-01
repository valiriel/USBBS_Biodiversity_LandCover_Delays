library(tidyverse)

load("data.2008.pred.rda")
data.08 <- data
load("data.iter.withpred.rda")
data.t2 <- data

partition.08 <- data.08$partition

data.t2 <- data.t2 %>% filter(partition %in% partition.08) %>% 
                          mutate(q0.eq.t2 = q0.eq,
                                 q0.lag.t2 = q0.lag,
                                 q0.obs.t2 = q0.t2) %>%
                                    select(partition, q0.obs.t2, q0.eq.t2, q0.lag.t2, 
                                           urban.t2, grassland.t2, wetland.t2, forest.t2, cropland.t2, tmean.t2,
                                           -q0.eq, - q0.lag, -q0.t2)
                          
data.08 <- data.08 %>% transmute(partition,
                                 q0.obs.t1 = q0.t1,
                                 q0.obs.t08 = q0.t08,
                                 q0.lag.t08 = q0.eq,
                                 q0.eq.t08 = q0.now,
                                 urban.t1, urban.t08, forest.t1, forest.t08, cropland.t1, cropland.t08,
                                 grassland.t1, grassland.t08, wetland.t1, wetland.t08, tmean.t1, tmean.t08)

data <- merge(data.08, data.t2, by = "partition")

#'
#' *setup comparison df for 2008 model*

compare.df.08 <- data %>% transmute(partition,
                                    delta.q0.obs.08.16 = q0.obs.t2 - q0.obs.t08, # difference in observed species between two timepoints
                                    delta.q0.eq08.obs08 = q0.eq.t08 - q0.obs.t08, # delta equilibrium in 2008 and observed in 2008
                                    delta.lc.t2.08 = abs(urban.t2 - urban.t08) + abs(forest.t2 - forest.t08) + abs(grassland.t2 - grassland.t08) +
                                                     abs(cropland.t2 - cropland.t08) + abs(wetland.t2 - wetland.t08) + abs(tmean.t2 - tmean.t08)) 

#' *it does nothing to subset for landscapes with close to 0 land cover and temp changes*
#compare.df.08 <- compare.df.08 %>% filter(delta.lc.t2.08 < 10) 

scatter.08.debtcredit.deltaobs <- ggplot(compare.df.08, aes(x=delta.q0.eq08.obs08, y=delta.q0.obs.08.16)) +
                                    #stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white") + 
                                    geom_point(alpha = 0.1, colour = "black", size=0.75) + 
                                    geom_vline(xintercept=0, alpha=0.5) +
                                    geom_hline(yintercept=0, alpha=0.5) +  
                                    geom_smooth(size=1.75, colour="mediumvioletred", alpha=0.3, method="lm") +
                                    scale_y_continuous(limits = c(-25, 20)) + 
                                    annotate("text", x=40, y=-35, label= "R\u00b2 = 0.20", size=4) +
                                    labs(y=" Observed \u0394 species richness between 2016 and 2008", x="Predicted extinction debts and colonization credits in 2008") + 
                                    theme_minimal() + theme(legend.position="none") 

#CONTOUR PLOT

ggsave(plot = scatter.08.debtcredit.deltaobs, file = "USBBS_Modelling_Output/scatter.08.debtcredit.deltaobs.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = scatter.08.debtcredit.deltaobs, file = "USBBS_Modelling_Output/scatter.08.debtcredit.deltaobs.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)


smoothScatter(y=compare.df.08$delta.q0.eq08.obs08, x=compare.df.08$delta.q0.obs.08.16,
              nbin = 256, nrpoints = 100, 
              colramp = colorRampPalette(c("white", "red")),
              xlab="Obs q0 2016 - 2008", ylab="2008 debt and credit")

smoothScatter(y=compare.df.08$delta.q0.obs.08.16, x=compare.df.08$delta.q0.eq08.obs08,
              nbin = 256, nrpoints = 100, 
              colramp = colorRampPalette(c("white", "red")),
              xlab="Obs q0 2016 - 2008", ylab="2008 debt and credit")
  


cor.test(compare.df.08$delta.q0.obs.08.16, compare.df.08$delta.q0.eq08.obs08)

model <- lm(delta.q0.obs.08.16 ~ delta.q0.eq08.obs08, data=compare.df.08)

summary(model)














