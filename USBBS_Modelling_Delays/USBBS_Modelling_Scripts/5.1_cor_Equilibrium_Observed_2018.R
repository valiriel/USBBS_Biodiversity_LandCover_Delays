library(tidyverse)
library(viridis)
library(ggthemes)

load("USBBS_Modelling_Delays/data.withpred.18.centroid_springtemp.rda")

load("USBBS_Modelling_Delays/data.withpred.notemplag.rda")
load("USBBS_Modelling_Delays/alpha_18.rda")
alpha.18$partition <- str_replace(alpha.18$partition, pattern=" ", replacement="_")

data <- merge(data, alpha.18, by="partition")
data[,-(1:2)] <- round(data[,-(1:2)],3)

cor.test(data$q0.t2, data$q0.eq)
round(0.648^2,2) # explained variance
scatter.predSR.obsSR <- ggplot(data, aes(x=q0.eq, y=q0.t2)) +
                          #stat_density_2d(aes(fill = NULL), geom = "polygon", colour="white", alpha=0.1) +
                          geom_point(alpha = 0.15, colour = "black", size=1) + 
                          #geom_abline(intercept=0, slope=1) +
                          geom_smooth(size=1.75, colour="black", alpha=0.3, method="glm") +
                          #add model line                                      
                          scale_y_continuous(limits = c(0, 80)) + 
                          scale_x_continuous(limits = c(0, 80)) +
                          #annotate("text", x=60, y=15, label= "R\u00b2 = 0.43", size=4) +
                          labs(x=" Model predicted species richness in 2016", y="Observed species richness in 2016") +
                          theme_clean(base_size = 15) + 
                          theme(axis.text=element_text(size=12),
                                axis.title=element_text(size=18),
                                plot.background = element_rect(color = "white"),
                                panel.grid.major.y = element_line(size=0.2))

ggsave(plot = scatter.predSR.obsSR, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/obsSR~predSR.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = scatter.predSR.obsSR, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/obsSR~predSR.tiff",
       units = "cm", dpi = "retina", width =15 , height = 15)

data <- data %>% transmute(partition, 
                           q0.eq.t2 = q0.eq,
                           q0.lag.t2 = q0.lag,
                           q0.obs.t2 = q0.t2,
                           q0.obs.18 = q0.18) %>%
                            na.omit()

#'
#' *setup comparison df for 2008 model*

compare.df.18 <- data %>% transmute(partition,
                                    delta.q0.obs.16.18 = q0.obs.18 - q0.obs.t2, # difference in observed species between two timepoints
                                    delta.q0.eq16.obs16 = q0.eq.t2 - q0.obs.t2) # delta equilibrium in 2016 and observed in 2016
                                    
#' *we do not have land cover data for 2018 so cannot control for 0 change*

cor.test(compare.df.18$delta.q0.obs.16.18, compare.df.18$delta.q0.eq16.obs16)
round(0.39^2,2) # explained variance

scatter.18.debtcredit.deltaobs <- ggplot(compare.df.18, aes(x=delta.q0.eq16.obs16, y=delta.q0.obs.16.18)) +
                                        #stat_density_2d(aes(fill = NULL), geom = "polygon", colour="white", alpha=0.1) +
                                        geom_point(alpha = 0.15, colour = "black", size=1) + 
                                        geom_vline(xintercept=0, alpha=0.5) +
                                        geom_hline(yintercept=0, alpha=0.5) +  
                                        geom_smooth(size=1.75, colour="black", alpha=0.3, method="glm") +
                                        #geom_abline(intercept=-8.35, slope=0.245)+
                                        #add model line                                      
                                        scale_y_continuous(limits = c(-25, 15)) + 
                                        scale_x_continuous(limits = c(-30, 30)) +
                                        #annotate("text", x=25, y=-35, label= "R\u00b2 = 0.2", size=4) +
                                        labs(y=" Observed \u0394 species richness between 2018 and 2016", x="Forecasted extinction debts and colonization credits in 2016") +
                                        theme_clean(base_size = 15) + 
                                        theme(axis.text=element_text(size=12),
                                              axis.title=element_text(size=13),
                                              plot.background = element_rect(color = "white"),
                                              panel.grid.major.y = element_line(size=0.2))

ggsave(plot = scatter.18.debtcredit.deltaobs, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/scatter.18.debtcredit.deltaobs.svg",
       units = "cm", dpi = "retina", width =15, height = 15)
ggsave(plot = scatter.18.debtcredit.deltaobs, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/scatter.18.debtcredit.deltaobs.tiff",
       units = "cm", dpi = "retina", width =15 , height = 15)

model <- lm(delta.q0.obs.16.18 ~ delta.q0.eq16.obs16, data=compare.df.18)

summary(model)

plot(x=compare.df.18$delta.q0.eq16.obs16, y=compare.df.18$delta.q0.obs.16.18) + abline(a=-8.019, b=0.29,"blue")

