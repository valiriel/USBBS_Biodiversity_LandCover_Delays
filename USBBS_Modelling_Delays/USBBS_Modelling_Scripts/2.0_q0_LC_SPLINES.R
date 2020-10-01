library(tidyverse)
library(fields)
library(viridis)
library(coda)
library(ggpubr)

###################################################################
#'*SETUP DATA *
#'
load("data.centroid.springtemp.rda")

N <- nrow(data) # set number of observations per each var

####
data$delta.pos.urban <- vector("numeric",length = N)
data$delta.neg.urban <- vector("numeric",length = N)

for(i in 1:N){
  if(data$delta.urban[i]>0)
    data$delta.pos.urban[i] <- abs(data$delta.urban[i])
  if(data$delta.urban[i]<=0)
    data$delta.neg.urban[i] <- abs(data$delta.urban[i])
}

####
data$delta.pos.forest<- vector("numeric",length = N)
data$delta.neg.forest <- vector("numeric",length = N)

for(i in 1:N){
  if(data$delta.forest[i]>0)
    data$delta.pos.forest[i] <- abs(data$delta.forest[i])
  if(data$delta.forest[i]<=0)
    data$delta.neg.forest[i] <- abs(data$delta.forest[i])
}

####
data$delta.pos.grassland <- vector("numeric",length = N)
data$delta.neg.grassland <- vector("numeric",length = N)

for(i in 1:N){
  if(data$delta.grassland[i]>0)
    data$delta.pos.grassland[i] <- abs(data$delta.grassland[i])
  if(data$delta.grassland[i]<=0)
    data$delta.neg.grassland[i] <- abs(data$delta.grassland[i])
}

####
data$delta.pos.cropland <- vector("numeric",length = N)
data$delta.neg.cropland <- vector("numeric",length = N)

for(i in 1:N){
  if(data$delta.cropland[i]>0)
    data$delta.pos.cropland[i] <- abs(data$delta.cropland[i])
  if(data$delta.cropland[i]<=0)
    data$delta.neg.cropland[i] <- abs(data$delta.cropland[i])
}

####
data$delta.pos.wetland <- vector("numeric",length = N)
data$delta.neg.wetland <- vector("numeric",length = N)

for(i in 1:N){
  if(data$delta.wetland[i]>0)
    data$delta.pos.wetland[i] <- abs(data$delta.wetland[i])
  if(data$delta.wetland[i]<=0)
    data$delta.neg.wetland[i] <- abs(data$delta.wetland[i])
}

####
data$delta.pos.temp <- vector("numeric",length = N)
data$delta.neg.temp <- vector("numeric",length = N)

for(i in 1:N){
  if(data$delta.temp[i]>0)
    data$delta.pos.temp[i] <- abs(data$delta.temp[i])
  if(data$delta.temp[i]<=0)
    data$delta.neg.temp[i] <- abs(data$delta.temp[i])
}

###################################################################
#'
#'*Generate prediction by applying model to initial data*
#'

#load("USBBS_Modelling_Delays/USBBS_models_jags/results.q0.rda")
load("USBBS_Modelling_Delays/USBBS_models_jags/results.q0.centroid.springtemp.rda")

results <- results.q0.centroidspringtemp

attach(data)
source("USBBS_Modelling_Delays/USBBS_Modelling_Scripts/1.2_model_function.R")
detach(data)

data$debtcredit <- data$q0.eq - data$q0.lag

save(data, file="USBBS_Modelling_Delays/data.withpred.centroid.springtemp.rda")
#load("USBBS_Modelling_Delays/data.withpred.rda")
#load("USBBS_Modelling_Delays/data.withpred.buffer_springtemp.rda")

summary(data)

# check how many remain at 0
if(sum(data$q0.eq == 0)!=0) stop(print("ACHTUNG! ACHTUNG!, some species richness are = to 0"))

#################################################
#' *Figures panel*

scatter.q0 <- ggplot(data, aes(x=q0.t2, y = q0.eq)) + 
  geom_point( color='black', alpha=0.15)+
  theme_bw() + 
  scale_x_continuous( limits = c(0, 100), breaks = seq(from=0, to=85, by=10)) +
  scale_y_continuous( limits = c(0, 100), breaks = seq(from = 0, to=85, by =10)) + 
  geom_abline(intercept = 0, slope = 1) +
  labs (x = "Observed SR", y = "pred SR", title = '')

ggsave(plot = scatter.q0, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/srobs~srpred_spline.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = scatter.q0, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/srobs~srpred_spline.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

cor.test(data$q0.t2, data$q0.eq)

#################################################
#'
#' *Urban spline.plot*
#'

spline.urban <- ggplot(data, aes(x=urban.t2)) + 
  geom_point(aes(y=q0.t2), color='gray38', alpha=0.15)+
  geom_smooth(aes(y=q0.eq), colour='black', alpha=0.5, method="auto", fullrange=T) + 
  #geom_smooth(aes(y=q0.eq),formula= y ~ s(x, bs = "cs"), colour='violetred', alpha=0.5, method="gam") +
  theme_bw() + 
  scale_x_continuous( limits = c(0, 100), breaks = seq(from=0, to=100, by=20)) +
  #scale_x_log10(limits = c(-5, 100), breaks = seq(from=0, to=100, by=10)) +
  scale_y_continuous( limits = c(0, 80), breaks = seq(from = 0, to=80, by =10)) + 
  labs (x = "Urban %", y = "Species richness", title = '') + 
  geom_rug(colour="black", alpha=0.05)

ggsave(plot = spline.urban, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~urban_spline.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = spline.urban, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~urban_spline.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

#################################################
#'
#' *Cropland spline.plot*
#'

spline.cropland <- ggplot(data, aes(x=cropland.t2)) + 
  geom_point(aes(y=q0.t2), color='darkgoldenrod2', alpha=0.15)+
  geom_smooth(aes(y=q0.eq),  colour='black', alpha=0.5, method="auto", fullrange=T) + 
  theme_bw() + 
  scale_x_continuous( limits = c(0, 100), breaks = seq(from=0, to=100, by=20)) +
  #scale_x_log10(limits = c(-5, 100), breaks = seq(from=0, to=100, by=10)) +
  scale_y_continuous( limits = c(0, 80), breaks = seq(from = 0, to=80, by =10)) + 
  labs (x = "Cropland %", y = "Species richness", title = '') + 
  geom_rug(colour="black", alpha=0.05 )

ggsave(plot = spline.cropland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~cropland_spline.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = spline.cropland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~cropland_spline.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

#################################################
#'
#' *Grassland spline.plot*
#'

spline.grassland <- ggplot(data, aes(x=grassland.t2)) + 
  geom_point(aes(y=q0.t2), color='limegreen', alpha=0.15)+
  geom_smooth(aes(y=q0.eq), colour='black', alpha=0.5, method="auto", fullrange=T) +
  theme_bw() + 
  scale_x_continuous( limits = c(0, 100), breaks = seq(from=0, to=100, by=20)) +
  #scale_x_log10(limits = c(-5, 100), breaks = seq(from=0, to=100, by=10)) +
  scale_y_continuous( limits = c(0, 80), breaks = seq(from = 0, to=80, by =10)) + 
  labs (x = "Grassland %", y = "Species richness", title = '') + 
  geom_rug(colour="black", alpha=0.05 )

ggsave(plot = spline.grassland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~grassland_spline.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = spline.grassland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~grassland_spline.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

#################################################
#'
#' *Forest spline.plot*
#'

spline.forest <- ggplot(data, aes(x=forest.t2)) + 
  geom_point(aes(y=q0.t2), color='darkgreen', alpha=0.15)+
  geom_smooth(aes(y=q0.eq), colour='black', alpha=0.5, method="auto", fullrange=T) + 
  theme_bw() +  
  scale_x_continuous( limits = c(0, 100), breaks = seq(from=0, to=100, by=20)) +
  #scale_x_log10(limits = c(-5, 100), breaks = seq(from=0, to=100, by=10)) +
  scale_y_continuous( limits = c(0, 80), breaks = seq(from = 0, to=80, by = 10)) + 
  labs (x = "Forest %", y = "Species richness", title = '') + 
  geom_rug(colour="black", alpha=0.05 )

ggsave(plot = spline.forest, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~forest_spline.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = spline.forest, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~forest_spline.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

#################################################
#'
#' *Temperature spline.plot*
#'

spline.temp <- ggplot(data, aes(x=temp.t2)) + 
  geom_point(aes(y=q0.t2), color='darkred', alpha=0.15)+
  geom_smooth(aes(y=q0.eq), colour='black', alpha=0.5, method="auto", fullrange=T) + 
  theme_bw() +  
  #scale_x_continuous( limits = c(-5, 30), breaks = seq(from=0, to=100, by=5)) +
  #scale_x_log10(limits = c(-5, 100), breaks = seq(from=0, to=100, by=10)) +
  scale_y_continuous( limits = c(0, 80), breaks = seq(from = 0, to= 80, by = 10)) + 
  labs (x = "Temperature (°C)", y = "Species richness", title = '') + 
  geom_rug(colour="black", alpha=0.05 )

ggsave(plot = spline.temp, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~temp_spline.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = spline.temp, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~temp_spline.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

#################################################
#'
#' *Wetland spline.plot*
#'

spline.wetland <- ggplot(data, aes(x=wetland.t2)) + 
  geom_point(aes(y=q0.t2), color='cornflowerblue', alpha=0.15)+
  geom_smooth(aes(y=q0.eq), colour='black', alpha=0.5, method="auto", fullrange=T) + 
  theme_bw() +  
  scale_x_continuous( limits = c(0, 100), breaks = seq(from=0, to=100, by=20)) +
  #scale_x_log10(limits = c(-5, 100), breaks = seq(from=0, to=100, by=10)) +
  scale_y_continuous( limits = c(0, 80), breaks = seq(from = 0, to=100, by =10)) + 
  labs (x = "Wetland %", y = "Species richness", title = '') + 
  geom_rug(colour="black", alpha=0.05 )

ggsave(plot = spline.wetland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~wetland_spline.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = spline.wetland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~wetland_spline.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

#################################################
#'
#' *All land cover spline.*
#'

spline.all <- ggplot(data, aes(y=q0.eq)) +
                geom_smooth(aes(x=wetland.t2), colour='cornflowerblue', alpha=0.2, method="auto", size=1.1, fullrange=T) + 
                geom_smooth(aes(x=grassland.t2), colour='limegreen', alpha=0.2, method="auto", size=1.1, fullrange=T) + 
                geom_smooth(aes(x=cropland.t2), colour='darkgoldenrod2', alpha=0.2, method="auto", size=1.1, fullrange=T) +
                geom_smooth(aes(x=forest.t2), colour='darkgreen', alpha=0.2, method="auto", size=1.1, fullrange=T) + 
                geom_smooth(aes(x=urban.t2),colour='grey38', alpha=0.2, method="auto", size=1.1, fullrange=T) + 
                theme_bw() +  
                scale_x_continuous( limits = c(0, 100), breaks = seq(from=0, to=100, by=10)) +
                #scale_x_log10(limits = c(-5, 100), breaks = seq(from=0, to=100, by=10)) +
                scale_y_continuous( limits = c(0, 50), breaks = seq(from = 0, to=100, by =5)) + 
                labs (x = "Land cover %", y = "Species richness", title = '') + 
                geom_rug(colour="black", alpha=0.05 )

ggsave(plot = spline.all, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~all_spline.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
ggsave(plot = spline.all, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/sr~all_spline.pdf",
       units = "cm", dpi = "retina", width =15 , height = 15)

#################################################
#'
#' *Figures panel*
#'

spline.panel <- ggarrange(spline.forest + rremove("grid"), 
                            spline.urban + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"), 
                            spline.wetland + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"),
                          spline.grassland+ rremove("grid"), 
                            spline.cropland + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"), 
                            spline.temp + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"),
                          labels = c("A", "B", "C", "D", "E", "F"),
                          label.x = 0.92, label.y = 0.925,
                          font.label = list(size = 13, face = "bold", color ="black"),
                          ncol = 3, nrow = 2,
                          widths = c(1,0.9,0.9), heights = c(1,1))

ggsave(plot = spline.panel, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/PANEL_sr~vars_spline.svg",
       units = "cm", dpi = "retina", width =29.7 , height = 21)
ggsave(plot = spline.panel, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/sr~vars_splines/PANEL_sr~vars_spline.pdf",
       units = "cm", dpi = "retina", width =29.7 , height = 21)

