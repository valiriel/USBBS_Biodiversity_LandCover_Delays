library(tidyverse)
library(rgdal)
library(lme4)

# data from observed and predictions
load("USBBS_Modelling_Delays/data.withpred.notemplag.rda")

# data from whole usa with predicted current and equilibrium species richness
#shp <- readOGR("D:/USBBS_DATA/USA_predict_map/USA_predict_notemplag.shp")
shp.xy <- readOGR("D:/USBBS_DATA/USBBS_LandCover/usaPredict_centroid.shp")

shp.xy <- spTransform(shp.xy, CRS("+init=epsg:4326")) # convert shapefile from NAD83 to WGS proj to obtain longitude and latitude

head(shp.xy@coords)

shp.xy$X <- shp.xy@coords[,1]
shp.xy$Y <- shp.xy@coords[,2]

plot(shp.xy$X , shp.xy$Y)

data <- data.frame(shp.xy@data)
data <- na.omit(data)

data <- data %>% transmute(partition=partitn, 
                           q0.eq.t2 = q0_eq,
                           q0.obs.t2 = q0_lag,
                           ################################
                           urban.t1=urbn_t1, forest.t1=frst_t1, wetland.t1=wtlnd_1,
                           grassland.t1=grssl_1, cropland.t1=crpln_1, temp.t1 = tmen_t1,
                           ################################
                           urban.change = dlt_rbn, pos.urban.change = dlt_ps_r,
                           forest.change = dlt_frs, pos.forest.change = dlt_ps_f, neg.forest.change = -dlt_ng_f,
                           grass.change = dlt_grs, pos.grass.change = dlt_ps_g, neg.grass.change = -dlt_ng_g,
                           crop.change = dlt_crp, pos.crop.change = dlt_ps_c, neg.crop.change = -dlt_ng_c,
                           wet.change= dlt_wtl, pos.wet.change = dlt_ps_w, neg.wet.change = -dlt_ng_w,
                           X, Y) %>%
  mutate(debtcredit = q0.eq.t2 - q0.obs.t2) %>%
  na.omit()

#' *Pie Chart for Fig1, percentage debt and credit across whole USA*

perc.colonization <- round(sum(data$debtcredit > 0.1)/length(data$debtcredit)*100)
perc.debt <- round(sum(data$debtcredit < (-0.1))/length(data$debtcredit)*100)
perc.equilibrium <- 100 - (perc.colonization + perc.debt)

values <- c(perc.colonization, perc.debt, perc.equilibrium)
names <- c("Colonization credits", "Extinction debts", "At equilibrium")

# Create Data
data2plot <- data.frame(
              group = c("Colonization credits", "Extinction debts", "At equilibrium"),
              values = c(perc.colonization, perc.debt, perc.equilibrium)
              )

# Basic pie chart
pie <- ggplot(data2plot, aes(x="", y=values, fill=group)) +
        geom_bar(stat="identity", width=1, colour="black") +
        coord_polar("y", start=0) +
        theme_void() + # remove background, grid, numeric labels
        theme(legend.position="none") +
        geom_text(aes(label = paste0(values, "%")), position = position_stack(vjust=0.5), 
                  colour="black", size=13) +
        scale_fill_manual(values=c("Colonization credits"="cornflowerblue", "Extinction debts"="coral3", 
                                   "At equilibrium"="grey95")) 
        
ggsave(plot = pie, "USBBS_Modelling_Delays/USBBS_Modelling_Output/Piechart_USAmap.svg",
       units = "cm", dpi = "retina", width =15, height = 15)
ggsave(plot = pie, "USBBS_Modelling_Delays/USBBS_Modelling_Output/Piechart_USAmap.tiff",
       units = "cm", dpi = "retina", width =15, height = 15) 

#############################################
#'
#' *Understanding debts and credits model*
#'

library(lme4)
library(MASS)
library(lmerTest)
library(cAIC4)
library(piecewiseSEM)
library(ggeffects)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggpubr)

hist(data$debtcredit,100)

################################################################################################################################################

model <- glm(debtcredit ~ pos.urban.change + pos.forest.change + neg.forest.change + pos.grass.change + 
                          neg.grass.change + pos.crop.change + neg.crop.change + pos.wet.change + neg.wet.change,
             data=data)
  
model <- step(model)
summary.model <- summary(model)
anova(model)

#save(model.normal, file="model.normal.rda")
#load("model.normal.rda")

plot_model(model, type="est",
           show.values = TRUE, show.p = FALSE, value.offset = .3,
           sort.est = TRUE, title="", vline.color = "black")

################################################################################################

plot.data <- data.frame( param = c("Urban gain", "Forest gain", "Forest loss", "Grassland gain", "Grassland loss", 
                                   "Cropland gain", "Cropland loss", "Wetland gain", "Wetland loss"),
                         estimate = c(summary.model$coefficients[2], summary.model$coefficients[3], summary.model$coefficients[4], summary.model$coefficients[5], summary.model$coefficients[6], summary.model$coefficients[7],
                                    summary.model$coefficients[8], summary.model$coefficients[9], summary.model$coefficients[10]),
                         lower.ci = c(summary.model$coefficients[2] - 1.96*summary.model$coefficients[2,2], summary.model$coefficients[3] - 1.96*summary.model$coefficients[3,2],
                                      summary.model$coefficients[4] - 1.96*summary.model$coefficients[4,2], summary.model$coefficients[5] - 1.96*summary.model$coefficients[5,2],
                                      summary.model$coefficients[6] - 1.96*summary.model$coefficients[6,2], summary.model$coefficients[7] - 1.96*summary.model$coefficients[7,2],
                                      summary.model$coefficients[8] - 1.96*summary.model$coefficients[8,2], summary.model$coefficients[9] - 1.96*summary.model$coefficients[9,2],
                                      summary.model$coefficients[10] - 1.96*summary.model$coefficients[9,2]),
                         upper.ci = c(summary.model$coefficients[2] + 1.96*summary.model$coefficients[2,2], summary.model$coefficients[3] + 1.96*summary.model$coefficients[3,2],
                                      summary.model$coefficients[4] + 1.96*summary.model$coefficients[4,2], summary.model$coefficients[5] + 1.96*summary.model$coefficients[5,2],
                                      summary.model$coefficients[6] + 1.96*summary.model$coefficients[6,2], summary.model$coefficients[7] + 1.96*summary.model$coefficients[7,2],
                                      summary.model$coefficients[8] + 1.96*summary.model$coefficients[8,2], summary.model$coefficients[9] + 1.96*summary.model$coefficients[9,2],
                                      summary.model$coefficients[10] + 1.96*summary.model$coefficients[10,2]),
                         sign = c("neg", "neg", "pos", "pos", "neg", "neg", "pos", "pos", "pos"))

plot.estimate <- plot.data %>% 
                  mutate(param = fct_reorder(param, estimate)) %>%
                    ggplot(aes(y = param, x =estimate, colour=sign)) +
                      geom_point(size=4) + 
                      geom_text(aes(label= round(estimate,2)),hjust=0, vjust=-1) +
                      geom_linerange(aes(xmax = upper.ci, xmin= lower.ci), size=1.25) +
                      geom_vline(xintercept=0) +
                        labs(y="", x="Estimate") +
                      theme_clean(base_size = 15) + 
                      theme(legend.position='none',
                              panel.background = element_blank(), axis.line = element_line(colour = "black"),
                              axis.text=element_text(size=15), axis.title=element_text(size=15),
                              plot.background = element_rect(color = "white"),
                              panel.grid.major.y = element_line(size=0.2))

ggsave(plot=plot.estimate, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/UnderstandingDelays_estimates.svg",
       units = "cm", dpi = "retina", width =15 , height = 21)


#############################################################################################
#'
#' /add change plot for areas wit debt and credits above 1 or belw -1/
#'

area <- 87

#'/Calculate change in km² for each land cover/
#'
#'/swith between credit and debt datasets/

urban.pos <- sum((data.lowerfifty$pos.urban.change/100)/area)

forest.pos <- sum((data.lowerfifty$pos.forest.change/100)/area)
  forest.neg <- -sum((data.lowerfifty$neg.forest.change/100)/area)

grassland.pos <- sum((data.lowerfifty$pos.grass.change/100)/area)
  grassland.neg <- -sum((data.lowerfifty$neg.grass.change/100)/area)

cropland.pos <- sum((data.lowerfifty$pos.crop.change/100)/area)
  cropland.neg <- -sum((data.lowerfifty$neg.crop.change/100)/area)

wetland.pos <- sum((data.lowerfifty$pos.wet.change/100)/area)
  wetland.neg <- -sum((data.lowerfifty$neg.wet.change/100)/area)

plot.data.debt <- tibble("value"=c(urban.pos, forest.pos, forest.neg, grassland.pos, grassland.neg, 
                                cropland.pos, cropland.neg, wetland.pos, wetland.neg),
                    "LC"=c("Urban", "Forest", "Forest", "Grassland", "Grassland", 
                             "Cropland", "Cropland", "Wetland", "Wetland"),
                    "Directionality"=c("positive", "positive", "negative", "positive", "negative", 
                                         "positive", "negative", "positive", "negative"))
  
lc_change_credit <- ggplot(plot.data.debt, aes(x=LC, y=value, fill=LC)) +
  geom_bar(data=plot.data.debt[plot.data.debt$value>0,], stat="identity") +
  geom_bar(data=plot.data.debt[plot.data.debt$value<0,], stat="identity", alpha=0.6) +
  scale_y_continuous(limits = c(-(6100), 6000), breaks = seq(from = -(6000), to=6000, by =3000),
                     labels = c("-6", "-3", "0", "3", "6")) +
  scale_x_discrete(limits=c("Grassland",  "Cropland", "Urban", "Forest", "Wetland")) + # order types on x axys
  scale_fill_manual(values=c("Grassland"='limegreen', "Forest"='darkgreen', "Cropland"='darkgoldenrod2', 
                             "Urban"='gray38', "Wetland"='cornflowerblue')) +
  labs (x = "", y = "Change in areas of credit (10³ km²)", title = "") +
  geom_hline(yintercept=0) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), legend.position='none', panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=18), axis.title=element_text(size=18))

ggsave(plot = lc_change_credit, "USBBS_Modelling_Delays/USBBS_Modelling_Output/credit_understand.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = lc_change_credit, "USBBS_Modelling_Delays/USBBS_Modelling_Output/credit_understand.tiff",
       units = "cm", dpi = "retina", width =29.7, height = 21) 

lc_change_debt <- ggplot(plot.data.debt, aes(x=LC, y=value, fill=LC)) +
  geom_bar(data=plot.data.debt[plot.data.debt$value>0,], stat="identity") +
  geom_bar(data=plot.data.debt[plot.data.debt$value<0,], stat="identity", alpha=0.6, colour="black") +
  scale_y_continuous(limits = c(-(8500), 6500), breaks = seq(from = -(8500), to=6500, by = 3000),
                     labels = c( "-9", "-6", "-3", "0", "3", "6")) +
  scale_x_discrete(limits=c("Grassland",  "Cropland", "Urban", "Forest", "Wetland")) + # order types on x axys
  scale_fill_manual(values=c("Grassland"='limegreen', "Forest"='darkgreen', "Cropland"='darkgoldenrod2', 
                             "Urban"='gray38', "Wetland"='cornflowerblue')) +
  labs (x = "", y = "Change in areas of debt (10³ km²)", title = "") +
  geom_hline(yintercept=0) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(), legend.position='none', panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=18), axis.title=element_text(size=18))

ggsave(plot = lc_change_debt, "USBBS_Modelling_Delays/USBBS_Modelling_Output/debt_understand.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = lc_change_debt, "USBBS_Modelling_Delays/USBBS_Modelling_Output/debt_understand.tiff",
       units = "cm", dpi = "retina", width =29.7, height = 21)

