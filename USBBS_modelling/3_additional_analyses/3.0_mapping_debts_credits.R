library(ggthemes);  library(tidyverse); library(sf)

theme <- theme_clean() + 
  theme(axis.title=element_text(size=16), axis.text=element_text(size=14),
        legend.text=element_text(size=12), legend.title=element_text(size=16), 
        plot.background = element_rect(color = "white"), 
        panel.border= element_blank(), plot.title=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.background = element_rect(color = NA),
        strip.background.y = element_rect(color = "grey30"))

load("USBBS_data/map_predict_data/USA_landscape_predicted.rda"); usa_landscape$debtcredit_mean[is.na(usa_landscape$debtcredit_mean)] <-0
usa_borders <- read_sf("USBBS_data/map_predict_data/USA_border.shp") %>% st_transform(crs=5070) %>% group_by(ID) %>% summarise()

#st_write(usa_landscape, "USBBS_data/map_predict_data/USA_landscape_predicted.shp", append=FALSE)

#'--------------------------------------------------------------------------------------------------
#'
#'* Visualizations for debts and credits values *
#'

ggplot(usa_landscape, aes(x=debtcredit_mean, color=debtcredit_mean, fill=debtcredit_mean)) + 
  geom_histogram(bins=100) +
  scale_fill_gradient2(low="red3", mid="white", high="royalblue3", midpoint = 0) +
  theme

summary(usa_landscape); hist(usa_landscape$debtcredit_sd, 100); hist(usa_landscape$debtcredit_cv, 100)

#'--------------------------------------------------------------------------------------------------
#'* Maps for debts and credits values *
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> master
#'

p1 <- ggplot() + 
        geom_sf(data = usa_landscape, aes(fill=debtcredit_mean), color=NA) +
        geom_sf(data=usa_borders, color="black", size=0.5, alpha=0.5) + 
  scale_fill_gradientn(colours = c("darkred", "firebrick1", "white", "royalblue2", "navy"),
                       limits = c(-12, 12), breaks = c(-12, 0, 12), labels = c(-12, 0, 9)) +
        #scale_fill_gradient2(low="red3", mid="white", high="royalblue3", midpoint = 0,
        #                     limits = c(-12, 9), breaks = c(-12, 0, 9), labels = c(-12, 0, 9)) +
<<<<<<< HEAD
        theme #+ labs(title = "Debts & Credits mean", fill="") 
=======
        theme + labs(title = "Debts & Credits mean", fill="") 
>>>>>>> master
ggsave(plot = p1, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_mean.png",units = "cm", dpi = "retina", width =33, height = 19)

##############################

q <- as.numeric(quantile(usa_landscape$debtcredit_mean, prob=c(.1, .25, .5, .75, .9),na.rm = T))

#' *pie chart *
# using a 0 effective species cutoff

<<<<<<< HEAD
data2plot <- tibble(type = c("Credit", "Debt"),#, "perc.eq"),
=======
data2plot <- tibble(type = c("perc.debt", "perc.credit", "perc.eq"),
>>>>>>> master
                    perc = c(round(sum(usa_landscape$debtcredit_mean > 0)/nrow(usa_landscape)*100,0),
                             round(sum(usa_landscape$debtcredit_mean < 0)/nrow(usa_landscape)*100,0)),
                             #round(sum(usa_landscape$debtcredit_mean > 0 & usa_landscape$debtcredit_mean < 0)
                                   #/nrow(usa_landscape)*100,0)),
                    amount = c(round(sum(usa_landscape$debtcredit_mean[usa_landscape$debtcredit_mean > 0]),0),
                               round(sum(usa_landscape$debtcredit_mean[usa_landscape$debtcredit_mean < 0]),0)))
                               #round(sum(usa_landscape$debtcredit_mean[usa_landscape$debtcredit_mean == 0]),0)))
data2plot

pie_chart <- ggplot(data2plot, aes(x="", y=perc, fill=type)) +
  geom_bar(stat="identity", width=1, color="gray50", alpha=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(perc, "%")), 
            position = position_stack(vjust=0.5), size=10) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme + theme(axis.line.x = element_blank(), axis.line.y = element_blank(), axis.ticks = element_blank(),
                legend.position = "none", axis.text.x=element_text(color='white'))  +
  scale_fill_manual(values=c("cornflowerblue", "coral1", "white"))
ggsave(plot = pie_chart, file = "USBBS_output/modelling_output/debts_credits_maps/pie_chart.png",units = "cm", dpi = "retina", width =10, height = 10)
ggsave(plot = pie_chart, file = "USBBS_output/modelling_output/debts_credits_maps/pie_chart.svg",units = "cm", dpi = "retina", width =10, height = 10)

<<<<<<< HEAD
data2plot$amount[2] <- data2plot$amount[2] * -1 # make species positive 4 plot
bar_chart <- ggplot(data2plot, aes(x=type, y=amount, fill=type)) +
              geom_bar(colour="black", stat="identity") + theme +
              scale_fill_manual(values=c("cornflowerblue", "coral1", "white")) +
              labs(x = "", y = "Effective number of species") +
              theme(legend.position="none", 
                    axis.title=element_text(size=15), axis.text=element_text(size=15),
                    legend.text=element_text(size=10), legend.title=element_text(size=12))
ggsave(plot = bar_chart, file = "USBBS_output/modelling_output/debts_credits_maps/bar_chart.png",units = "cm", dpi = "retina", width =10, height = 16)
ggsave(plot = bar_chart, file = "USBBS_output/modelling_output/debts_credits_maps/bar_chart.svg",units = "cm", dpi = "retina", width =10, height = 16)

#'*map*
#'
=======
#'

p1 <- ggplot() + 
        geom_sf(data = usa_landscape, aes(fill=debtcredit_mean), color=NA) +
        geom_sf(data=usa_borders, color="black", size=0.5, alpha=0.5) + 
  scale_fill_gradientn(colours = c("darkred", "firebrick1", "white", "royalblue2", "navy"),
                       limits = c(-12, 12), breaks = c(-12, 0, 12), labels = c(-12, 0, 9)) +
        #scale_fill_gradient2(low="red3", mid="white", high="royalblue3", midpoint = 0,
        #                     limits = c(-12, 9), breaks = c(-12, 0, 9), labels = c(-12, 0, 9)) +
        theme + labs(title = "Debts & Credits mean", fill="") 
ggsave(plot = p1, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_mean.png",units = "cm", dpi = "retina", width =33, height = 19)

##############################

q <- as.numeric(quantile(usa_landscape$debtcredit_mean, prob=c(.1, .25, .5, .75, .9),na.rm = T))

#' *pie chart *
# using a 0 effective species cutoff

data2plot <- tibble(type = c("perc.debt", "perc.credit", "perc.eq"),
                    perc = c(round(sum(usa_landscape$debtcredit_mean > 0)/nrow(usa_landscape)*100,0),
                             round(sum(usa_landscape$debtcredit_mean < 0)/nrow(usa_landscape)*100,0),
                             round(sum(usa_landscape$debtcredit_mean == 0)/nrow(usa_landscape)*100,0)),
                    amount = c(round(sum(usa_landscape$debtcredit_mean[usa_landscape$debtcredit_mean > 0]),0),
                               round(sum(usa_landscape$debtcredit_mean[usa_landscape$debtcredit_mean < 0]),0),
                               round(sum(usa_landscape$debtcredit_mean[usa_landscape$debtcredit_mean == 0]),0)))
data2plot

pie_chart <- ggplot(data2plot, aes(x="", y=perc, fill=type)) +
  geom_bar(stat="identity", width=1, color="gray50", alpha=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(perc, "%")), 
            position = position_stack(vjust=0.5), size=3.5) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme + theme(axis.line.x = element_blank(), axis.line.y = element_blank(), axis.ticks = element_blank(),
                legend.position = "none", axis.text.x=element_text(color='white'))  +
  scale_fill_manual(values=c("cornflowerblue", "coral1", "white"))

#'*map*
#'
>>>>>>> master
=======
#'*map*
#'
>>>>>>> master
usa_landscape$type <- ""
usa_landscape$type[usa_landscape$debtcredit_mean > q[2] & usa_landscape$debtcredit_mean < q[4] ] <- "near equilibrium (25% < x < 75% quantile)"
usa_landscape$type[usa_landscape$debtcredit_mean <= q[2]] <- "debt (< 25% quantile)"
usa_landscape$type[usa_landscape$debtcredit_mean <= q[1]] <- "debt (< 10% quantile)"
usa_landscape$type[usa_landscape$debtcredit_mean >= q[4]] <- "credit (> 75% quantile)"
usa_landscape$type[usa_landscape$debtcredit_mean >= q[5]] <- "credit (> 90% quantile)"
usa_landscape$type <- factor(usa_landscape$type, levels = c("debt (< 10% quantile)", "debt (< 25% quantile)", "near equilibrium (25% < x < 75% quantile)", 
                                                            "credit (> 75% quantile)", "credit (> 90% quantile)"))
p2 <- ggplot() + theme +
  geom_sf(data = usa_landscape, aes(fill=type), color=NA) +
  geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5) +
  theme + labs(title = "", fill="") +
  scale_fill_manual(values =c("darkred", "coral", "white", "cornflowerblue", "navy")) + 
  theme(legend.position="bottom", axis.line = element_blank(), axis.text=element_blank(), axis.title=element_blank(),
        axis.ticks=element_blank(), legend.key = element_rect(colour = "black"),
        legend.text=element_text(size=10), legend.title=element_text(size=12))
ggsave(plot = p2, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_quantiles.png",units = "cm", dpi = "retina", width =27, height = 19)
ggsave(plot = p2, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_quantiles.svg",units = "cm", dpi = "screen", width =27, height = 19)

######################
sum(usa_landscape$debtcredit_geom_cv > 1, na.rm = T) # 28872
sum(usa_landscape$debtcredit_geom_cv[usa_landscape$debtcredit_mean > 0.5 | usa_landscape$debtcredit_mean < -0.5]>5, na.rm = T) 
# 81 higher than 1 # 0 higher than 5

p2.cv <- ggplot() + 
<<<<<<< HEAD
<<<<<<< HEAD
        geom_sf(data = usa_landscape, aes(fill=debtcredit_geom_cv), color=NA) +
        geom_sf(data=usa_borders, color="black") +
        scale_fill_viridis_c(option = "D") +
  theme + theme(legend.position="right", axis.line = element_blank(), axis.text=element_blank(), axis.title=element_blank(),
                axis.ticks=element_blank(),
                legend.text=element_text(size=10), legend.title=element_text(size=12)) +
  labs(fill="Geometric cv")
ggsave(plot = p2.cv, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_cv.png", units = "cm", dpi = "retina", width =28, height = 17)


#'----------------------------------------------------------------------------------------
#' *upper boundary*

q <- as.numeric(quantile(usa_landscape$debtcredit_upper, prob=c(.1, .25, .5, .75, .9),na.rm = T))

usa_landscape$type <- ""
usa_landscape$type[usa_landscape$debtcredit_upper > q[2] & usa_landscape$debtcredit_upper < q[4] ] <- "near equilibrium (25% < x < 75% quantile)"
usa_landscape$type[usa_landscape$debtcredit_upper <= q[2]] <- "debt (< 25% quantile)"
usa_landscape$type[usa_landscape$debtcredit_upper <= q[1]] <- "debt (< 10% quantile)"
usa_landscape$type[usa_landscape$debtcredit_upper >= q[4]] <- "credit (> 75% quantile)"
usa_landscape$type[usa_landscape$debtcredit_upper >= q[5]] <- "credit (> 90% quantile)"
usa_landscape$type <- factor(usa_landscape$type, levels = c("debt (< 10% quantile)", "debt (< 25% quantile)", "near equilibrium (25% < x < 75% quantile)", 
                                                            "credit (> 75% quantile)", "credit (> 90% quantile)"))
usa_landscape$type[is.na(usa_landscape$type)] <- "near equilibrium (25% < x < 75% quantile)"

p2.upper <- ggplot() + theme +
  geom_sf(data = usa_landscape, aes(fill=type), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  labs(fill="Upper CI") +
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "navy", midpoint = 0) +
  scale_fill_manual(values =c("darkred", "coral", "white", "cornflowerblue", "navy")) + 
    theme(legend.position="bottom", axis.line = element_blank(), axis.text=element_blank(), axis.title=element_blank(),
        axis.ticks=element_blank(), legend.key = element_rect(colour = "black"),
        legend.text=element_text(size=10), legend.title=element_text(size=12))
ggsave(plot = p2.upper, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_upper_quantile.png", units = "cm", dpi = "retina", width =28, height = 17)

#'----------------------------------------------------------------------------------------
#' *lower boundary*

q <- as.numeric(quantile(usa_landscape$debtcredit_lower, prob=c(.1, .25, .5, .75, .9),na.rm = T))

usa_landscape$type <- ""
usa_landscape$type[usa_landscape$debtcredit_lower > q[2] & usa_landscape$debtcredit_lower < q[4] ] <- "near equilibrium (25% < x < 75% quantile)"
usa_landscape$type[usa_landscape$debtcredit_lower <= q[2]] <- "debt (< 25% quantile)"
usa_landscape$type[usa_landscape$debtcredit_lower <= q[1]] <- "debt (< 10% quantile)"
usa_landscape$type[usa_landscape$debtcredit_lower >= q[4]] <- "credit (> 75% quantile)"
usa_landscape$type[usa_landscape$debtcredit_lower >= q[5]] <- "credit (> 90% quantile)"
usa_landscape$type <- factor(usa_landscape$type, levels = c("debt (< 10% quantile)", "debt (< 25% quantile)", "near equilibrium (25% < x < 75% quantile)", 
                                                            "credit (> 75% quantile)", "credit (> 90% quantile)"))
usa_landscape$type[is.na(usa_landscape$type)] <- "near equilibrium (25% < x < 75% quantile)"

p2.lower <- ggplot() + theme +
  geom_sf(data = usa_landscape, aes(fill=type), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  labs(fill="Lower CI") +
  #scale_fill_gradient2(low = "darkred", mid = "white", high = "navy", midpoint = 0) +
  scale_fill_manual(values =c("darkred", "coral", "white", "cornflowerblue", "navy")) + 
  theme(legend.position="bottom", axis.line = element_blank(), axis.text=element_blank(), axis.title=element_blank(),
        axis.ticks=element_blank(), legend.key = element_rect(colour = "black"),
        legend.text=element_text(size=10), legend.title=element_text(size=12))
ggsave(plot = p2.lower, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_lower_quantile.png", units = "cm", dpi = "retina", width =28, height = 17)

#'------------------------------------------------------------------------------------
#'
usa_landscape$debtcredit_lower - usa_landscape$debtcredit_upper

length(usa_landscape$debtcredit_cv[usa_landscape$debtcredit_cv<20])/length(usa_landscape$debtcredit_cv) * 100
p2.cv.hist <- ggplot(data = usa_landscape) + 
  geom_histogram(aes(x=debtcredit_geom_cv), bins = 1000) +
  theme + labs(x="Geometric coefficient of variation", y="Count") +
  scale_x_continuous(limits = c(0, 10))
ggsave(plot = p2.cv.hist, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_cv_hist.png", units = "cm", dpi = "retina", width =20, height = 20)

p <- cowplot::plot_grid(p2.upper, p2.lower, labels = c('A', 'B'), nrow=2)
ggsave(plot = p, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_PANEL_S7.png", units = "cm", dpi = "retina", width =15, height = 19)
=======
=======
>>>>>>> master
        geom_sf(data = usa_landscape, aes(fill=debtcredit_cv), color=NA) +
        geom_sf(data=usa_borders, color="black") +
        scale_fill_viridis_c(option = "E") + 
        theme + labs(fill="Coefficient of variation") + theme(legend.position="bottom")
ggsave(plot = p2.cv, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_cv.png", units = "cm", dpi = "retina", width =28, height = 19)
<<<<<<< HEAD
>>>>>>> master
=======
>>>>>>> master

p2.sd <- ggplot() + 
  geom_sf(data = usa_landscape, aes(fill=debtcredit_sd), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_viridis_c(option = "E") + 
  theme + labs(fill="sd")
ggsave(plot = p2.sd, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_sd.png", units = "cm", dpi = "retina", width =33, height = 19)

#'--------------------------------------------------------------------------------------------------
#' * Equilibrium effective number of species *

p3 <- ggplot() + 
  geom_sf(data = usa_landscape, aes(fill=eq_mean), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_viridis_c(option = "D") + 
  theme + labs(title="Equilibrium (only t2) mean", fill="")
ggsave(plot = p3, file = "USBBS_output/modelling_output/debts_credits_maps/equilibrium_map_mean.png", units = "cm", dpi = "retina", width =33, height = 19)

p4 <- ggplot() + 
  geom_sf(data = usa_landscape, aes(fill=eq_cv), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_viridis_c(option = "C") + 
  theme + labs(title="Equilibrium (only t2) cv", fill="")
ggsave(plot = p4, file = "USBBS_output/modelling_output/debts_credits_maps/equilibrium_map_cv.png", units = "cm", dpi = "retina", width =33, height = 19)

p <- cowplot::plot_grid(p3, p4, labels = c('A', 'B'), nrow=2)
ggsave(plot = p, file = "USBBS_output/modelling_output/debts_credits_maps/equilibrium_map_PANEL.png", units = "cm", dpi = "retina", width =15, height = 19)

#'--------------------------------------------------------------------------------------------------
#' * Delayed effective number of species *

p5 <- ggplot() + 
  geom_sf(data = usa_landscape, aes(fill=delay_mean), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_viridis_c(option = "D") + 
  theme + labs(title="Delayed (full model) mean", fill="")
ggsave(plot = p5, file = "USBBS_output/modelling_output/debts_credits_maps/delayed_map_mean.png", units = "cm", dpi = "retina", width =33, height = 19)

p6 <- ggplot() + 
  geom_sf(data = usa_landscape, aes(fill=delay_cv), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_viridis_c(option = "C") + 
  theme + labs(title="Delayed (full model) cv", fill="")
ggsave(plot = p6, file = "USBBS_output/modelling_output/debts_credits_maps/delayed_map_cv.png", units = "cm", dpi = "retina", width =33, height = 19)

p <- cowplot::plot_grid(p5, p6, labels = c('A', 'B'), nrow=2)
ggsave(plot = p, file = "USBBS_output/modelling_output/debts_credits_maps/delayed_map_PANEL.png", units = "cm", dpi = "retina", width =15, height = 19)

