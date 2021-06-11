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

# using a 1 effective species cutoff
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

#'--------------------------------------------------------------------------------------------------
#'* Maps for debts and credits values *
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

usa_landscape$type <- ""
usa_landscape$type[usa_landscape$debtcredit_mean <= q[2]] <- "debit (< 25% quantile)"
usa_landscape$type[usa_landscape$debtcredit_mean <= q[1]] <- "debit (< 10% quantile)"
usa_landscape$type[usa_landscape$debtcredit_mean >= q[4]] <- "credit (> 75% quantile)"
usa_landscape$type[usa_landscape$debtcredit_mean >= q[5]] <- "credit (> 90% quantile)"
usa_landscape$type[usa_landscape$debtcredit_mean > q[2] & usa_landscape$debtcredit_mean < q[4] ] <- "    "
usa_landscape$type <- factor(usa_landscape$type, levels = c("debit (< 10% quantile)", "debit (< 25% quantile)", "    ", 
                                                            "credit (> 75% quantile)", "credit (> 90% quantile)"))
p2 <- ggplot() + theme +
  geom_sf(data = usa_landscape, aes(fill=type), color=NA) +
  geom_sf(data=usa_borders, color="grey50", size=0.5, alpha=0.5) +
  theme + labs(title = "", fill="") +
  scale_fill_manual(values =c("darkred", "coral", "white", "cornflowerblue", "navy")) + 
  theme(legend.position="bottom")
ggsave(plot = p2, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_coloring.png",units = "cm", dpi = "retina", width =27, height = 19)

p <- cowplot::ggdraw(p2) + cowplot::draw_plot(pie_chart, x = 0.012, y = 0.115, width = 0.26, height = 0.26, scale=1) 
ggsave(plot = p, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_piechart.png",units = "cm", dpi = "retina", width =27, height = 19)

######################

p2.cv <- ggplot() + 
        geom_sf(data = usa_landscape, aes(fill=log(debtcredit_cv+1)), color=NA) +
        geom_sf(data=usa_borders, color="black") +
        scale_fill_viridis_c(option = "E") + 
        theme + labs(title="Debts & Credits cv", fill="")
ggsave(plot = p2.cv, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_cv.png", units = "cm", dpi = "retina", width =33, height = 19)

p2.sd <- ggplot() + 
  geom_sf(data = usa_landscape, aes(fill=debtcredit_sd), color=NA) +
  geom_sf(data=usa_borders, color="black") +
  scale_fill_viridis_c(option = "E") + 
  theme + labs(title="Debts & Credits sd", fill="")
ggsave(plot = p2.sd, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_sd.png", units = "cm", dpi = "retina", width =33, height = 19)

p <- cowplot::plot_grid(p2, p2.sd, labels = c('A', 'B'), nrow=2)
ggsave(plot = p, file = "USBBS_output/modelling_output/debts_credits_maps/debt_credit_map_PANEL.png", units = "cm", dpi = "retina", width =15, height = 19)

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

