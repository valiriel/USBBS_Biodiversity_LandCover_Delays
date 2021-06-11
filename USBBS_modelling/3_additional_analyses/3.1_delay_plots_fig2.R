library(tidyverse); library(ggthemes); library(cmdstanr); library(ggplot2); library(ggpubr); library(gridExtra)
theme <- theme_clean() + 
  theme(axis.title=element_text(size=14), axis.text=element_text(size=11),
        legend.text=element_text(size=9), legend.title=element_text(size=11), 
        plot.background = element_rect(color = "white"), 
        panel.border= element_blank(), plot.title=element_text(size=14),
        panel.grid.major.y = element_line(size=0.2, linetype = "dotted", colour = "grey90"),  
        panel.grid.major.x = element_line(size=0.2, linetype = "dotted", colour = "grey90"),
        legend.background = element_rect(color = NA),
        strip.background.y = element_rect(color = "grey30"))

#'-----------------------------------------------------------------------------------------------------
#' * Plot delay value as a function of land cover change magnitudes*
#'
#' * FIGURE 2 of the manuscript *
#'

# load the model in
f <- "mean"; buffer.size <- "500"; buffer.type <- "segment"; file <- 3
result <- readRDS(paste0("USBBS_modelling/1_model_fitting/stan_model_fitted/sampled_", f, "_", buffer.type,"_",buffer.size,".RDS"))
load(paste0("USBBS_data/model_ready_data/", file, "years_x_timepoint/data_", f, "_", buffer.type,"_",buffer.size,".rda"))

# extract draws for delay params
params <- c("c_pos_urban", "c_pos_forest", "c_pos_grass", "c_pos_crop", "c_pos_wet",        
                           "c_neg_forest", "c_neg_grass", "c_neg_crop", "c_neg_wet")

# 4000 total draws from 4 chains with 1000_iter + 500_warmup each
draws <- result$draws(params, inc_warmup=F, format="draws_df")

# extract length of posterior sample
sample.length <- as.numeric(nrow(draws))

# set values of land cover change for plotting and equation
delta_values <- seq(0, 50, by=0.1)
lenght.delta <- as.numeric(length(delta_values))

# number of times to sample posterior distribution
n.samples <- 10000

# setup function to calculate w value at each instance of delta land cover change
# marginal on everything else being = to 0*
w_eq <- function(c = 0, delta_values = 0) (1 - exp(-c*delta_values))

#'-----------------------------------------------------------------------------------------------------
#' * Sample posterior and predict lags across values*
#'

for(lag in params) {
  
  # initialize storage for each sample parameter estimate
  w.run <- matrix(data=0, nrow=n.samples, ncol=lenght.delta)
  
  for (i in 1:n.samples) {
    
    # generate random number within number of distribution values
    pos.sample <- round(runif(n=1, max=sample.length, min = 1))
    
    # sample a value from the parameter posterior distro
    lag.estimate <- as.numeric(draws[pos.sample,lag])
    
    w.run[i,] <- w_eq(c = lag.estimate, delta_values = delta_values)
  }

  # Obtain confidence intervals and mean of the multiple runs for each delta
  
  #initialize summary 
  w.summary <- data.frame("delta_values"=delta_values, "average"=1:lenght.delta, "upperCI"=1:lenght.delta, "lowerCI"=1:lenght.delta)

  # average 
  w.summary$average <- apply(w.run, 2, mean)
  
  for(i in 1:nrow(w.summary)){
    
    w.now <- w.run[,i]
    
    w.quantiles <- quantile(w.now, c(0.025, 0.975))
    w.summary$upperCI[i] <-  w.quantiles[2]
    w.summary$lowerCI[i] <-  w.quantiles[1]
    
  }
  
  w.summary$type <- substr(lag, 7, nchar(lag))
  assign(paste0("df_",lag), w.summary) 
  
}

# delta values to negative for negative changes 
df_c_neg_forest$delta_values <- -delta_values
df_c_neg_crop$delta_values <- -delta_values
df_c_neg_grass$delta_values <- -delta_values
df_c_neg_wet$delta_values <- -delta_values

#'-----------------------------------------------------------------------------------------------------
#' * Generate plots*
#'
#'-----------------------------------------------------------------------------------------------------
#'   *Urban delay*
#' 

for (i in 1:length(df_c_pos_urban$delta_values)) {
  if(df_c_pos_urban$delta_values[i] < round(max(data.list$delta_pos_urban)))
    df_c_pos_urban$valid[i] <- "valid"
  else df_c_pos_urban$valid[i] <- "invalid"
}

w.urban <- ggplot(df_c_pos_urban, aes(x=delta_values)) +  
  geom_line(data=df_c_pos_urban[df_c_pos_urban$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="gray38", alpha=1) +
  geom_ribbon(data=df_c_pos_urban[df_c_pos_urban$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.5) +
  geom_line(data=df_c_pos_urban[df_c_pos_urban$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="gray38",alpha=0.4) +
  geom_ribbon(data=df_c_pos_urban[df_c_pos_urban$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.2) +
  #geom_line(data=df_c_neg_urban[df_c_neg_urban$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="gray38", alpha=1) +
  #geom_ribbon(data=df_c_neg_urban[df_c_neg_urban$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.5) +
  #geom_line(data=df_c_neg_urban[df_c_neg_urban$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="gray38",alpha=0.4) +
  #geom_ribbon(data=df_c_neg_urban[df_c_neg_urban$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.2) +
  theme +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-50, 50), breaks = seq(from=-50, to=50, by=10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) + 
  labs (x = "Urban change", y = "Proportional contribution of past landscape")

ggsave(plot = w.urban, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltaurban.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = w.urban, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltaurban.png",
       units = "cm", dpi = "retina", width =29.7, height = 21)

#'-----------------------------------------------------------------------------------------------------
#'   *Forest delay*
#' 

for (i in 1:length(df_c_pos_forest$delta_values)) {
  if(df_c_pos_forest$delta_values[i] < round(max(data.list$delta_pos_forest)))
    df_c_pos_forest$valid[i] <- "valid"
  else df_c_pos_forest$valid[i] <- "invalid"
  
  if(df_c_neg_forest$delta_values[i] > -round(max(data.list$delta_neg_forest)))
    df_c_neg_forest$valid[i] <- "valid"
  else df_c_neg_forest$valid[i] <- "invalid"
}

colorcode <- "darkgreen"

w.forest <- ggplot(df_c_pos_forest, aes(x=delta_values)) +  
  geom_line(data=df_c_pos_forest[df_c_pos_forest$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour=colorcode, alpha=1) +
  geom_ribbon(data=df_c_pos_forest[df_c_pos_forest$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill=colorcode, alpha=0.5) +
  geom_line(data=df_c_pos_forest[df_c_pos_forest$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour=colorcode,alpha=0.4) +
  geom_ribbon(data=df_c_pos_forest[df_c_pos_forest$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill=colorcode, alpha=0.2) +
  geom_line(data=df_c_neg_forest[df_c_neg_forest$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour=colorcode, alpha=1) +
  geom_ribbon(data=df_c_neg_forest[df_c_neg_forest$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill=colorcode, alpha=0.5) +
  geom_line(data=df_c_neg_forest[df_c_neg_forest$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour=colorcode,alpha=0.4) +
  geom_ribbon(data=df_c_neg_forest[df_c_neg_forest$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill=colorcode, alpha=0.2) +
  theme +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-50, 50), breaks = seq(from=-50, to=50, by=10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) + 
  labs (x = "Forest change", y = "Proportional contribution of past landscape")

ggsave(plot = w.forest, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltaforest.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = w.forest, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltaforest.png",
       units = "cm", dpi = "retina", width =29.7, height = 21)

#'-----------------------------------------------------------------------------------------------------
#'   *Cropland delay*
#' 

for (i in 1:length(df_c_pos_crop$delta_values)) {
  if(df_c_pos_crop$delta_values[i] < round(max(data.list$delta_pos_crop)))
    df_c_pos_crop$valid[i] <- "valid"
  else df_c_pos_crop$valid[i] <- "invalid"
  
  if(df_c_neg_crop$delta_values[i] > -round(max(data.list$delta_neg_crop)))
    df_c_neg_crop$valid[i] <- "valid"
  else df_c_neg_crop$valid[i] <- "invalid"
}

w.cropland <- ggplot(df_c_pos_crop, aes(x=delta_values)) +  
  geom_line(data=df_c_pos_crop[df_c_pos_crop$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgoldenrod2", alpha=1) +
  geom_ribbon(data=df_c_pos_crop[df_c_pos_crop$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.5) +
  geom_line(data=df_c_pos_crop[df_c_pos_crop$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgoldenrod2",alpha=0.4) +
  geom_ribbon(data=df_c_pos_crop[df_c_pos_crop$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.2) +
  geom_line(data=df_c_neg_crop[df_c_neg_crop$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgoldenrod2", alpha=1) +
  geom_ribbon(data=df_c_neg_crop[df_c_neg_crop$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.5) +
  geom_line(data=df_c_neg_crop[df_c_neg_crop$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgoldenrod2",alpha=0.4) +
  geom_ribbon(data=df_c_neg_crop[df_c_neg_crop$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.2) +
  theme +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-50, 50), breaks = seq(from=-50, to=50, by=10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) + 
  labs (x = "Cropland change", y = "Proportional contribution of past landscape")

ggsave(plot = w.cropland, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltacrop.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)  
ggsave(plot = w.cropland, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltacrop.png",
       units = "cm", dpi = "retina", width =29.7, height = 21) 

#'-----------------------------------------------------------------------------------------------------
#' **Grassland delay**
#'

for (i in 1:length(df_c_pos_grass$delta_values)) {
  if(df_c_pos_grass$delta_values[i] < round(max(data.list$delta_pos_grass)))
    df_c_pos_grass$valid[i] <- "valid"
  else df_c_pos_grass$valid[i] <- "invalid"
  
  if(df_c_neg_grass$delta_values[i] > -round(max(data.list$delta_pos_grass)))
    df_c_neg_grass$valid[i] <- "valid"
  else df_c_neg_grass$valid[i] <- "invalid"
}

w.grassland <- ggplot(df_c_pos_grass, aes(x=delta_values)) +  
  geom_line(data=df_c_pos_grass[df_c_pos_grass$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="limegreen", alpha=1) +
  geom_ribbon(data=df_c_pos_grass[df_c_pos_grass$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.5) +
  geom_line(data=df_c_pos_grass[df_c_pos_grass$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="limegreen",alpha=0.4) +
  geom_ribbon(data=df_c_pos_grass[df_c_pos_grass$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.2) +
  geom_line(data=df_c_neg_grass[df_c_neg_grass$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="limegreen", alpha=1) +
  geom_ribbon(data=df_c_neg_grass[df_c_neg_grass$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.5) +
  geom_line(data=df_c_neg_grass[df_c_neg_grass$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="limegreen",alpha=0.4) +
  geom_ribbon(data=df_c_neg_grass[df_c_neg_grass$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.2) +
  theme +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-50, 50), breaks = seq(from=-50, to=50, by=10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) + 
  labs (x = "Grassland change", y = "Proportional contribution of past landscape")

ggsave(plot = w.grassland, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltagrass.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = w.grassland, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltagrass.png",
       units = "cm", dpi = "retina", width =29.7, height = 21)

#'-----------------------------------------------------------------------------------------------------
#' *Wetland delay*
#'

for (i in 1:length(df_c_pos_wet$delta_values)) {
  if(df_c_pos_wet$delta_values[i] < round(max(data.list$delta_pos_wet)))
    df_c_pos_wet$valid[i] <- "valid"
  else df_c_pos_wet$valid[i] <- "invalid"
  
  if(df_c_neg_wet$delta_values[i] > -round(max(data.list$delta_neg_wet)))
    df_c_neg_wet$valid[i] <- "valid"
  else df_c_neg_wet$valid[i] <- "invalid"
}

w.wetland <- ggplot(df_c_pos_wet, aes(x=delta_values)) +  
  geom_line(data=df_c_pos_wet[df_c_pos_wet$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="cornflowerblue", alpha=1) +
  geom_ribbon(data=df_c_pos_wet[df_c_pos_wet$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.5) +
  geom_line(data=df_c_pos_wet[df_c_pos_wet$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="cornflowerblue",alpha=0.4) +
  geom_ribbon(data=df_c_pos_wet[df_c_pos_wet$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.2) +
  geom_line(data=df_c_neg_wet[df_c_neg_wet$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="cornflowerblue", alpha=1) +
  geom_ribbon(data=df_c_neg_wet[df_c_neg_wet$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.5) +
  geom_line(data=df_c_neg_wet[df_c_neg_wet$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="cornflowerblue",alpha=0.4) +
  geom_ribbon(data=df_c_neg_wet[df_c_neg_wet$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.2) +
  theme +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-50, 50), breaks = seq(from=-50, to=50, by=10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) + 
  labs (x = "Wetland change", y = "Proportional contribution of past landscape")

ggsave(plot = w.wetland, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltawet.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = w.wetland, file = "USBBS_output/modelling_output/w~deltavars_plots/w~deltawet.png",
       units = "cm", dpi = "retina", width =29.7, height = 21)

#################################################################################
#'
#' **Delay panel**
#'

tabledata <- tibble("Landcover change"=c("Urban", "Forest", "Wetland", "Grassland", "Cropland"), 
                    "- 10\U0025"=c("------", 
                                   as.numeric(round(df_c_neg_forest[df_c_neg_forest$delta_values==-10,][2],3)),
                                   as.numeric(round(df_c_neg_wet[df_c_neg_wet$delta_values==-10,][2],3)),
                                   as.numeric(round(df_c_neg_grass[df_c_neg_grass$delta_values==-10,][2],3)),
                                   as.numeric(round(df_c_neg_crop[df_c_neg_crop$delta_values==-10,][2],3))),
                    "+ 10\U0025"=c(as.numeric(round(df_c_pos_urban[df_c_pos_urban$delta_values==10,][2],3)),
                                   as.numeric(round(df_c_pos_forest[df_c_pos_forest$delta_values==10,][2],3)),
                                   as.numeric(round(df_c_pos_wet[df_c_pos_wet$delta_values==10,][2],3)),
                                   as.numeric(round(df_c_pos_grass[df_c_pos_grass$delta_values==10,][2],3)), 
                                   as.numeric(round(df_c_pos_crop[df_c_pos_crop$delta_values==10,][2],3))))
table <- gridExtra::tableGrob(tabledata, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 1.3, hjust=0, x=0.15)),
                                                                            colhead = list(fg_params=list(cex = 1.3)),
                                                                            rowhead = list(fg_params=list(cex = 1.0))))

w.panel <- ggarrange(w.urban + rremove("grid") + rremove("y.title"), #+ rremove("x.text") + rremove("x.ticks"), 
                     w.forest + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"), #+ rremove("x.text") + rremove("x.ticks"), 
                     w.wetland + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"),#+ rremove("x.text") + rremove("x.ticks"),
                     w.grassland + rremove("grid") + rremove("y.title"), #+ rremove("x.text") + rremove("x.ticks"), 
                     w.cropland + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"),# + rremove("x.text") + rremove("x.ticks"), 
                     table,#lc_change + rremove("x.text") + rremove("x.ticks"),
                     #w.temp + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"),
                     labels = c("A", "B", "C", "D", "E", "F"),
                     label.x = 0.91, label.y = 0.975,
                     font.label = list(size = 18, face = "bold", color ="black"),
                     ncol = 3, nrow = 2,
                     widths = c(1,0.9,0.9), heights = c(1,1))
annotate_figure(w.panel,left = text_grob("Proportional contribution of past landscape", 
                                         size = 20, color = "black", rot = 90))

ggsave(file = "USBBS_output/modelling_output/main_figures/fig2_w~deltas.svg",
       units = "cm", dpi = "retina", width =29.7 , height = 21)
ggsave(file = "USBBS_output/modelling_output/main_figures/fig2_w~deltas.png",
       units = "cm", dpi = "retina", width =29.7 , height = 21)

#################################################################################
#'
#' * half saturation *
#'

df_c_pos_urban[df_c_pos_urban$average<=0.5,]

df_c_pos_forest[df_c_pos_forest$average<=0.5,]
df_c_neg_forest[df_c_neg_forest$average<=0.5,]

df_c_pos_grass[df_c_pos_grass$average<=0.5,]
df_c_neg_grass[df_c_neg_grass$average<=0.5,]

df_c_pos_crop[df_c_pos_crop$average<=0.5,]
df_c_neg_crop[df_c_neg_crop$average<=0.5,]

df_c_pos_wet[df_c_pos_wet$average<=0.5,]
df_c_neg_wet[df_c_neg_wet$average<=0.5,]

