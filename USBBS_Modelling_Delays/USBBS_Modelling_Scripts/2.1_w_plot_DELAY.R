library(coda)
library(tidyverse)
library(Rmisc)
library(ggpubr)

#'################################################################################
#'
#' *w function against different magnitudes of change in land covers* 
#'
#'################################################################################

load("data.centroid.springtemp.rda")

load("USBBS_Modelling_Delays/USBBS_models_jags/results.q0.centroid.springtemp.rda")
summary.m.top <- as_tibble(results.q0.top$BUGSoutput$summary, rownames=NA) %>% round(5) %>% rownames_to_column %>% slice(-(50):-(n()-1)) 
summary.m.top

results <- results.q0.centroidspringtemp

##################################################################################

# extract mcmc of each parameter
mcmc.final <- mcmc(results$BUGSoutput$sims.list)

# retain mcmc of delay parameter, c
c.values <- data.frame("c.pos.urban" = mcmc.final$c.pos.urban, "c.pos.forest" = mcmc.final$c.pos.forest, 
                       "c.pos.grassland" = mcmc.final$c.pos.grassland, "c.pos.cropland" = mcmc.final$c.pos.cropland,
                       "c.pos.wetland"=mcmc.final$c.pos.wetland,  "c.pos.temp" = mcmc.final$c.pos.temp,
                       "c.neg.forest" = mcmc.final$c.neg.forest, "c.neg.temp" = mcmc.final$c.neg.temp, 
                       "c.neg.cropland" = mcmc.final$c.neg.cropland, "c.neg.grassland" = mcmc.final$c.neg.grassland, "c.neg.wetland"=mcmc.final$c.neg.wetland)
                      # notice no negative urban, as no data available

# extract length of posterior sample
sample.length <- nrow(c.values)

# set values of land cover change for plotting and equation
delta.values <- seq(0, 100, by=0.1)
temp.values <- seq(0, 5, by=0.005)

lenght.delta <- as.numeric(length(delta.values))

# number of times to sample posterior distribution
n.samples <- 10000

# setup function to calculate w value at each instance of delta land cover change
#' *marginal on everything else being = to 0*
w_eq <- function(c = 0, delta.values = 0) (1 - exp(-c*delta.values))

lags <- c("c.pos.urban", "c.pos.forest", "c.pos.grassland", "c.pos.cropland", "c.pos.wetland", "c.pos.temp", "c.neg.temp",
            "c.neg.forest", "c.neg.grassland", "c.neg.cropland", "c.neg.wetland")

for(lag in lags) {
  
# initialize storage for each sample parameter estimate
w.run <- matrix(data=0, nrow=n.samples, ncol=lenght.delta)

for (i in 1:n.samples) {
  
  # generate random number within number of distribution values
  pos.sample <- round(runif(n=1, max=sample.length, min = 1))
  
  # sample a value form the parameter posterior distro
  lag.estimate <- c.values[pos.sample,lag]
  
  w.run[i,] <- w_eq(c = lag.estimate, delta.values = delta.values)

  if(lag=="c.pos.temp") {w.run[i,] <- w_eq(c = lag.estimate, delta.values = temp.values)}
  if(lag=="c.neg.temp") {w.run[i,] <- w_eq(c = lag.estimate, delta.values = temp.values)}
  
}

# Obtain confidence intervals and mean of the multiple runs for each delta

#initialize summary 
w.summary <- data.frame("delta.values"=delta.values, "average"=1:lenght.delta, "upperCI"=1:lenght.delta, "lowerCI"=1:lenght.delta)

if(lag=="c.pos.temp") w.summary <- data.frame("delta.values"=temp.values, "average"=1:lenght.delta, "upperCI"=1:lenght.delta, "lowerCI"=1:lenght.delta)
if(lag=="c.neg.temp") w.summary <- data.frame("delta.values"=temp.values, "average"=1:lenght.delta, "upperCI"=1:lenght.delta, "lowerCI"=1:lenght.delta)

# average 
w.summary$average <- apply(w.run, 2, mean)

for(i in 1:nrow(w.summary)){
  
  w.now <- w.run[,i]
  
  w.quantiles <- quantile(w.now, c(0.025, 0.975))
  w.summary$upperCI[i] <-  w.quantiles[2]
  w.summary$lowerCI[i] <-  w.quantiles[1]
  
}

assign(paste0("df.",lag), w.summary) 
       
}

#' *delta values to negative for negative change*

df.c.neg.forest$delta.values <- -df.c.neg.forest$delta.values
df.c.neg.cropland$delta.values <- -df.c.neg.cropland$delta.values
df.c.neg.grassland$delta.values <- -df.c.neg.grassland$delta.values
df.c.neg.wetland$delta.values <- -df.c.neg.wetland$delta.values
df.c.neg.temp$delta.values <- -df.c.neg.temp$delta.values

####################################################################################################################
#'
#'   *Urban delay*
#' 

for (i in 1:length(df.c.pos.urban$delta.values)) {
  if(df.c.pos.urban$delta.values[i] < round(max(data$delta.urban)))
    df.c.pos.urban$valid[i] <- "valid"
  else df.c.pos.urban$valid[i] <- "invalid"
}

w.urban <- ggplot(df.c.pos.urban, aes(x=delta.values)) +  
  geom_line(data=df.c.pos.urban[df.c.pos.urban$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="gray38", alpha=1) +
  geom_ribbon(data=df.c.pos.urban[df.c.pos.urban$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.5) +
    geom_line(data=df.c.pos.urban[df.c.pos.urban$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="gray38",alpha=0.4) +
    geom_ribbon(data=df.c.pos.urban[df.c.pos.urban$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.2) +
  #geom_line(data=df.c.neg.urban[df.c.neg.urban$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="gray38", alpha=1) +
  #geom_ribbon(data=df.c.neg.urban[df.c.neg.urban$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.5) +
    #geom_line(data=df.c.neg.urban[df.c.neg.urban$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="gray38",alpha=0.4) +
    #geom_ribbon(data=df.c.neg.urban[df.c.neg.urban$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.2) +
  theme_bw(base_size = 15) + 
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-100, 100), breaks = seq(from=-100, to=100, by=25), labels= c("-100","-75","-50","-25","0","+25","+50","+75","+100")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1000, by = 0.25)) + 
  labs (x = "Urban", y = "Proportional contribution of past timepoint")

ggsave(plot = w.urban, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltaurbana.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = w.urban, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltaurbana.pdf",
       units = "cm", dpi = "retina", width =29.7, height = 21)

################################################
#'
#'   *Forest delay*
#' 

for (i in 1:length(df.c.pos.forest$delta.values)) {
  if(df.c.pos.forest$delta.values[i] < round(max(data$delta.forest)))
    df.c.pos.forest$valid[i] <- "valid"
  else df.c.pos.forest$valid[i] <- "invalid"
  
  if(df.c.neg.forest$delta.values[i] > round(min(data$delta.forest)))
    df.c.neg.forest$valid[i] <- "valid"
  else df.c.neg.forest$valid[i] <- "invalid"
}

w.forest <- ggplot(df.c.pos.forest, aes(x=delta.values)) +  
  geom_line(data=df.c.pos.forest[df.c.pos.forest$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgreen", alpha=1) +
  geom_ribbon(data=df.c.pos.forest[df.c.pos.forest$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgreen", alpha=0.5) +
    geom_line(data=df.c.pos.forest[df.c.pos.forest$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgreen",alpha=0.4) +
    geom_ribbon(data=df.c.pos.forest[df.c.pos.forest$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgreen", alpha=0.2) +
  geom_line(data=df.c.neg.forest[df.c.neg.forest$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgreen", alpha=1) +
  geom_ribbon(data=df.c.neg.forest[df.c.neg.forest$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgreen", alpha=0.5) +
    geom_line(data=df.c.neg.forest[df.c.neg.forest$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgreen",alpha=0.4) +
    geom_ribbon(data=df.c.neg.forest[df.c.neg.forest$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgreen", alpha=0.2) +
  theme_bw(base_size = 15) + 
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-100, 100), breaks = seq(from=-100, to=100, by=25), labels= c("-100","-75","-50","-25","0","+25","+50","+75","+100")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1000, by = 0.25)) + 
  labs (x = "Forest", y = "Proportional contribution of past timepoint")

ggsave(plot = w.forest, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltaforesta.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = w.forest, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltaforesta.pdf",
       units = "cm", dpi = "retina", width =29.7, height = 21)


################################################
#'
#'   *Cropland delay*
#' 

for (i in 1:length(df.c.pos.cropland$delta.values)) {
  if(df.c.pos.cropland$delta.values[i] < round(max(data$delta.cropland)))
    df.c.pos.cropland$valid[i] <- "valid"
  else df.c.pos.cropland$valid[i] <- "invalid"
  
  if(df.c.neg.cropland$delta.values[i] > round(min(data$delta.cropland)))
    df.c.neg.cropland$valid[i] <- "valid"
  else df.c.neg.cropland$valid[i] <- "invalid"
}

w.cropland <- ggplot(df.c.pos.cropland, aes(x=delta.values)) +  
  geom_line(data=df.c.pos.cropland[df.c.pos.cropland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgoldenrod2", alpha=1) +
  geom_ribbon(data=df.c.pos.cropland[df.c.pos.cropland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.5) +
    geom_line(data=df.c.pos.cropland[df.c.pos.cropland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgoldenrod2",alpha=0.4) +
    geom_ribbon(data=df.c.pos.cropland[df.c.pos.cropland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.2) +
  geom_line(data=df.c.neg.cropland[df.c.neg.cropland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgoldenrod2", alpha=1) +
  geom_ribbon(data=df.c.neg.cropland[df.c.neg.cropland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.5) +
    geom_line(data=df.c.neg.cropland[df.c.neg.cropland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkgoldenrod2",alpha=0.4) +
    geom_ribbon(data=df.c.neg.cropland[df.c.neg.cropland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.2) +
  theme_bw(base_size = 15) + 
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-100, 100), breaks = seq(from=-100, to=100, by=25), labels= c("-100","-75","-50","-25","0","+25","+50","+75","+100")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1000, by = 0.25)) + 
  labs (x = "Cropland", y = "Proportional contribution of past timepoint")

ggsave(plot = w.cropland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltacroplanda.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)  
ggsave(plot = w.cropland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltacroplanda.pdf",
       units = "cm", dpi = "retina", width =29.7, height = 21) 

#################################################################################
#'
#' **Grassland delay**
#'

for (i in 1:length(df.c.pos.grassland$delta.values)) {
  if(df.c.pos.grassland$delta.values[i] < round(max(data$delta.grassland)))
    df.c.pos.grassland$valid[i] <- "valid"
  else df.c.pos.grassland$valid[i] <- "invalid"
  
  if(df.c.neg.grassland$delta.values[i] > round(min(data$delta.grassland)))
    df.c.neg.grassland$valid[i] <- "valid"
  else df.c.neg.grassland$valid[i] <- "invalid"
}

w.grassland <- ggplot(df.c.pos.grassland, aes(x=delta.values)) +  
  geom_line(data=df.c.pos.grassland[df.c.pos.grassland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="limegreen", alpha=1) +
  geom_ribbon(data=df.c.pos.grassland[df.c.pos.grassland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.5) +
    geom_line(data=df.c.pos.grassland[df.c.pos.grassland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="limegreen",alpha=0.4) +
    geom_ribbon(data=df.c.pos.grassland[df.c.pos.grassland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.2) +
  geom_line(data=df.c.neg.grassland[df.c.neg.grassland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="limegreen", alpha=1) +
  geom_ribbon(data=df.c.neg.grassland[df.c.neg.grassland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.5) +
    geom_line(data=df.c.neg.grassland[df.c.neg.grassland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="limegreen",alpha=0.4) +
    geom_ribbon(data=df.c.neg.grassland[df.c.neg.grassland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.2) +
  theme_bw(base_size = 15) + 
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-100, 100), breaks = seq(from=-100, to=100, by=25), labels= c("-100","-75","-50","-25","0","+25","+50","+75","+100")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1000, by = 0.25)) + 
  labs (x = "Grassland", y = "Proportional contribution of past timepoint")

ggsave(plot = w.grassland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltagrasslanda.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = w.grassland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltagrasslanda.pdf",
       units = "cm", dpi = "retina", width =29.7, height = 21)

#################################################################################
#'
#' **Wetland delay**
#'

for (i in 1:length(df.c.pos.wetland$delta.values)) {
  if(df.c.pos.wetland$delta.values[i] < round(max(data$delta.wetland)))
    df.c.pos.wetland$valid[i] <- "valid"
  else df.c.pos.wetland$valid[i] <- "invalid"
  
  if(df.c.neg.wetland$delta.values[i] > round(min(data$delta.wetland)))
    df.c.neg.wetland$valid[i] <- "valid"
  else df.c.neg.wetland$valid[i] <- "invalid"
}

w.wetland <- ggplot(df.c.pos.wetland, aes(x=delta.values)) +  
  geom_line(data=df.c.pos.wetland[df.c.pos.wetland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="cornflowerblue", alpha=1) +
  geom_ribbon(data=df.c.pos.wetland[df.c.pos.wetland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.5) +
  geom_line(data=df.c.pos.wetland[df.c.pos.wetland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="cornflowerblue",alpha=0.4) +
  geom_ribbon(data=df.c.pos.wetland[df.c.pos.wetland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.2) +
  geom_line(data=df.c.neg.wetland[df.c.neg.wetland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="cornflowerblue", alpha=1) +
  geom_ribbon(data=df.c.neg.wetland[df.c.neg.wetland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.5) +
  geom_line(data=df.c.neg.wetland[df.c.neg.wetland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="cornflowerblue",alpha=0.4) +
  geom_ribbon(data=df.c.neg.wetland[df.c.neg.wetland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.2) +
  theme_bw(base_size = 15) + 
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-100, 100), breaks = seq(from=-100, to=100, by=25), labels= c("-100","-75","-50","-25","0","+25","+50","+75","+100")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1000, by = 0.25)) + 
  labs (x = "Wetland", y = "Proportional contribution of past timepoint")

ggsave(plot = w.wetland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltawetlanda.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = w.wetland, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltawetlanda.pdf",
       units = "cm", dpi = "retina", width =29.7, height = 21)

#################################################################################
#'
#' **Temperature delay**
#'

for (i in 1:length(df.c.pos.temp$delta.values)) {
  if(df.c.pos.temp$delta.values[i] < max(data$delta.temp))
    df.c.pos.temp$valid[i] <- "valid"
  else df.c.pos.temp$valid[i] <- "invalid"
  
  if(df.c.neg.temp$delta.values[i] > min(data$delta.temp))
    df.c.neg.temp$valid[i] <- "valid"
  else df.c.neg.temp$valid[i] <- "invalid"
}

w.temp <- ggplot(df.c.pos.temp, aes(x=delta.values)) +  
  geom_line(data=df.c.pos.temp[df.c.pos.temp$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkred", alpha=1) +
  geom_ribbon(data=df.c.pos.temp[df.c.pos.temp$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkred", alpha=0.5) +
    geom_line(data=df.c.pos.temp[df.c.pos.temp$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkred",alpha=0.4) +
    geom_ribbon(data=df.c.pos.temp[df.c.pos.temp$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkred", alpha=0.2) +
  geom_line(data=df.c.neg.temp[df.c.neg.temp$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkred", alpha=1) +
  geom_ribbon(data=df.c.neg.temp[df.c.neg.temp$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkred", alpha=0.5) +
    geom_line(data=df.c.neg.temp[df.c.neg.temp$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 1, colour="darkred",alpha=0.4) +
    geom_ribbon(data=df.c.neg.temp[df.c.neg.temp$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkred", alpha=0.2) +
  theme_bw(base_size = 15) + 
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(-4, 4), breaks = seq(from=-4, to=4, by=2), labels= c("-4","-2","0","+2","+4")) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1000, by = 0.25)) + 
  labs (x = "Temperature (°C)", y = "Proportional contribution of past timepoint")

ggsave(plot = w.temp, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltatemp.svg",
       units = "cm", dpi = "retina", width =29.7, height = 21)
ggsave(plot = w.temp, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~deltatemp.pdf",
       units = "cm", dpi = "retina", width =29.7, height = 21)

####################################################################################################################
#'
#'   *All delays together*
#'  

delta.values <- tibble(delta.values=delta.values)

w.all <- ggplot(data=delta.values, aes(x=delta.values)) +  
          geom_line(data=df.c.pos.wetland[df.c.pos.wetland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="cornflowerblue", alpha=1) +
                  #geom_ribbon(data=df.c.pos.wetland[df.c.pos.wetland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.5) +
                  geom_line(data=df.c.pos.wetland[df.c.pos.wetland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="cornflowerblue",alpha=0.4) +
                  #geom_ribbon(data=df.c.pos.wetland[df.c.pos.wetland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.2) +
                  geom_line(data=df.c.neg.wetland[df.c.neg.wetland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="cornflowerblue", alpha=1) +
                  #geom_ribbon(data=df.c.neg.wetland[df.c.neg.wetland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.5) +
                  geom_line(data=df.c.neg.wetland[df.c.neg.wetland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="cornflowerblue",alpha=0.4) +
                  #geom_ribbon(data=df.c.neg.wetland[df.c.neg.wetland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="cornflowerblue", alpha=0.2) +
          geom_line(data=df.c.pos.grassland[df.c.pos.grassland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="limegreen", alpha=1) +
                  #geom_ribbon(data=df.c.pos.grassland[df.c.pos.grassland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.5) +
                  geom_line(data=df.c.pos.grassland[df.c.pos.grassland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="limegreen",alpha=0.4) +
                  #geom_ribbon(data=df.c.pos.grassland[df.c.pos.grassland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.2) +
                  geom_line(data=df.c.neg.grassland[df.c.neg.grassland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="limegreen", alpha=1) +
                  #geom_ribbon(data=df.c.neg.grassland[df.c.neg.grassland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.5) +
                  geom_line(data=df.c.neg.grassland[df.c.neg.grassland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="limegreen",alpha=0.4) +
                  #geom_ribbon(data=df.c.neg.grassland[df.c.neg.grassland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="limegreen", alpha=0.2) +
          geom_line(data=df.c.pos.cropland[df.c.pos.cropland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="darkgoldenrod2", alpha=1) +
                  #geom_ribbon(data=df.c.pos.cropland[df.c.pos.cropland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.5) +
                  geom_line(data=df.c.pos.cropland[df.c.pos.cropland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="darkgoldenrod2",alpha=0.4) +
                  #geom_ribbon(data=df.c.pos.cropland[df.c.pos.cropland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.2) +
                  geom_line(data=df.c.neg.cropland[df.c.neg.cropland$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="darkgoldenrod2", alpha=1) +
                  #geom_ribbon(data=df.c.neg.cropland[df.c.neg.cropland$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.5) +
                  geom_line(data=df.c.neg.cropland[df.c.neg.cropland$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="darkgoldenrod2",alpha=0.4) +
                  #geom_ribbon(data=df.c.neg.cropland[df.c.neg.cropland$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgoldenrod2", alpha=0.2) +
          geom_line(data=df.c.pos.forest[df.c.pos.forest$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="darkgreen", alpha=1) +
                  #geom_ribbon(data=df.c.pos.forest[df.c.pos.forest$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgreen", alpha=0.5) +
                  geom_line(data=df.c.pos.forest[df.c.pos.forest$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="darkgreen",alpha=0.4) +
                  #geom_ribbon(data=df.c.pos.forest[df.c.pos.forest$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgreen", alpha=0.2) +
                  geom_line(data=df.c.neg.forest[df.c.neg.forest$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="darkgreen", alpha=1) +
                  #geom_ribbon(data=df.c.neg.forest[df.c.neg.forest$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgreen", alpha=0.5) +
                  geom_line(data=df.c.neg.forest[df.c.neg.forest$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="darkgreen",alpha=0.4) +
                  #geom_ribbon(data=df.c.neg.forest[df.c.neg.forest$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="darkgreen", alpha=0.2) +
          geom_line(data=df.c.pos.urban[df.c.pos.urban$valid == "valid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="gray38", alpha=1) +
                  #geom_ribbon(data=df.c.pos.urban[df.c.pos.urban$valid == "valid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.5) +
                  geom_line(data=df.c.pos.urban[df.c.pos.urban$valid == "invalid",], aes(y=average), inherit.aes = TRUE, size = 2, colour="gray38",alpha=0.4) +
                  #geom_ribbon(data=df.c.pos.urban[df.c.pos.urban$valid == "invalid",], aes(ymin=lowerCI,ymax=upperCI), fill="gray38", alpha=0.2) +
                    geom_vline(xintercept = 0) +
                    theme_bw(base_size = 15) + 
                    scale_x_continuous(limits = c(-100, 100), breaks = seq(from=-100, to=100, by=10)) +
                    scale_y_continuous(limits = c(0, 1), breaks = seq(from = 0, to = 1, by = 0.2)) + 
                    labs (x = "Land covers change", y = "ω") 

ggsave(plot = w.all, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~all.svg",
       units = "cm", dpi = "retina", width =29.7 , height = 21)
ggsave(plot = w.all, file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/w~all.pdf",
       units = "cm", dpi = "retina", width =29.7 , height = 21)

#################################################################################
#'
#' **Delay panel**
#'

w.panel <- ggarrange(w.urban + rremove("grid") + rremove("y.title"), 
                      w.forest + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"), 
                      w.wetland + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"),
                      w.grassland + rremove("grid") + rremove("y.title"), 
                      w.cropland + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"), 
                      w.temp + rremove("y.text") + rremove("y.title") + rremove("y.ticks") + rremove("grid"),
                      labels = c("A", "B", "C", "D", "E", "F"),
                      label.x = 0.92, label.y = 0.985,
                      font.label = list(size = 13, face = "bold", color ="black"),
                      ncol = 3, nrow = 2,
                      widths = c(1,0.9,0.9), heights = c(1,1))
annotate_figure(w.panel,left = text_grob("Proportional contribution of past timepoint", 
                                         size = 17, color = "black", rot = 90))
                          
ggsave(file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/PANEL_w~delta.svg",
       units = "cm", dpi = "retina", width =29.7 , height = 21)
ggsave(file = "USBBS_Modelling_Delays/USBBS_Modelling_Output/w~deltavars_plots/PANEL_w~delta.pdf",
       units = "cm", dpi = "retina", width =29.7 , height = 21)

#################################################################################
#'
#' **half saturation**
#'

df.c.pos.urban[df.c.pos.urban$average<=0.5,]

df.c.pos.forest[df.c.pos.forest$average<=0.5,]
  df.c.neg.forest[df.c.neg.forest$average<=0.5,]

df.c.pos.grassland[df.c.pos.grassland$average<=0.5,]
  df.c.neg.grassland[df.c.neg.grassland$average<=0.5,]

df.c.pos.cropland[df.c.pos.cropland$average<=0.5,]
  df.c.neg.cropland[df.c.neg.cropland$average<=0.5,]

df.c.pos.wetland[df.c.pos.wetland$average<=0.5,]
  df.c.neg.wetland[df.c.neg.wetland$average<=0.5,]
  
df.c.pos.temp[df.c.pos.temp$average<=0.5,]
  df.c.neg.temp[df.c.neg.temp$average<=0.5,]

#################################################################################
#'
#' **half saturations**
  
df.c.pos.urban[df.c.pos.urban$delta.values==10,]
  
df.c.pos.forest[df.c.pos.forest$delta.values==10,]
df.c.neg.forest[df.c.neg.forest$delta.values==-10,]
  
df.c.pos.grassland[df.c.pos.grassland$delta.values==10,]
df.c.neg.grassland[df.c.neg.grassland$delta.values==-10,]
  
df.c.pos.cropland[df.c.pos.cropland$delta.values==10,]
df.c.neg.cropland[df.c.neg.cropland$delta.values==-10,]
  
df.c.pos.wetland[df.c.pos.wetland$delta.values==10,]
df.c.neg.wetland[df.c.neg.wetland$delta.values==-10,]
  
df.c.pos.temp[df.c.pos.temp$delta.values==1,]
df.c.neg.temp[df.c.neg.temp$delta.values==-1,]


















