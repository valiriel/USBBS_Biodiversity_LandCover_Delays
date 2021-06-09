
library(tidyverse); library(ggpubr); library(ggthemes)

theme <- theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=13), 
                               #axis.ticks=element_blank(), axis.line=element_blank(), 
                               legend.text=element_text(size=15), legend.title=element_text(size=15), 
                               plot.background = element_rect(color = "white"), legend.background = element_rect(color = NA),
                               #panel.grid = element_blank(), panel.border= element_blank(),
                               strip.text=element_text(size=15))

file <- 2

for (file in 2:3) {

  load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/alpha_min.rda"))
  alphas.min <- alphas
  alphas.min$abun_type <- "min"
  
  load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/alpha_mean.rda"))
  alphas.mean <- alphas
  alphas.mean$abun_type <- "mean"
  
  load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/alpha_max.rda"))
  alphas.max <- alphas
  alphas.max$abun_type <- "max"
  
  alphas <- rbind(alphas.min, alphas.mean, alphas.max)
  
  alphas$abun_type <- as.factor(alphas$abun_type)
  
  ggplot(alphas, aes(y = q1.t2, x = q1.t1, color=abun_type)) +
    geom_point(alpha=0.2) +
    geom_smooth(color="black") + theme + #+
    facet_wrap(~abun_type) + geom_abline(size=0.5)
  ggsave(paste0("USBBS_output/diversity_output/", file, "years_x_timepoint/diversity_comparison/comparison_scatter_t1_t2.svg"), units = "cm", dpi = "retina", width =30, height = 10)
  
  ggplot(data=alphas.max, aes(x=delta.q1)) +
    geom_histogram(data=alphas.max, aes(x=delta.q1, fill="max"), col = "black", center = 0, size=0.3, alpha=0.6) +
    geom_histogram(data=alphas.mean, aes(x=delta.q1, fill="mean"), col = "black", center = 0, size=0.3, alpha=0.6) +
    geom_histogram(data=alphas.min, aes(x=delta.q1, fill="min"), col = "black", center = 0, size=0.3, alpha=0.6) + 
      geom_vline(xintercept=10, size=0.5)+
      geom_vline(xintercept=-10, size=0.5)+
      geom_vline(xintercept=0, size=0.9)+
        scale_x_continuous(breaks = c(-20, -10, 0, 10, 20)) +
    facet_wrap(~abun_type) + theme + labs(fill="abundances")
  ggsave(paste0("USBBS_output/diversity_output/", file, "years_x_timepoint/diversity_comparison/comparison_delta_hist.svg"), units = "cm", dpi = "retina", width =30, height = 10)
  
  p1 <- ggplot(alphas.max) +
    geom_point(aes_string(x=alphas.max$q1.t1, y=alphas.min$q1.t1), alpha=0.05) +
    geom_smooth(aes_string(x=alphas.max$q1.t1, y=alphas.min$q1.t1), color="blue") + 
    theme + geom_abline(size=0.5) +
    theme + labs(x = "max_q1", y = "min_q1", title="max~min")
  
  p2 <- ggplot(alphas.max) +
    geom_point(aes_string(x=alphas.max$q1.t1, y=alphas.mean$q1.t1), alpha=0.05) +
    geom_smooth(aes_string(x=alphas.max$q1.t1, y=alphas.mean$q1.t1), color="blue") + 
    theme + geom_abline(size=0.5) +
    theme + labs(x = "max_q1", y = "mean_q1", title="max~mean")
  
  p3 <- ggplot(alphas.mean) +
    geom_point(aes_string(x=alphas.mean$q1.t1, y=alphas.min$q1.t1), alpha=0.05) +
    geom_smooth(aes_string(x=alphas.mean$q1.t1, y=alphas.min$q1.t1), color="blue") + 
    theme + geom_abline(size=0.5) +
    theme + labs(x = "mean_q1", y = "min_q1", title="mean~min")
  
  ggarrange(p1,p2,p3, ncol=3)
  ggsave(paste0("USBBS_output/diversity_output/", file, "years_x_timepoint/diversity_comparison/comparison_abunType_q1.svg"), units = "cm", dpi = "retina", width =30, height = 10)
  
}  
