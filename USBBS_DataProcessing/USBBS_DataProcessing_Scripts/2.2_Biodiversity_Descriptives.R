library(summarytools)
st_options(footnote=NA, style="grid", descr.transpose=T, 
           descr.stats=c("mean", "sd", "med", "min", "max"))
library(tidyverse)

#'##########################################################################
#'
#' *Import sp matrices and setup diversity datafile*
#'
load("USBBS_DataProcessing/alpha.01.16.rda")
data <- alphas

####################################################################################################################
#'
#' *Descriptive Statistics*
#'

view(summarytools::descr(data))
print(descr(data),
      file = "USBBS_DataProcessing/USBBS_Biodiversity_Output/USBBS_Diversity_Descriptive_Stats.html", 
      report.title = "USBBS Alpha diversity",
      footnote = "<b>USA BBS Dataset</b><br/><b>Alpha diversity measures, Hill numbers</b>")

####################################################################################################################
#' **PLOT OF DIVERSITY MEASURES**
####################################################################################################################
#'
#' *Scatter plot of diversity measures between the two timepoints* 
#'
#' Only figures of species richness and evenness are presented in the paper

divs.t1 <- c('q0.t1', 'q1.t1', 'q2.t1', 'qInf.t1', 'berger.t1', 'even.t1') 
divs.t2 <- c('q0.t2', 'q1.t2', 'q2.t2', 'qInf.t2', 'berger.t2', 'even.t2') 
divs.name <- c('Species richness', 'log(Shannon entropy)', '1/Simpson index', 'Berger-Parker Hills', 'Berger-Parker dominance', 'Evenness')
divs.increment <- c(10, 5, 5, 2.5, 0.1, 0.1)
divs.limits <- c(80, 50, 40, 20, 1, 1)
div.colours <- c('violetred3', 'orange', 'dodgerblue3', 'brown3', 'darkturquoise', 'limegreen')

#' *Select which measure you want 1 = species richness, 2 = log(shannon entropy), 3 = 1/Simpson, 4 = Berger Parker, 5 = Evenness*
#'
i <- 6

scatter.div <- ggplot(data, aes_string(divs.t1[i], divs.t2[i])) +  
                geom_point(inherit.aes = TRUE, size = 1.8, alpha = 0.25, colour=div.colours[i]) +
                geom_smooth() +
                theme_bw() + 
                theme(legend.position = "none", strip.background = element_rect("gray90", size=0.5),
                      strip.text = element_text(size = 8, family = "sans", )) +
                scale_x_continuous(limits = c(0, divs.limits[i]), breaks = seq(from = 0, to = divs.limits[i], by = divs.increment[i])) +
                scale_y_continuous(limits = c(0, divs.limits[i]), breaks = seq(from = 0, to = divs.limits[i], by = divs.increment[i])) + 
                labs (x = paste(divs.name[i], " in 2001"), y = paste(divs.name[i], " in 2016"), title = paste('')) + 
                geom_rug(colour="black", alpha=0.05 ) + geom_abline(size=0.5)

ggsave(plot=scatter.div, "USBBS_DataProcessing/USBBS_Biodiversity_Output/even_scatter_01_16.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)

####################################################################################################################
#'
#' *CHANGE histograms of species richness and evenness between the two years*
#'

change.hist <- ggplot(data, aes(x = delta.q0)) +  # violetred3, limegreen
                geom_histogram(inherit.aes=T, binwidth = 1, fill="violetred3", col = "black", 
                               center = 0, size=0.5) +
                  geom_vline(xintercept=0, size=1.1)+
                  theme_bw() +
                  theme(legend.position = "none", strip.background = element_rect("gray90", size=0.5),
                        strip.text = element_text(size = 8, family = "sans", )) +
                  labs(title='', x = "\u0394 Species richness between 2001 and 2016", y = "Frequency") +
                  scale_x_continuous(limits = c(-40, 40), breaks = seq(from = -200, to = 200, by = 10)) 

ggsave(plot=change.hist, "USBBS_DataProcessing/USBBS_Biodiversity_Output/delta.q0_hist_01_16.svg",
       units = "cm", dpi = "retina", width =15 , height = 15)
