library(summarytools); library(tidyverse); library(ggthemes)
st_options(footnote=NA, style="grid", descr.transpose=T, 
           descr.stats=c("mean", "sd", "med", "min", "max"))

theme <- theme_clean() + theme(axis.title=element_text(size=15), axis.text=element_text(size=10), 
                               #axis.ticks=element_blank(), axis.line=element_blank(), 
                               legend.text=element_text(size=14), legend.title=element_text(size=15), 
                               plot.background = element_rect(color = "white"), 
                               #panel.grid = element_blank(), panel.border= element_blank(),
                               strip.text=element_text(size=15))

#'##########################################################################
#'
#' *Import sp matrices and setup diversity datafile*
#'
fun.names <- c("min", "mean", "max")

for (file in 2:3) {

   for (f in fun.names) {
      
   load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/alpha_", f, ".rda"))
      
   ####################################################################################################################
   #'
   #' *Descriptive Statistics*
   #'
   
   print(descr(alphas),
         file = paste0("USBBS_output/diversity_output/", file, "years_x_timepoint/diversity_",f,"/USBBS_Diversity_Descriptive_",f,".html"), 
         report.title = paste0("USBBS Alpha diversity ",f," abundance"),
         footnote = paste0("USA BBS Dataset - Alpha diversity measures - ",f, " abundance"))
   
      
      for (i in 1:6) {
         
      
         ####################################################################################################################
         #' **PLOT OF DIVERSITY MEASURES**
         ####################################################################################################################
         #'
         #' *Scatter plot of diversity measures between the two timepoints* 
         #'
         #' Only figures of species richness and evenness are presented in the paper
         
         divs.t1 <- c('q0.t1', 'q1.t1', 'q2.t1', 'qInf.t1', 'berger.t1', 'even.t1') 
         divs.t2 <- c('q0.t2', 'q1.t2', 'q2.t2', 'qInf.t2', 'berger.t2', 'even.t2') 
         divs.name <- c('q0', 'q1', 'q2', 'qInf', 'Berger-Parker dominance', 'Evenness')
         divs.increment <- c(10, 5, 5, 2.5, 0.1, 0.1)
         divs.limits <- c(max(alphas$q0.t1), max(alphas$q1.t1), max(alphas$q2.t1), max(alphas$qInf.t1), 1, 1)
         div.colours <- c('violetred3', 'orange', 'dodgerblue3', 'brown3', 'darkturquoise', 'limegreen')
         
         #' *Select which measure you want 1 = species richness, 2 = log(shannon entropy), 3 = 1/Simpson, 4 = Berger Parker, 5 = Evenness*
         #'
         
         scatter.div <- ggplot(alphas, aes_string(divs.t1[i], divs.t2[i])) +  
                         geom_point(inherit.aes = TRUE, size = 1.8, alpha = 0.25, colour=div.colours[i]) +
                         geom_smooth(method="glm", color="black") +
                         theme + 
                         scale_x_continuous(limits = c(0, divs.limits[i]), breaks = seq(from = 0, to = divs.limits[i], by = divs.increment[i])) +
                         scale_y_continuous(limits = c(0, divs.limits[i]), breaks = seq(from = 0, to = divs.limits[i], by = divs.increment[i])) + 
                         labs (x = paste(divs.name[i], " in 2001"), y = paste(divs.name[i], " in 2016"), title = paste('')) + 
                         geom_rug(colour="black", alpha=0.05 ) + geom_abline(size=0.5)
         
         ggsave(plot=scatter.div, paste0("USBBS_output/diversity_output/", file, "years_x_timepoint/diversity_",f,"/scatter_",str_sub(divs.t1[i],1,nchar(divs.t1[i])-3),"_",f,".svg"),
                units = "cm", dpi = "retina", width =15 , height = 15)
         
         ####################################################################################################################
         #'
         #' *CHANGE histograms of species richness and evenness between the two years*
         #'
         divs.change <- c('delta.q0', 'delta.q1', 'delta.q2', 'delta.qInf', 'delta.berger', 'delta.even') 
         divs.limits <- c(40, 40, 40, 20, 1, 1)
         divs.increment <- c(5, 5, 5, 5, 0.25, 0.25)
         
         change.hist <- ggplot(alphas, aes_string(x = divs.change[i])) +  # violetred3, limegreen
                          geom_histogram(inherit.aes=T, fill=div.colours[i], col = "black", center = 0, size=0.3) +
                           geom_vline(xintercept=0, size=0.9)+
                           theme +
                           labs(title=paste0('Change between 2001 and 2016, using ',f," abundances"), x = paste0("\u0394 ",divs.name[i]), y = "Frequency") +
                           scale_x_continuous(limits = c(-divs.limits[i], divs.limits[i]), breaks = seq(from = -divs.limits[i], to = divs.limits[i], by = divs.increment[i])) 
         
         ggsave(plot=change.hist,  paste0("USBBS_output/diversity_output/", file, "years_x_timepoint/diversity_",f,"/delta_",str_sub(divs.t1[i],1,nchar(divs.t1[i])-3),"_",f,".svg"),
                units = "cm", dpi = "retina", width =15 , height = 15)
      }
   }  
}  
   