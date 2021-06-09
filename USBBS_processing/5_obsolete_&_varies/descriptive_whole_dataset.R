library(summarytools)
st_options(footnote=NA, style="grid", descr.transpose=T, 
           descr.stats=c("mean", "sd", "med", "min", "max"))
library(tidyverse)

load("USBBS_Modelling_Delays/data.withpred.notemplag.rda")

view(summarytools::descr(data))
print(descr(data),
      file = "Data_Descriptive_Stats.html", 
      report.title = "Descriptive statstics whole data")
