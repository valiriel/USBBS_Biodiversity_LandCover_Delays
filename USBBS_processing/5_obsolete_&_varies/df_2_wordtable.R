library(officer)
library(flextable)
library(magrittr)

load("USBBS_Modelling_Delays/USBBS_models_jags/results.q0.notemplag.rda")
summary.m.top <- as.data.frame(results.q0.notemplag$BUGSoutput$summary, rownames=NA)%>% round(5) %>% tibble::rownames_to_column() %>% slice(-(55):-(n()-2)) 

# Create flextable object
ft <- flextable(data = summary.m.top) %>% 
  theme_zebra %>% 
  autofit
# See flextable in RStudio viewer
ft

# Create a temp file
tmp <- tempfile(fileext = ".docx")

# Create a docx file
read_docx() %>% 
  body_add_flextable(ft) %>% 
  print(target = tmp)

# open word document
browseURL(tmp)

load("USBBS_Modelling_Delays/USBBS_models_jags/results.q0.notemplag.rda")
summary.m.top <- as.data.frame(results.q0.notemplag$BUGSoutput$summary, rownames=NA)%>% round(5) %>% rownames_to_column()   %>% slice(-(37):-(n()-1)) 


