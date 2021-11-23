library(officer)
library(flextable)
library(magrittr)

f <- "mean"; buffer.size <- "500"; buffer.type <- "segment"; file <- 3
result <- readRDS(paste0("USBBS_modelling/1_model_fitting/stan_model_fitted/sampled_", f, "_", buffer.type,"_",buffer.size,".RDS"))
  params <- result$metadata()$stan_variables; params <- params[! params %in% c("lp__", "mu", "log_lik", "random_observer")]  

data <- as_tibble(result$summary(params)); data[,2:ncol(data)] <- round(data[,2:ncol(data)],3); data

# Create flextable object
ft <- flextable(data) %>% theme_zebra %>% autofit; ft; tmp <- tempfile(fileext = ".docx")

# Create a docx file
read_docx() %>% body_add_flextable(ft) %>% print(target = tmp)

# open word document
browseURL(tmp)


