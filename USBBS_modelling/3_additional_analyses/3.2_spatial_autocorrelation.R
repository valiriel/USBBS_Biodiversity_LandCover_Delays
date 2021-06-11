
library(tidyverse); library(spdep); library(sf)
f <- "mean"; buffer.size <- "500"; buffer.type <- "segment"; file <- 3

#'--------------------------------------------------------------------------
#' *import data*

# load in spatial features
load(paste0("USBBS_data/landscape_data/landcover_data/landcover_%_&_delta_extracted/lc_", buffer.type, "_", buffer.size, ".rda"))
lc$partition[lc$route=="17_222"] <- paste0("17_222_",1:5)
lc <- lc %>% dplyr::select(geometry, partition) %>% st_transform(crs=4326)

# load data
f <- "mean"; buffer.size <- "500"; buffer.type <- "segment"; file <- 3
load(paste0("USBBS_data/whole_data/", file, "years_x_timepoint/data_", f, "_", buffer.type,"_",buffer.size,".rda"))
data <- data %>% filter(segmentID==1 | segmentID==3 | segmentID==5)

data <- st_as_sf(left_join(data, lc, by="partition"))

# get centroid coordinates of each segment
data$centroid_X <- st_coordinates(st_centroid(data))[,1]
data$centroid_Y <- st_coordinates(st_centroid(data))[,2]

# look at some plots
mapview::mapview(data, zcol="q1.t2")

# load model result
f <- "mean"; buffer.size <- "500"; buffer.type <- "segment"; file <- 3
result <- readRDS(paste0("USBBS_modelling/1_model_fitting/stan_model_fitted/sampled_", f, "_", buffer.type,"_",buffer.size,".RDS"))

rm(lc, buffer.size, buffer.type, f, file)

#'--------------------------------------------------------------------------
#' *calculate neighbors for multiple distance bands*
#' 

# slower option using spdep 
# https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html

#dataa <- data[1:2000,]
S.dist  <-  dnearneigh(cbind(data$centroid_X, data$centroid_Y), 0, 500)
lw <- nb2listw(S.dist, style="W",zero.policy=T) 
MI1  <-  moran.mc(data$q1.t2, lw, nsim=1000); MI1

S.dist  <-  dnearneigh(cbind(data$centroid_X, data$centroid_Y), 0, 1000)
lw <- nb2listw(S.dist, style="W",zero.policy=T) 
MI2  <-  moran.mc(data$q1.t2, lw, nsim=1000); MI2

S.dist  <-  dnearneigh(cbind(data$centroid_X, data$centroid_Y), 0, 5000)
lw <- nb2listw(S.dist, style="W",zero.policy=T) 
MI3  <-  moran.mc(data$q1.t2, lw, nsim=1000); MI3

S.dist  <-  dnearneigh(cbind(data$centroid_X, data$centroid_Y), 0, 10000)
lw <- nb2listw(S.dist, style="W",zero.policy=T) 
MI4 <-  moran.mc(data$q1.t2, lw, nsim=1000); MI4

S.dist  <-  dnearneigh(cbind(data$centroid_X, data$centroid_Y), 0, 100000)
lw <- nb2listw(S.dist, style="W",zero.policy=T) 
MI5 <-  moran.mc(data$q1.t2, lw, nsim=1000); MI5

# adaptation of ape package Moran function using C++ rcpp
# library(devtools); install_github('mcooper/moranfast'); 
library(moranfast)

moranfast(data$q1.t2, data$centroid_X, data$centroid_Y)

?Moran.I()
