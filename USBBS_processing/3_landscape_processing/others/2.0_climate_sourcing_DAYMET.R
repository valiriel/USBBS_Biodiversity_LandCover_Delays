#'-------------------------------------------------------------------------------------------------
#'
#' *Sourcing climatic data = temperature and precipitation* 
#' Data from Daymet, 1km gridded data, monthly average max temperature and precipitation
#' https://daac.ornl.gov/DAYMET/guides/Daymet_V4_Monthly_Climatology.html

# content of this folder is not in the repo, due to size, you can download your own from the link & file download

vars <- c("tmax", "prcp")
timepoints <- c("t1", "t2")

# var <- "tmax"; t <- "t1"; y <- 2000

for (var in vars) {
  
  for (t in timepoints) {
    
    folder <- paste0("USBBS_data/landscape_data/climate_data/climate_raw/", var, "_", t)
    dir.create(folder)
    
    if(t == "t1") years <- c(2000, 2001)
    if(t == "t2") years <- c(2015, 2016)
    
    for (y in years){
      
      if(var == "tmax") {
        
        url <- paste0("https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1855/daymet_v4_", var, "_monavg_na_", y,".nc")
        file <- paste0(folder, "/daymet_monthly_1km_avg_", var, "_", y, ".nc")
        
        if( file.exists(file) == FALSE ) download.file(url, file) 
        
      }
      
      if(var == "prcp") {
        
        url <- paste0("https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1855/daymet_v4_", var, "_monttl_na_", y,".nc")
        file <- paste0(folder, "/daymet_monthly_1km_tot_", var, "_", y, ".nc")
        
        if( file.exists(file) == FALSE ) download.file(url, file)  
        
      }
    }
  }
}

