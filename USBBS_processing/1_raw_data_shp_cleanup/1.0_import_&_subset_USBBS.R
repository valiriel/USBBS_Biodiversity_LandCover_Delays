#'
#' *Import all USBBS data raw and keep only routes with values around*
#' *timepoints and excellent survey quality RPID 101*
#'
library(tidyverse)

# Import the multiple xlxs sheets containing all the USBBS data from 1966 to 2018
# saved on external folder as they are massive in size
usbbs.all.1 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=1)
usbbs.all.2 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=2)
usbbs.all.3 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=3)
usbbs.all.4 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=4)
usbbs.all.5 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=5)
usbbs.all.6 <- readxl::read_xlsx("D:/USBBS_DATA/USBBS_Diversity/USBBS_RawData/USBBS_ALL.xlsx", sheet=6)

# make a list with all the usbbs files
usbbs.all <- list(usbbs.all.1, usbbs.all.2, usbbs.all.3, usbbs.all.4, usbbs.all.5, usbbs.all.6)

# setup empty df
usbbs.data <- head(usbbs.all.1,0)

# pick years we want to extract
yrs <- c(2000, 2001, 2002, 2015, 2016, 2017)

for (usbbs.temp in usbbs.all) {
  
  t.usbbs <- usbbs.temp %>% filter(Year==yrs[1] | Year==yrs[2] | Year==yrs[3] | 
                                     Year==yrs[4] | Year==yrs[5] | Year==yrs[6])
                      
  usbbs.data <- rbind(usbbs.data, t.usbbs)
  
}

# keep only observation of high quality, (good weather etc.), standard set by USBBS guidelines
usbbs.data <- usbbs.data %>%  filter(RPID==101)

usbbs.data # have a look at that tibble

#write.csv(usbbs.data, "USBBS_DataProcessing/USBBS_subset.csv", row.names=F)
save(usbbs.data, file="USBBS_data/diversity_data/USBBS_subset.rda")

#'--------------------------------------------------------------------------------------------------
#'*Compare routes survey consistency across multiple years*

# load in subset for 2000. 2001, 2002 & 2015, 2016, 2017
load("USBBS_data/diversity_data/USBBS_subset.rda")

# generate Unique_State_Route_ID 
# to be matched with route shapefile as it missed route name

# set route and state string value to same length as shapefile for matching
usbbs.data$StateNum <- str_pad(usbbs.data$StateNum, width=2, side="left", pad="0")  
unique(usbbs.data$StateNum)
usbbs.data$Route <- str_pad(usbbs.data$Route, width=3, side="left", pad="0")
unique(usbbs.data$Route)

# generate unique state, route index
usbbs.data$U_S_R_I <- paste0(usbbs.data$StateNum, "_", usbbs.data$Route)
unique(usbbs.data$U_S_R_I)

# select unique Unique_State_Route_ID surveyed in each year

usbbs.00.unique <- unique(usbbs.data %>%
                            group_by(U_S_R_I) %>%
                            filter(Year==2000) %>% select(U_S_R_I) %>%
                            group_by()) %>% mutate_if(is.factor, as.character) 

usbbs.01.unique <- unique(usbbs.data %>%
                            group_by(U_S_R_I) %>%
                            filter(Year==2001) %>% select(U_S_R_I)%>%
                            group_by()) %>% mutate_if(is.factor, as.character)

usbbs.02.unique <- unique(usbbs.data %>%
                            group_by(U_S_R_I) %>%
                            filter(Year==2002) %>% select(U_S_R_I)%>%
                            group_by()) %>% mutate_if(is.factor, as.character)

usbbs.15.unique <- unique(usbbs.data %>%
                            group_by(U_S_R_I) %>%
                            filter(Year==2015) %>% select(U_S_R_I)%>%
                            group_by()) %>% mutate_if(is.factor, as.character)

usbbs.16.unique <- unique(usbbs.data %>%
                            group_by(U_S_R_I) %>%
                            filter(Year==2016) %>% select(U_S_R_I)%>%
                            group_by()) %>% mutate_if(is.factor, as.character)

usbbs.17.unique <- unique(usbbs.data %>%
                            group_by(U_S_R_I) %>%
                            filter(Year==2017) %>% select(U_S_R_I)%>%
                            group_by()) %>% mutate_if(is.factor, as.character)

############
# get how many routes surveyed consistently across year ranges

routes.6years <- Reduce(intersect, list(usbbs.00.unique, usbbs.01.unique, usbbs.02.unique, usbbs.15.unique, usbbs.16.unique, usbbs.17.unique))
# 1250 years                          

routes.00.01.15.16 <- Reduce(intersect, list(usbbs.00.unique, usbbs.01.unique, usbbs.15.unique, usbbs.16.unique)) 
# 1477 routes                                        

routes.01.16 <- Reduce(intersect, list(usbbs.01.unique, usbbs.16.unique))
# 1815 routes

#'
#'*keep routes for 00.01.15.16 subset* to account for stochasticity in count, see supplementary methodology
#'

usbbs.00.01.15.16 <- usbbs.data %>% mutate_if(is.factor, as.character) %>%
  filter(U_S_R_I %in% routes.00.01.15.16$U_S_R_I)
usbbs.data <- usbbs.00.01.15.16
save(usbbs.data, file="USBBS_data/diversity_data/2years_x_timepoint/USBBS_years_subset.rda")

usbbs.00.01.02.15.16.17 <- usbbs.data %>% mutate_if(is.factor, as.character) %>%
  filter(U_S_R_I %in% routes.6years$U_S_R_I)
usbbs.data <- usbbs.00.01.02.15.16.17
save(usbbs.data, file="USBBS_data/diversity_data/3years_x_timepoint/USBBS_years_subset.rda")
