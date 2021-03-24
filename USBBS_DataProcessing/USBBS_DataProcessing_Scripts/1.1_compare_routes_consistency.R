#'
#'*Compare routes survey consistency across multiple years*
#'

library(tidyverse)
library(stringr)

# load in subset for 2000. 2001, 2002 & 2015, 2016, 2017
load("USBBS_DataProcessing/USBBS_subset.rda")

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

#write.csv(usbbs.00.01.15.16, "USBBS_DataProcessing/USBBS_00.01_15.16..csv", row.names=F)
save(usbbs.data, file="USBBS_DataProcessing/USBBS_00.01_15.16.rda")


