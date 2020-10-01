
#'
#'*Set routes names to USBBS data from routes.csv*
#'
#' THIS TAKES FOREVER 
#'

library(tidyverse)

usbbs.data <- read.csv("USBBS.01.16.csv")

usbbs.routes <- read.csv("D:/USBBS_DATA/USBBS_Diversity/routes.csv") 

usbbs.routes <- usbbs.routes %>% select(StateNum, Route, RouteName, RouteTypeDetailID) %>%
                  mutate_if(is.factor, as.character)

usbbs.data$RouteName <- 0

for(i in 1:nrow(usbbs.data)){
  
  for(j in 1:nrow(usbbs.routes)){
    
   if(usbbs.data$StateNum[i] == usbbs.routes$StateNum[j])
     if(usbbs.data$Route[i] == usbbs.routes$Route[j])
        usbbs.data$RouteName[i] <- usbbs.routes$RouteName[j]
  }
  
  # fancy progress bar
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

# give it a small clean of useless vars
usbbs.data <- usbbs.data %>%
                select(-RouteDataID, -CountryNum, -StateNum, -Route, -RPID)

# remove short routes, less than 50 stops

short.routes <- usbbs.routes %>% filter(RouteTypeDetailID>2)

usbbs.data <- usbbs.data %>% filter(!RouteName %in% short.routes)

#################################################################

write.csv(usbbs.data, "USBBS.01.16.csv", row.names=F)

