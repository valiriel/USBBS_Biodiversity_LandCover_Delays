library(tidyverse)
library(rgdal)
library(mapview)

# Import route shapefile 
usa.buffer.debt.credit <- readOGR("D:/USBBS_DATA/USBBS_LandCover/USBBS_routes_shps/USBBS.segment.4km.buffer.shp")

usa.buffer.debt.credit@data$RTENAME

mapview(usa.buffer.debt.credit)



#import predicted data form model
load("data.iter.withpred.rda")

data <- data %>%
          transmute(RTENAME = partition,
                    debt.credit = q0.eq - q0.t2)

data$RTENAME <- gsub('.{2}$', '', data$RTENAME)

dataa <- data %>% group_by(RTENAME) %>% summarise(debt.credit = mean(debt.credit))

usa.buffer.debt.credit@data <- merge(usa.buffer.debt.credit@data, dataa, by.y= "RTENAME", all=T)

mapview(usa.buffer.debt.credit, zcol="debt.credit")

rgdal::writeOGR(usa.buffer.debt.credit, dsn="D:/USBBS_DATA/USA_predict_map", 
                layer="USA_route_predict", driver = "ESRI Shapefile",
                check_exists = FALSE)

a <- usa.buffer.debt.credit@data
