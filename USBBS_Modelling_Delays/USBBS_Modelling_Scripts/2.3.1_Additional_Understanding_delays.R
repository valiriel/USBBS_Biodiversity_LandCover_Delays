data$dominant.t1 <- "A" # initialize
data$dominant.t2 <- "A" # initialize

####################################################

for(i in 1:N){
  
  yo <- 0
  
  if(data$urban.t1[i] > yo) {
    yo <- data$urban.t1[i] 
    data$dominant.t1[i] <- "urban"}
  
  if(data$forest.t1[i] > yo){
    yo <- data$forest.t1[i] 
    data$dominant.t1[i] <- "forest"}
  
  if(data$grassland.t1[i] > yo){
    yo <- data$grassland.t1[i] 
    data$dominant.t1[i] <- "grassland"}
  
  if(data$wetland.t1[i] > yo){
    yo <- data$wetland.t1[i] 
    data$dominant.t1[i] <- "wetland"}
  
  if(data$cropland.t1[i] > yo){
    yo <- data$cropland.t1[i] 
    data$dominant.t1[i] <- "cropland"}
  
}

for(j in 1:N){
  
  yo <- 0
  
  if(data$urban.t2[j] > yo) {
    yo <- data$urban.t2[j] 
    data$dominant.t2[j] <- "urban"}
  
  if(data$forest.t2[j] > yo){
    yo <- data$forest.t2[j] 
    data$dominant.t2[j] <- "forest"}
  
  if(data$grassland.t2[j] > yo){
    yo <- data$grassland.t2[j] 
    data$dominant.t2[j] <- "grassland"}
  
  if(data$wetland.t2[j] > yo){
    yo <- data$wetland.t2[j] 
    data$dominant.t2[j] <- "wetland"}
  
  if(data$cropland.t2[j] > yo){
    yo <- data$cropland.t2[j] 
    data$dominant.t2[j] <- "cropland"}
  
}

data$dominant.change <- "a" # initialize

for (k in 1:N) {
  
  if(data$dominant.t1[k] == data$dominant.t2[k]){
    data$dominant.change[k] <- "no"
  } else{data$dominant.change[k] <- "yes"}
  
}
####################################################

data$mainchange <- "a" # initialize

for(i in 1:N){
  
  yo <- 0
  
  if(abs(data$delta.urban[i]) > yo) {
    yo <- abs(data$delta.urban[i])
    data$mainchange[i] <- ifelse(data$delta.urban[i]>0, "pos.urban", "neg.urban")}
  
  if(abs(data$delta.forest[i]) > yo){
    yo <- abs(data$delta.forest[i])
    data$mainchange[i] <- ifelse(data$delta.forest[i]>0, "pos.forest", "neg.forest")}
  
  if(abs(data$delta.grassland[i]) > yo){
    yo <- abs(data$delta.grassland[i]) 
    data$mainchange[i] <- ifelse(data$delta.grassland[i]>0, "pos.grassland", "neg.grassland")}
  
  if(abs(data$delta.wetland[i]) > yo){
    yo <- abs(data$delta.wetland[i])
    data$mainchange[i] <- ifelse(data$delta.wetland[i]>0, "pos.wetland", "neg.wetland")}
  
  if(abs(data$delta.cropland[i]) > yo){
    yo <- abs(data$delta.cropland[i]) 
    data$mainchange[i] <- ifelse(data$delta.cropland[i]>0, "pos.cropland", "neg.cropland")}
  
  if(data$mainchange[i] == "a"){
    data$mainchange[i] <- ifelse(data$delta.temp[i]>0, "pos.temp", "neg.temp")}
}

####################################################

data <- data %>% transmute(partition, 
                           q0.eq.t2 = q0.eq,
                           q0.obs.t2 = q0.t2,
                           mainchangetype = mainchange,
                           dominant.t1 = dominant.t1,
                           dominant.t2 = dominant.t2,
                           dominant.change = dominant.change,
                           urban.change = delta.urban, pos.urban.change = delta.pos.urban,
                           forest.change = delta.forest, pos.forest.change = delta.pos.forest, neg.forest.change = delta.neg.forest,
                           grass.change = delta.grassland, pos.grass.change = delta.pos.grassland, neg.grass.change = delta.neg.grassland,
                           crop.change = delta.cropland, pos.crop.change = delta.pos.cropland, neg.crop.change = delta.neg.cropland,
                           temp.change = delta.temp, #pos.temp.change = delta.pos.temp, neg.temp.change = delta.neg.temp,
                           wet.change= delta.wetland, pos.wet.change = delta.pos.wetland, neg.wet.change = delta.neg.wetland) %>%
  mutate(debtcredit = q0.eq.t2 - q0.obs.t2) %>%
  na.omit()

data %>% count(dominant.change) 

data %>% group_by(mainchangetype) %>% 
  summarise(mean = mean(debtcredit),
            sd = sd(debtcredit),
            median= median(debtcredit),
            max = max(debtcredit),
            min = min(debtcredit),
            n = n()) 

#' *checks on dominant land cover change kind of failed*
#' 
#' *switch to whats the largest land cover change*

###################################################################################################################
#'
#' *BCR DEBT CREDIT ANALYSIS*
#'

library(rgdal)

a <- readOGR("D:/USBBS_DATA/USBBS_LandCover/predict_point_BCR.shp") 

data.BCR.debtcredit <- na.omit(a@data)

data.BCR.debtcredit <- data.BCR.debtcredit %>% filter( BCRNAME!= "BOREAL TAIGA PLAINS") %>% filter( BCRNAME!= "GREAT LAKES")

summary.BCR <- data.BCR.debtcredit %>% group_by(BCRNAME) %>% 
  summarise(mean = mean(dbtcrdt),
            sd = sd(dbtcrdt),
            median= median(dbtcrdt),
            max = max(dbtcrdt),
            min = min(dbtcrdt),
            n = n()) 

ggplot(data = data.BCR.debtcredit, aes(x=reorder(BCRNAME, dbtcrdt) , y=dbtcrdt)) +
  geom_boxplot(outlier.alpha = 0.05) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip() + 
  scale_y_continuous(limits = c(-3,3) )

#'
#' *try to group BCR based on conservation joint venture *
#'

data.BCR.debtcredit$aggregate.BRC <- "a"

for(i in 1:nrow(data.BCR.debtcredit)) {
  
  if(data.BCR.debtcredit$BCRNAME[i]=="NORTHERN PACIFIC RAINFOREST")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Pacific coast"
  if(data.BCR.debtcredit$BCRNAME[i]=="COASTAL CALIFORNIA")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Pacific coast"
  
  if(data.BCR.debtcredit$BCRNAME[i]=="GREAT BASIN")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Intermountain west"
  if(data.BCR.debtcredit$BCRNAME[i]=="NORTHERN ROCKIES")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Intermountain west"
  if(data.BCR.debtcredit$BCRNAME[i]=="SIERRA NEVADA")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Intermountain west"
  if(data.BCR.debtcredit$BCRNAME[i]=="SOUTHERN ROCKIES/COLORADO PLATEAU")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Intermountain west"
  
  if(data.BCR.debtcredit$BCRNAME[i]=="SONORAN AND MOJAVE DESERTS")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Desert"
  if(data.BCR.debtcredit$BCRNAME[i]=="SIERRA MADRE OCCIDENTAL")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Desert"
  if(data.BCR.debtcredit$BCRNAME[i]=="CHIHUAHUAN DESERT")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Desert"
  
  if(data.BCR.debtcredit$BCRNAME[i]=="BADLANDS AND PRAIRIES")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Prairies"
  if(data.BCR.debtcredit$BCRNAME[i]=="PRAIRIE POTHOLES")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Prairies"
  if(data.BCR.debtcredit$BCRNAME[i]=="SHORTGRASS PRAIRIE")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Prairies"
  if(data.BCR.debtcredit$BCRNAME[i]=="CENTRAL MIXED GRASS PRAIRIE")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Prairies"
  
  if(data.BCR.debtcredit$BCRNAME[i]=="EASTERN TALLGRASS PRAIRIE")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Great Lakes region"
  if(data.BCR.debtcredit$BCRNAME[i]=="PRAIRIE HARDWOOD TRANSITION")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Great Lakes region"
  if(data.BCR.debtcredit$BCRNAME[i]=="BOREAL HARDWOOD TRANSITION")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Great Lakes region"
  if(data.BCR.debtcredit$BCRNAME[i]=="LOWER GREAT LAKES/ ST. LAWRENCE PLAIN")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Great Lakes region"
  
  if(data.BCR.debtcredit$BCRNAME[i]=="APPALACHIAN MOUNTAINS")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Appalachian"
  
  if(data.BCR.debtcredit$BCRNAME[i]=="ATLANTIC NORTHERN FOREST")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Atlantic coast"
  if(data.BCR.debtcredit$BCRNAME[i]=="NEW ENGLAND/MID-ATLANTIC COAST")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Atlantic coast"
  if(data.BCR.debtcredit$BCRNAME[i]=="PIEDMONT")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Atlantic coast"
  if(data.BCR.debtcredit$BCRNAME[i]=="PENINSULAR FLORIDA")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Atlantic coast"
  if(data.BCR.debtcredit$BCRNAME[i]=="SOUTHEASTERN COASTAL PLAIN")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Atlantic coast"
  
  
  if(data.BCR.debtcredit$BCRNAME[i]=="GULF COASTAL PRAIRIE")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Mississippi and Texas"
  if(data.BCR.debtcredit$BCRNAME[i]=="TAMAULIPAN BRUSHLANDS")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Mississippi and Texas"
  if(data.BCR.debtcredit$BCRNAME[i]=="EDWARDS PLATEAU")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Mississippi and Texas"
  if(data.BCR.debtcredit$BCRNAME[i]=="OAKS AND PRAIRIES")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Mississippi and Texas"
  if(data.BCR.debtcredit$BCRNAME[i]=="MISSISSIPPI ALLUVIAL VALLEY")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Mississippi and Texas"
  if(data.BCR.debtcredit$BCRNAME[i]=="WEST GULF COASTAL PLAIN/OUACHITAS")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Mississippi and Texas"
  if(data.BCR.debtcredit$BCRNAME[i]=="CENTRAL HARDWOODS")
    data.BCR.debtcredit$aggregate.BRC[i] <- "Mississippi and Texas"
  
}

ggplot(data = data.BCR.debtcredit, aes(x=reorder(aggregate.BRC, dbtcrdt), y=dbtcrdt), fill = aggregate.BRC) +
  geom_boxplot(outlier.shape = NA) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip() + 
  labs(y="Extinction debts and Colonization credits", x="Geographical Region") +
  scale_y_continuous(limits = c(-3,3) ) + theme_bw()
