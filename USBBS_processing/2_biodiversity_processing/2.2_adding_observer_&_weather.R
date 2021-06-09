library(tidyverse)

for (file in 2:3) {

  f <- "min"
  load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/alpha_", f, ".rda"))
  
  # load in observer
  obs.weather <- as_tibble(read.csv("USBBS_data/routes_data/observer_weather.csv")) %>%
                  transmute(route = paste0(str_pad(StateNum,2,"left",0),"_",str_pad(Route,3,"left",0)),
                            year = Year, recorder = ObsN, start.time = StartTime, end.time=EndTime) %>%
                 filter(route %in% unique(alphas$route)) 
  
  # just calculated even though we only care about second timepoint in model
  obs.weather.t1 <- obs.weather %>% filter(year < 2003) %>% filter(year > 1999) %>% mutate(year = "t1") %>%
    group_by(route) %>% summarise(observer.t1 = max(recorder), 
                                  time_1 = ceiling(mean(start.time)),
                                  time_5 = ceiling(mean(end.time))) %>%
    mutate(time_3 = ceiling((time_1 + time_5)/2)) %>% 
    mutate(time_2 = ceiling((time_1 + time_3)/2),
           time_4 = ceiling((time_3 + time_5)/2)) %>% 
    transmute(route, observer.t1,
              # all times rescaled in relation to earlier route ever across all, 4.14 am
              time_1 = time_1 - min(time_1),
              time_2 = time_2 - min(time_1),
              time_3 = time_3 - min(time_1),
              time_4 = time_4 - min(time_1),
              time_5 = time_5 - min(time_1))
  
  obs.weather.t2 <- obs.weather %>% filter(year < 2018) %>% filter(year > 2014) %>% mutate(year = "t2") %>%
    group_by(route) %>% summarise(observer.t2 = max(recorder), 
                                  time_1 = ceiling(mean(start.time)),
                                  time_5 = ceiling(mean(end.time))) %>%
    mutate(time_3 = ceiling((time_1 + time_5)/2)) %>% 
    mutate(time_2 = ceiling((time_1 + time_3)/2),
           time_4 = ceiling((time_3 + time_5)/2),
           min.time = min(time_1)) %>% 
    transmute(route, observer.t2,
              # all times rescaled in relation to earlier route ever across all, 4.14 am
              time_1 = time_1 - min.time,
              time_2 = time_2 - min.time,
              time_3 = time_3 - min.time,
              time_4 = time_4 - min.time,
              time_5 = time_5 - min.time)
  
  # two observers for some routes so they don't match in length between t1 and t2, route 72_194, 72_195
  #obs.weather.t2 [c(848:851), ]
  
  # no idea why so just remove one of the entry
  #obs.weather.t2 <- obs.weather.t2[- c(849,850), ] 
  
  # load in cars and noise info, not used
  #cars.noise <- read.csv("USBBS_data/routes_data/cars_noise_01_16.csv")%>%
                  #transmute(route = paste0(str_pad(StateNum,2,"left",0),"_",str_pad(Route,3,"left",0)),
                            #year = Year) 
  
  fun.names <- c("min", "mean", "max")
  
  for (f in fun.names) {
    
    # load in diversity measures
    load(paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/alpha_", f, ".rda"))
  
    #alphas <- left_join(alphas, obs.weather.t1, by="route")
    alphas <- alphas %>% arrange(partition)
    alphas$time.t2 <- obs.weather.t2 %>% arrange(route) %>% select(time_1, time_2, time_3, time_4, time_5) %>% t() %>% as.vector()
      
    alphas <- left_join(alphas, obs.weather.t2 %>% select(route, observer.t2), by="route")
    
    save(alphas, file=paste0("USBBS_data/diversity_data/", file, "years_x_timepoint/alpha_", f, ".rda"))
    
  }
}