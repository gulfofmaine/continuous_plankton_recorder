# Buoy Data Management
# 12/2/2019


####  Packages  ####
library(tidyverse)
library(here)


####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####  Buoy Data  ####
load(file = str_c(cpr_boxpath, "/data/processed_data/Buoy.RData"))

#Check Reference Tree to see list structure
lobstr::ref(Buoys)

#Buoys is a nested list with environmental data for each buoy at each depth
buoy_b <- Buoys$Buoy_B

#Function to pull buoy depths and collapse
buoy_collapse <- function(buoy) {
  depth_1m  <- bind_rows(buoy$depth_1m, .id = "var_name")
  depth_20m <- bind_rows(buoy$depth_20m, .id = "var_name")
  depth_50m <- bind_rows(buoy$depth_50m, .id = "var_name")
  
  
  if(is.null(buoy$depth_100m) == T) {
    buoy_out  <- bind_rows(list("1 meter" = depth_1m, 
                                "20 meters" = depth_20m, 
                                "50 meters" = depth_50m), .id = "reading_depth")
    return(buoy_out)
    
    #Buoy N has 3 more depths...
  } else {
    depth_100m  <- bind_rows(buoy$depth_100m, .id = "var_name")
    depth_150m  <- bind_rows(buoy$depth_150m, .id = "var_name")
    depth_180m  <- bind_rows(buoy$depth_180m, .id = "var_name")
    
    buoy_out <- bind_rows(list("1 meter" = depth_1m, 
                               "20 meters" = depth_20m, 
                               "50 meters" = depth_50m,
                               "100 meters" = depth_100m,
                               "150 meters" = depth_150m,
                               "180 meters" = depth_180m), 
                          .id = "reading_depth")
    return(buoy_out)
  }
  
}

#Tester
buoy_b <- buoy_collapse(buoy_b)


#Buoys Collapsed
buoys_collapsed <- Buoys %>% map(buoy_collapse) %>% 
  bind_rows(.id = "buoy_id")