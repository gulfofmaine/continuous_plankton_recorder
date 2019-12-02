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

#Check Structure
head(buoys_collapsed)



#Pivot the measurements into their own columns
buoys <- buoys_collapsed %>% 
  pivot_wider(names_from = var_name, values_from = daily_mean)

#clean environment
rm(Buoys, buoy_b, buoys_collapsed)

yearly_means <- buoys %>% 
  mutate(year = lubridate::year(Date),
         year = factor(year)) %>% 
  group_by(buoy_id, year) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_sal = mean(sal, na.rm = T),
            mean_dens = mean(density, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(period = "Annual")

quarterly_means <- buoys %>% 
  mutate(year = lubridate::year(Date),
         year = factor(year),
         month_col = lubridate::month(Date),
         month_col = factor(month_col),
         period = case_when(
           month_col %in% c(1:3)   ~ "Q1",
           month_col %in% c(4:6)   ~ "Q2",
           month_col %in% c(7:9)   ~ "Q3",
           month_col %in% c(10:12) ~ "Q4"
         )) %>% 
  group_by(buoy_id, year, period) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_sal = mean(sal, na.rm = T),
            mean_dens = mean(density, na.rm = T)) %>% 
  ungroup()

#Put them back together
buoy_dataset <- full_join(yearly_means, quarterly_means)
