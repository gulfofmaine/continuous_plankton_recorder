# Buoy Data Management
# 12/2/2019

####  Packages  ####
library(tidyverse)
library(here)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))


####____________________________####
####  Import Buoy Data  ####
#Buoy data is generated using two scripts acquired from Matt Dzaugis:
# 1. R/buoy_data/UpdateBuoy_functions.Rmd
# 2. R/buoy_data/Update_Buoy.Rmd

#load(file = str_c(cpr_boxpath, "/data/processed_data/Buoy.RData"))
load(file = str_c(cpr_boxpath, "/data/processed_data/Buoys_Md_2019.RData"))

###__ 1. Collapse List Structure  ####

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
  pivot_wider(names_from = var_name, values_from = daily_mean) %>% 
  mutate(reading_depth = factor(
    reading_depth, levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters")
  ))


#Export Dailies
write_csv(buoys, 
          path = str_c(cpr_boxpath, "data/processed_data/buoys_daily.csv", sep = "/"), 
          col_names = TRUE)

#clean environment
rm(Buoys, buoy_b)



#####__ 2. Estimate  Aggreagate Values  ####
yearly_means <- buoys %>% 
  mutate(year = lubridate::year(Date),
         year = factor(year)) %>% 
  group_by(buoy_id, year, reading_depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_sal = mean(sal, na.rm = T),
            mean_dens = mean(density, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(period = "Annual")

#Quarters are 91 julian day increments
quarterly_means <- buoys %>% 
  mutate(
         year = lubridate::year(Date),
         year = factor(year),
         julian = lubridate::yday(Date), 
         period = case_when(
           julian <= 91                       ~ "Q1",
           between(julian, left = 92, 182)    ~ "Q2",
           between(julian, left = 183, 273)   ~ "Q3",
           julian > 273                       ~ "Q4"
         )) %>% 
  group_by(buoy_id, year, period, reading_depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_sal = mean(sal, na.rm = T),
            mean_dens = mean(density, na.rm = T)) %>% 
  ungroup()

bi_monthly_means <- buoys %>% 
  mutate(year = lubridate::year(Date),
         year = factor(year),
         month_col = lubridate::month(Date),
         month_col = factor(month_col),
         period = case_when(
           month_col %in% c(1:2)     ~ "P1",
           month_col %in% c(3:4)     ~ "P2",
           month_col %in% c(5:6)     ~ "P3",
           month_col %in% c(7:8)     ~ "P4",
           month_col %in% c(9:10)    ~ "P5",
           month_col %in% c(11:12)   ~ "P6",
         )) %>% 
  group_by(buoy_id, year, period, reading_depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_sal = mean(sal, na.rm = T),
            mean_dens = mean(density, na.rm = T)) %>% 
  ungroup()

#Put them back together
buoy_dataset <- full_join(yearly_means, quarterly_means) %>% 
  full_join(bi_monthly_means)


###__ 3. Stratification index  ####
# Brunt-Vaisala frequency N = sqrt(-g/potential density * delta density/delta depth)
# the ocean stratification is quantified by the measured value of delta density/denta depth
# If the water is more stratified, the frequency is higher. If less stratified, frequency is lower
strat_set <- buoys_collapsed %>% 
  filter(var_name == "density",
         reading_depth %in% c("1 meter", "50 meters")) %>% 
         pivot_wider(names_from = reading_depth, values_from = daily_mean) %>% 
  split(.$buoy_id) %>% 
  map(~ .x %>% mutate(
    difference =  `1 meter` - `50 meters`,
    index      = sqrt(-(difference)/50)
  )) %>% 
  bind_rows()


#####__ 4. Aggregate Stratification Values  ####
yearly_strat <- strat_set %>% 
  mutate(year = lubridate::year(Date),
         year = factor(year)) %>% 
  group_by(buoy_id, year) %>% 
  summarise(mean_diff = mean(difference, na.rm = T),
            mean_strat_index = mean(index, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(period = "Annual")

quarterly_strat <- strat_set %>% 
  mutate(year = lubridate::year(Date),
         year = factor(year),
         julian = lubridate::yday(Date),
         period = case_when(
           julian <= 91                       ~ "Q1",
           between(julian, left = 92, 182)    ~ "Q2",
           between(julian, left = 183, 273)   ~ "Q3",
           julian > 273                       ~ "Q4"
         )) %>% 
  group_by(buoy_id, year, period) %>% 
  summarise(mean_diff = mean(difference, na.rm = T),
            mean_strat_index = mean(index, na.rm = T)) %>% 
  ungroup()

bi_monthly_strat <- strat_set %>% 
  mutate(year      = lubridate::year(Date),
         year      = factor(year),
         month_col = lubridate::month(Date),
         month_col = factor(month_col),
         period    = case_when(
           month_col %in% c(1:2)   ~ "P1",
           month_col %in% c(3:4)   ~ "P2",
           month_col %in% c(5:6)   ~ "P3",
           month_col %in% c(7:8)   ~ "P4",
           month_col %in% c(9:10)   ~ "P5",
           month_col %in% c(11:12)   ~ "P6",
         )) %>% 
  group_by(buoy_id, year, period) %>% 
  summarise(mean_diff = mean(difference, na.rm = T),
            mean_strat_index = mean(index, na.rm = T)) %>% 
  ungroup()


strat_aggregates <- full_join(yearly_strat, quarterly_strat) %>% 
  full_join(bi_monthly_strat)

####__ 5.   Combine with physical measurements  ####
buoys_out <- full_join(buoy_dataset, strat_aggregates)


####____________________________####
####  Export Out  ####
write_csv(buoys_out, 
          path = str_c(cpr_boxpath, "data/processed_data/buoys_aggregated.csv", sep = "/"), 
          col_names = TRUE)



####____________________________####
#### BONUS: map locations  ####
library(sf)
library(rnaturalearth)
library(raster)
library(metR)

#Geographic boundaries
northeast <- ne_states("united states of america") %>%
  st_as_sf() %>%
  filter(region_sub %in% c("New England", "Middle Atlantic", "South Atlantic"))
canada <- ne_states("canada") %>% st_as_sf()

# Depth contours
bathy <- raster("~/Documents/Repositories/Points_and_contours/NEShelf_Etopo1_bathy.tiff")
contours_make <- c(-50, -100, -250)
bathy_contours <- rasterToContour(bathy, levels = contours_make) %>% st_as_sf()
bathy_contours$level <- factor(bathy_contours$level, levels = as.character(contours_make))

# Buoy Locations
buoy_locations <- tribble(
  ~"Buoy", ~"lat", ~"lon", 
  "B",     43.18,  -70.42,
  "E",     43.71,  -69.35,
  "F",     44.05,  -68.99,
  "I",     44.10,  -68.10,
  "N",     42.33,  -65.90,
  "M",     43.49,  -67.87) %>% 
  mutate(label_lon = lon + .12, label_lat = lat - .14) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)



####  sf contours  ####
buoy_map <- ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  geom_sf(data = bathy_contours, aes(color = level, fill = level, linetype = level), 
          #color = "gray80", fill = "gray80",
          show.legend = FALSE) +
  geom_sf(data = buoy_locations) +
  geom_text(data = buoy_locations, aes(label_lon, label_lat, label = Buoy), colour = "black") +
  scale_color_grey(start = 0.6) +
  scale_fill_grey(start = 0.6) +
  coord_sf(xlim = c(-71.25, -65.25), ylim = c(41, 44.65), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major = element_line(colour = "transparent"))

buoy_map

# Save it
ggsave(buoy_map, 
       filename = here::here("R", "new_anom_analyses", "figures", "buoy_map.png"), device = "png")






####  metR contours  ####


# Convert raster to df
bathy_df <- as.data.frame(raster::coordinates(bathy))
bathy_df$depth <- raster::extract(bathy, bathy_df)
bathy_df$depth <- bathy_df$depth * -1
contours_make <- c(50, 100, 250)


#Now with sf objects
metR_map <- ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  geom_contour(data = bathy_df, aes(x, y, z = depth), 
               breaks = contours_make, 
               color = "gray80") +
  geom_text_contour(data = bathy_df, aes(x, y, z = depth), 
                    breaks = contours_make, 
                    color = "gray40",
                    size = 1.8,
                    min.size = 10,
                    stroke = 0.2, 
                    rotate = FALSE,
                    check_overlap = TRUE) +
  geom_sf(data = buoy_locations) +
  geom_text(data = buoy_locations, aes(label_lon, label_lat, label = Buoy), colour = "black") +
  coord_sf(xlim = c(-71.25, -65.25), ylim = c(41, 44.65), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major = element_line(colour = "transparent"))

metR_map

# Save it
ggsave(metR_map, 
       filename = here::here("R", "new_anom_analyses", "figures", "buoy_map.png"), device = "png")
