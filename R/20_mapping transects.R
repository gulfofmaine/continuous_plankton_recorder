####
# Exploring Single Transects & Survey Coverage
# 6/2/20202
# Adam A. Kemberling
####

####  Packages  ####
library(gmRi)
library(broom)
library(splines)
library(mgcv)
library(tidyverse)
library(patchwork)
library(sf)
library(here)

#CCEL Boxpath
ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)

#### Spline function  ####
source(here("R", "cpr_helper_funs.R"))


####  Data  ####
cpr <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "zooplankton_combined.csv", sep = "/"), 
                guess_max = 1e6, col_types = cols()) %>% 
  rename(lon = `longitude (degrees)`, lat = `latitude (degrees)`) %>% 
  mutate_at(c("year", "month", "day"), factor) %>% 
  mutate(lon = ifelse(lon > 0, lon * -1, lon))


####  Study Area Bounding Boxes  ####

# Sf polygons
northeast <- rnaturalearth::ne_states(country = "united states of america") %>% st_as_sfc(crs = 4326)
canada <- rnaturalearth::ne_states(country = "canada") %>% st_as_sfc(crs = 4326)

area_bboxes <- tribble( #### open area bboxes  ####
                        ~"area",  ~"lon",  ~"lat",
                        
                        "GOM",   -70.000000,	42.200000,
                        "GOM",   -68.600000,	42.200000,
                        "GOM",   -68.600000,	42.400000,
                        "GOM",   -66.600000,	42.400000,
                        "GOM",   -66.600000,	43.400000,
                        "GOM",   -68.600000,	43.400000,
                        "GOM",   -68.600000,	43.200000,
                        "GOM",   -70.000000,	43.200000,
                        "GOM",   -70.000000,	42.200000,
                        
                        "GOM_new",   -70.000000,	42.200000, #bottom left
                        "GOM_new",   -66.600000,	42.200000, #bottom right
                        "GOM_new",   -66.600000,	43.800000, #top right
                        "GOM_new",   -70.000000,	43.800000, #top left
                        "GOM_new",   -70.000000,	42.200000,
                        
                        "CCB",   -70.800000,	42.200000,
                        "CCB",   -70.000000,	42.200000,
                        "CCB",   -70.000000,	42.800000,
                        "CCB",   -70.800000,	42.800000,
                        "CCB",   -70.800000,	42.200000,
                        
                        "WGOM",  -70.000000, 	42.200000,
                        "WGOM",  -68.600000, 	42.200000,
                        "WGOM",  -68.600000, 	43.200000,
                        "WGOM",  -70.000000, 	43.200000,
                        "WGOM",  -70.000000, 	42.200000,
                        
                        "EGOM",  -68.600000,	42.400000,
                        "EGOM",  -66.600000,	42.400000,
                        "EGOM",  -66.600000,	43.400000,
                        "EGOM",  -68.600000,	43.400000,
                        "EGOM",  -68.600000,	42.400000,
                        
                        "SS",    -66.600000,	42.600000,
                        "SS",    -65.400000,	42.600000,
                        "SS",    -65.400000,	43.400000,
                        "SS",    -66.600000,	43.400000,
                        "SS",    -66.600000,	42.600000,
) %>% arrange(area) #### close area bboxes  ####


#Turn them into polygons
area_polygons <- area_bboxes %>%
  split(.$area) %>% #Be careful here because split makes the list alphabetical
  map(function(x) {
    poly_out <- x %>% select(lon, lat) %>% 
      as.matrix() %>% 
      list() %>% 
      st_polygon()
    
    return(poly_out)
    
  })# %>% st_multipolygon()

#And then make the sf object  from the polygons
area_polygons <- st_sf(area = unique(area_bboxes$area), st_sfc(area_polygons), crs = 4326)



####_______________####
####  Making Maps  ####

####__Sample Coverage + Study Area  ####
t_2017 <- cpr %>% filter(year == "2017") 
t_2017_sf <- t_2017 %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)


ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  #geom_sf(data = st_as_sf(cpr, coords = c("lon", "lat"), crs = 4326)) +
  geom_sf(data = t_2017_sf) +
  geom_sf(data = filter(area_polygons, area == "GOM_new"), aes(fill = area), alpha = 0.3) +
  coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
  theme_bw() +
  guides(fill = guide_legend(title = "Gulf of Maine Study Area", 
                             label = FALSE, 
                             title.position = "left")) +
  theme(legend.position = c(0.7, 0.15),
        legend.background = element_blank()) #+ labs(caption = "Sampling Conducted in 2017 - 4 Transects")



###__Yearly Coverage  ####
ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  #geom_line(data = t_2017, aes(lon, lat, color = month)) +
  geom_sf(data = t_2017_sf, aes(color = month)) +
  coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_blank()) + 
  labs(caption = "Sampling Conducted in 2017 - 4 Transects")



####__ Single Transect  ####
t1 <- t_2017 %>% filter(cruise == "477MC")
t1_sf <- t1 %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  #geom_sf(data = t1_sf, aes(color = factor(station))) +
  geom_sf(data = filter(t1_sf, station == 13), size = 6, shape = 3) +
  coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
  theme_bw() +
  theme(legend.background = element_blank(),
        legend.title = element_blank()) +
  labs(caption = "Single Station - Station 13")
  

# Take top ten most abundant taxa for each station
t1_summs <- t1 %>% 
  pivot_longer(names_to = "taxa", values_to = "abundance", cols = 12:ncol(t1)) %>% 
  split(.$station) %>% 
  map(arrange, desc(abundance)) %>% 
  bind_rows()
  
t1_summs %>% filter(station == 13)
