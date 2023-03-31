####
# Exploring Single Transects & Survey Coverage
# 6/2/20202
# Adam A. Kemberling
####

####  Packages  ####
library(gmRi)
library(patchwork)
library(sf)
library(raster)
library(metR)
library(here)
library(tidyverse)

#CCEL Boxpath
# ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)
ccel_boxpath <-cs_path(box_group = "ccel", subfolder = NULL)

# ggtheme
theme_set(theme_bw())

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
####  CPR Maps  ####

# Cruises by year
# cpr %>% group_by(year) %>% summarise(n_cruises = n_distinct(cruise)) %>% View("cruise counts")

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

# plotting all areas
ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  geom_sf(data = area_polygons, aes(fill = area), alpha = 0.3) +
  coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
  facet_wrap(~area)


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




####____________________________####
#### Mapping Buoy Locations  ####



#Geographic boundaries
library(rnaturalearth)
northeast <- ne_states("united states of america") %>%
  st_as_sf() #%>% filter(region_sub %in% c("New England", "Middle Atlantic", "South Atlantic"))
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

# # Save it
# ggsave(buoy_map, 
#        filename = here::here("R", "new_anom_analyses", "figures", "buoy_map.png"), device = "png")






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

# # Save this better map
# ggsave(metR_map, 
#        filename = here::here("R", "new_anom_analyses", "figures", "buoy_map.png"), device = "png")




####____________________________####
#### CPR + Buoys  ####


# So base plot with the contours
base_plot <- ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  geom_contour(data = bathy_df, aes(x, y, z = depth), 
               breaks = contours_make, 
               color = "gray80") +
  # geom_text_contour(data = bathy_df, aes(x, y, z = depth), 
  #                   breaks = contours_make, 
  #                   color = "gray40",
  #                   size = 1.8,
  #                   min.size = 10,
  #                   stroke = 0.2, 
  #                   rotate = FALSE,
  #                   check_overlap = TRUE) +
  # coord_sf(xlim = c(-71.25, -65.25), ylim = c(41, 44.65), expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major = element_line(colour = "transparent"))

base_plot

# Add Cpr Locations
cpr_sf <- cpr %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Flag if they are within the study area somehow
gom_area <- area_polygons %>% filter(area == "GOM_new") 
cpr_areas <- cpr_sf %>% st_ (gom_area)
cpr_sf$indicator <- st_within(cpr_sf, gom_area) %>% lengths > 0


# Add cpr to base
combo_plot <- base_plot +
  geom_sf(data = cpr_sf, shape = 3, size = 0.5, aes(color = indicator), show.legend = FALSE) +
  scale_color_manual(values = c("gray80", "gray20")) +
  geom_sf(data = gom_area, fill = "transparent", size = 1, color = "royalblue") +
  #geom_sf(data = buoy_locations, shape = 17, size = 2) +
  #geom_label(data = buoy_locations, aes(label_lon, label_lat, label = Buoy), colour = "black") +
  geom_label(data = buoy_locations, aes(lon, lat, label = Buoy), colour = "black") +
  coord_sf(xlim = c(-71.25, -65.25), ylim = c(41, 44.65), expand = FALSE)
  

# Show it
combo_plot

# Save this better map
ggsave(combo_plot,
       filename = here::here("R", "new_anom_analyses", "figures", "buoy_cpr_map.png"), device = "png")
