####  Preparing Data for Maria
#### 4/29/2021



#### Exploratory Data Analysis of NOAA+SAHFOS Anomalies
#### Anomalies Calculated using splines generated in R
#### Adam A. Kemberling
#### 2/10/2019


####  Packages  ####
library(raster)
library(here)
library(gmRi)
library(ggpmisc)
library(patchwork)
library(tidyverse)
library(sf)
library(exactextractr)


# Polygons for mapping
new_england <- rnaturalearth::ne_states("united states of america") %>% st_as_sf(crs = 4326)
canada <- rnaturalearth::ne_states("canada") %>% st_as_sf(crs = 4326)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))
ccel_boxpath <- box_path(box_group = "Climate Change Ecology Lab", subfolder = NULL)
oisst_path   <- box_path(box_group = "RES_Data", subfolder = "OISST/oisst_mainstays")




####____####
####  Gulf of Maine Transect  ####

# Combined dataset from NOAA/SAHFOS, concentrations in common units: # / meters cubed
# Source: 17_noaa_sahfos_eda.R
gom_abund <- read_csv(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/2020_combined_data/zooplankton_combined.csv"), 
                      guess_max = 1e6, 
                      col_types = cols())

# Drop a bunch of years
gom_abund <- gom_abund %>% 
  filter(year %in% c(2009,2010))

# clean up dates
gom_abund <- gom_abund %>% 
  mutate(
    month = str_pad(month, width = 2, side = "left", pad = "0"),
    day = str_pad(day, width = 2, side = "left", pad = "0"),
    date = as.Date(str_c(year, month, day, sep = "-")),
    .after = station,
    day = NULL,
    month = NULL,
    #year = NULL,
    `longitude (degrees)` = `longitude (degrees)`*-1)


##### Map GOM  ####

# Build data window for loading sst later
gom_window <- data.frame(lon = c(min(gom_abund$`longitude (degrees)`), max(gom_abund$`longitude (degrees)`)),
                         lat = c(min(gom_abund$`latitude (degrees)`), max(gom_abund$`latitude (degrees)`)),
                         time = as.Date(c("1998-01-01", "2010-12-31")))


# build sf for area
gom_window_sf <- gom_window %>% 
  expand(lon, lat) %>% 
  left_join(gom_window) %>% 
  mutate(area = "GOM Area") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_cast("POLYGON") %>% 
  st_convex_hull()

# sf for GOM points
gom_sf <- gom_abund %>% 
  distinct(date, station, `longitude (degrees)`, `latitude (degrees)`) %>% 
  st_as_sf(coords = c("longitude (degrees)", "latitude (degrees)"), crs = 4326, remove = FALSE)


# Map for GOM region
gom_map <- ggplot() +
  geom_sf(data = new_england) +
  geom_sf(data = canada) +
  geom_sf(data = gom_sf) +
  geom_sf(data = gom_window_sf, aes(fill = area), alpha = 0.3) +
  coord_sf(xlim = c(-73, -64),
           ylim = c(41.75, 45),
           expand = F)
gom_map


##### Gulf of Maine Daily  ####

# Make it long format for grouping
gom_long <- gom_abund %>% 
  pivot_longer(names_to = "taxa", values_to = "abundance_per_100_cubic_meters", cols = 10:ncol(.))



# make date and station key to expand taxa * station
# need to do this to not create stations that didnt exist
gom_long <- mutate(gom_long, 
                   station = str_pad(station, side = "left", width = 2, pad = "0"),
                   sta_date = str_c(station, "-", date))



# Get average abundance
gom_daily <- gom_long %>%
  expand(sta_date, taxa) %>% 
  left_join(gom_long) %>% 
  mutate(abundance_per_100_cubic_meters = ifelse(is.na(abundance_per_100_cubic_meters), 0, abundance_per_100_cubic_meters)) %>% 
  group_by(date, taxa) %>% 
  summarise(avg_abund_per_100m = mean(abundance_per_100_cubic_meters, na.rm = T),
            area = "Gulf of Maine",
            .groups = "keep") %>% 
  ungroup()






####  Mid Atlantic Bight Transect  ####
# Source: 01_mab_firstlook.R
mab_abund <- read_csv(str_c(ccel_boxpath, "Data/Mid Atlantic CPR/noaa_mab_cpr_long.csv"), 
                      guess_max = 1e6, 
                      col_types = cols())



# Drop a bunch of years
mab_abund <- mab_abund %>% 
  filter(year %in% c(2009,2010))


# clean up dates
mab_abund <- mab_abund %>% 
  mutate(
    month = str_pad(month, width = 2, side = "left", pad = "0"),
    day = str_pad(day, width = 2, side = "left", pad = "0"),
    date = as.Date(str_c(year, month, day, sep = "-")),
    .after = sample,
    day = NULL,
    month = NULL,
    year = NULL)





#####  Map Mid-Atlantic  ####

# map mab transect
mab_sf <- mab_abund %>% 
  distinct(date, sample, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)




# Region Assignment
# Source Andy Pershing
# Reference Pershing et al. 2010
mab_slope <- tribble(
    ~"longitude",       ~"latitude",
    -71.07764473940071, 36.98990872585177,
    -70.37294964932077, 37.52560031966651,
    -69.45253157248167, 38.43942715617401,
    -71.437183050666,   39.4320321682425,
    -71.68166910232638, 39.17994200644733,
    -72.19940427054837, 38.89634057442776,
    -72.5014164520112,  38.75453985841798) %>% 
  mutate(area = "MAB Slope") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_cast("POLYGON") %>% 
  st_convex_hull()

mab_shelf <- tribble(
    ~"longitude",       ~"latitude",
    -72.83219169837525, 39.227208911783926,
    -72.74590250367159, 39.40052089801811,
    -72.6452317765173,  39.4792990735791,
    -72.54456104936304, 39.699877965149874,
    -72.18502273809776, 39.95196812694505,
    -73.47936065865275, 40.47190408564759,
    -73.91080663217107, 40.47190408564759,
    -73.85328050236863, 40.15679138340362) %>% 
  mutate(area = "MAB Shelf") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_cast("POLYGON") %>% 
  st_convex_hull()


# Plot MAB
mab_plot <- ggplot() +
  geom_sf(data = mab_sf) +
  geom_sf(data = mab_shelf, aes(fill = "MAB Shelf"), alpha = 0.3) +
  geom_sf(data = mab_slope, aes(fill = "MAB Slope"), alpha = 0.3) + 
  geom_sf(data = new_england) +
  coord_sf(xlim = c(-76,-69.5),
           ylim = c(36, 41.5), 
           expand = T)
mab_plot



####  Map Both  ####
ggplot() +
  geom_sf(data = mab_sf, pch = 3, aes(color = "Gulf of Maine Transect"), alpha = 0.4) +
  geom_sf(data = gom_sf, pch = 3, aes(color = "Mid-Atlantic Transect"), alpha = 0.4) +
  geom_sf(data = canada) +
  geom_sf(data = new_england) +
  scale_color_gmri() +
  theme_bw() +
  labs(color = "") +
  coord_sf(xlim = c(-76,-64),
           ylim = c(36, 45), 
           expand = F)



##### Region Assignment  ####


# Use st_join to get area overlay
mab_sf_shelf <- mab_sf %>% 
  st_join(mab_shelf, join = st_intersects) %>% 
  drop_na(area) %>% 
  select(-geometry) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

mab_sf_slope <- mab_sf %>% 
  st_join(mab_slope, join = st_intersects) %>% 
  drop_na(area)  %>% 
  select(-geometry) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# map to verify
ggplot() +
  geom_sf(data = mab_sf_shelf, aes(color = area)) +
  geom_sf(data = mab_shelf, aes(fill = area), alpha = 0.3) +
  geom_sf(data = mab_sf_slope, aes(color = area)) +
  geom_sf(data = mab_slope, aes(fill = area), alpha = 0.3) + 
  geom_sf(data = new_england) +
  coord_sf(xlim = c(-76,-69.5),
           ylim = c(36, 41.5), 
           expand = T)




##### Mid-Atlantic Daily  ####
# already long
glimpse(mab_sf_shelf)
mab_points_to_daily <- function(mab_region_sf){
  
  # make date and station key to expand taxa * station
  # need to do this to not create stations that didnt exist
  mab_long <- mab_region_sf %>%
    st_drop_geometry() %>% 
    left_join(mab_abund, by = c("sample", "date")) %>% 
    mutate(sample = str_pad(sample, side = "left", width = 2, pad = "0"),
           sta_date = str_c(sample, "-", date)) 
  
  # don't need to expand because zeros are added back in 01_mab_firstlook.R
  mab_daily <- mab_long %>% 
    mutate(abundance_per_100_cubic_meters = ifelse(is.na(abundance_per_100_cubic_meters), 0, abundance_per_100_cubic_meters)) %>%
    group_by(area, date, taxonomic_name, life_stage) %>% 
    summarise(avg_abund_per_100m = mean(abundance_per_100_cubic_meters, na.rm = T),
              .groups = "keep")
  
  # return the daily data
  return(mab_daily)
  
}

# Use function to build daily averages
mab_shelf_daily <- mab_points_to_daily(mab_sf_shelf)
mab_slope_daily <- mab_points_to_daily(mab_sf_slope)






# same units? for sure?
# ggplot(mab_daily, aes(x = avg_abund_per_100m)) + geom_histogram() + labs(subtitle = "mab")
ggplot(mab_shelf_daily, aes(x = avg_abund_per_100m)) + geom_histogram() + labs(subtitle = "mab shelf")
ggplot(mab_slope_daily, aes(x = avg_abund_per_100m)) + geom_histogram() + labs(subtitle = "mab slope")
ggplot(gom_daily, aes(x = avg_abund_per_100m)) + geom_histogram() + labs(subtitle = "gom")





####  Daily CPR Exports  ####
# write_csv(gom_daily , here::here("maria_data/gom_cpr_mean_abund100m3.csv"))
# write_csv(mab_shelf_daily , here::here("maria_data/mab_shelf_mean_abund100m3.csv"))
# write_csv(mab_slope_daily , here::here("maria_data/mab_slope_mean_abund100m3.csv"))






####_____####
####  Prepare Regional SST  ####

##### Gulf of Maine SST  ####


# load sst
gom_sst <- oisst_window_load(oisst_path = oisst_path, 
                             data_window = gom_window,
                             anomalies = FALSE) %>% stack()


# Crop to specific dimensions
gom_sst_df <- exact_extract(gom_sst, gom_window_sf, "mean") %>% 
  t() %>%  
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  rename(sst = V1) %>% 
  mutate(date = str_sub(date, 7, -1),
         date = str_replace_all(date, "[.]", "-"),
         date = as.Date(date),
         area = "Gulf of Maine")




#####  Mid Atlantic Bight Overall  ####
mab_window <- data.frame(lon = c(min(mab_sf$longitude), max(mab_sf$longitude)),
                         lat = c(min(mab_sf$latitude), max(mab_sf$latitude)),
                         time = as.Date(c("1998-01-01", "2010-12-31")))


# Load OISST for MAB
mab_oisst <- oisst_window_load(oisst_path = oisst_path, 
                               data_window = mab_window,
                               anomalies = FALSE) %>% stack()

# Make it a table - overall MAB
mab_sst_df <- raster::cellStats(mab_oisst, mean, na.rm = T) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  setNames(c("date", "sst")) %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "[.]", "-"),
         date = as.Date(date),
         area = "Mid Atlantic Bight")









#####  MAB OISST  Extraction  ####



####  Mask the MAB Slope and Shelf

# Calculate vector of daily mean temps for the areas

# MAB Shelf
mab_shelf_sst_df <- exact_extract(mab_oisst, mab_shelf, "mean") %>% 
  t() %>%  
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  rename(sst = V1) %>% 
  mutate(date = str_sub(date, 7, -1),
         date = str_replace_all(date, "[.]", "-"),
         date = as.Date(date),
         area = "MAB Shelf")

# MAB Slope
mab_slope_sst_df <- exact_extract(mab_oisst, mab_slope, "mean")  %>% 
  t() %>%  
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  rename(sst = V1) %>% 
  mutate(date = str_sub(date, 7, -1),
         date = str_replace_all(date, "[.]", "-"),
         date = as.Date(date),
         area = "MAB Slope")



#####  Regional SST Exports  ####
# write_csv(gom_sst_df, here::here("maria_data/gom_daily_sst.csv"))
# write_csv(mab_sst_df, here::here("maria_data/mab_daily_sst.csv"))
# write_csv(mab_slope_sst_df, here::here("maria_data/mab_slope_daily_sst.csv"))
# write_csv(mab_shelf_sst_df, here::here("maria_data/mab_shelf_daily_sst.csv"))
