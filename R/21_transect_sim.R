####
# Exploring Differences in Boston and Portland Transects through simulation
# 6/5/20202
# Adam A. Kemberling
####

####  Packages  ####
library(gmRi)
library(raster)
library(patchwork)
library(sf)
library(here)
library(tidyverse)
library(gganimate)


#CCEL Boxpath
# ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)
ccel_boxpath <-cs_path(box_group = "ccel", subfolder = NULL)


# #### Spline function  ####
# source(here("R", "cpr_helper_funs.R"))


####  1. Data  ####
cpr <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "zooplankton_combined.csv", sep = "/"), 
                guess_max = 1e6, col_types = cols()) %>% 
  rename(lon = `longitude (degrees)`, lat = `latitude (degrees)`) %>% 
  mutate_at(c("month", "day"), factor) %>% 
  mutate(lon = ifelse(lon > 0, lon * -1, lon))

# Sf polygons
northeast <- rnaturalearth::ne_states(country = "united states of america") %>% st_as_sfc(crs = 4326)
canada <- rnaturalearth::ne_states(country = "canada") %>% st_as_sfc(crs = 4326)

####  2. Different Transects  ####

# what years went to portland - in 2013 they switched, but super awkwardly
cpr %>% 
  filter(year == 2013) %>% 
  ggplot(aes(lon, lat, color = factor(month))) +
  geom_point()

# Portland Transect - begins 2013
pmod <- cpr %>% filter(year == "2013")#, cruise == "477MC") 
pmod_sf <- pmod %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Boston Transect
bmod <- cpr %>% filter(year == "1990", cruise == "YC9001")
bmod_sf <- bmod %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)



####  3. Map the Port Transition  ####
ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  geom_sf(data = pmod_sf, aes(color = "Portland Transect - Mid-2013 Switch")) +
  geom_sf(data = bmod_sf, aes(color = "Boston Transect - 1960")) +
  coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
  theme_bw() + 
  labs(color = "Transect Routes", 
       subtitle = "CPR Survey's Relocation to Portland")






####  4. Transect Endpoints  ####

# Process:
# take all portland transects and get a mean/var for the start/end coordinates to set up the end points
# Do the same for boston transect

# West End
west <- cpr %>%
  filter(year >= 2014) %>% 
  split(.$cruise) %>% 
  map(function(x) {first_station <- x %>% arrange(lon) %>% slice(1) }) %>% 
  bind_rows() %>% 
  filter(lon < -65) 

# East End
east <- cpr %>%
  filter(year >= 2014) %>% 
  split(.$cruise) %>% 
  map(function(x) {first_station <- x %>% arrange(desc(lon)) %>% slice(1) }) %>% 
  bind_rows()

# Save their summary stats
west_summ <- west %>% summarise(
  lon_mu = mean(lon),
  lon_sd = sd(lon),
  lat_mu = mean(lat),
  lat_sd = sd(lat))

east_summ <- east %>% summarise(
  lon_mu = mean(lon),
  lon_sd = sd(lon),
  lat_mu = mean(lat),
  lat_sd = sd(lat))


# Together
ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  geom_point(data = west, aes(lon, lat)) +
  geom_point(data = east, aes(lon, lat)) +
  geom_point(data = west_summ, aes(lon_mu, lat_mu, color = "Western Endpoint Avg."), size = 3) +
  geom_point(data = east_summ, aes(lon_mu, lat_mu, color = "Eastern Endpoint Avg."), size = 3) +
  coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
  labs(subtitle = "Portland Transect Endpoint Centroids")






####______________________####


####  Simulation Functions  ####

####  Generate Endpoint Function  ####
endpoint_means <- function(cpr_data){
  
  # West End
  west <- cpr_data %>%
    split(.$cruise) %>% 
    map(function(x) {first_station <- x %>% arrange(lon) %>% slice(1) }) %>% 
    bind_rows() %>% 
    filter(lon < -65) 
  
  # East End
  east <- cpr_data %>%
    split(.$cruise) %>% 
    map(function(x) {first_station <- x %>% arrange(desc(lon)) %>% slice(1) }) %>% 
    bind_rows()  %>% 
    filter(lon < -65) 
 
  # Save their summary stats
  west_summ <- west %>% summarise(
    lon_mu = mean(lon),
    lon_sd = sd(lon),
    lat_mu = mean(lat),
    lat_sd = sd(lat))
  
  east_summ <- east %>% summarise(
    lon_mu = mean(lon),
    lon_sd = sd(lon),
    lat_mu = mean(lat),
    lat_sd = sd(lat))
  
  return(list(
    "west_end" = west_summ,
    "east_end" = east_summ
  ))
   
}


####  Generate Portland/Boston Endpoints
port_endpoints <- endpoint_means(cpr_data = filter(cpr, year >= 2014))
bost_endpoints <- endpoint_means(cpr_data = filter(cpr, year <= 2012))





####  Generate Transect Function  ####
generate_transect <- function(west_center = port_endpoints$west_end, 
                              east_center = port_endpoints$east_end, 
                              n_stations = 10, 
                              n_transects = 1){
  
  
  # Make list of appropriate length for n_transects
  transect_list <- vector(length = n_transects, mode = "list")
  
  # Use map() to generate transects
  transect_data <- map(transect_list, function(x){
    # Generate Random ends from mean and variance of existing transects
    west_lon <- rnorm(1, mean = west_center$lon_mu, sd = west_center$lon_sd)
    west_lat <- rnorm(1, mean = west_center$lat_mu, sd = west_center$lat_sd)
    east_lon <- rnorm(1, mean = east_center$lon_mu, sd = east_center$lon_sd)
    east_lat <- rnorm(1, mean = east_center$lat_mu, sd = east_center$lat_sd)
    
    # Generate 10 stations along the transect
    transect_coords <- data.frame(
      lon = seq(west_lon, east_lon, length.out = 10),
      lat = seq(west_lat, east_lat, length.out = 10)
    )
    return(transect_coords)
    
  }) %>% bind_rows(.id = "transect_id")
  
  return(transect_data)
 
}


# And Voila!

# Simulate Portland Transects
portland_tester <- generate_transect(west_center = port_endpoints$west_end, 
                                     east_center = port_endpoints$east_end, 
                                     n_stations = 10,
                                     n_transects = 4) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 


# Simulate some Boston Transects
boston_tester <- generate_transect(west_center = bost_endpoints$west_end, 
                                   east_center = bost_endpoints$east_end, 
                                   n_stations = 10,
                                   n_transects = 4) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 



# Map simulations
ggplot() +
    geom_sf(data = portland_tester, aes(shape = transect_id, color = "Simulated Portland Transects")) +
    geom_sf(data = boston_tester, aes(shape = transect_id, color = "Simulated Boston Transects")) +
    geom_sf(data = northeast) +
    geom_sf(data = canada) +
    coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
    theme_bw()+
    labs(subtitle = "Transect Simulation", color = "Transect Origins", shape = "Transect ID#'s")







#####__________________####

####  Simating Data  ####
####__1. Simulate Transects  ####

# Add sample_date column
cpr_dated <- mutate(cpr, sample_date = as.Date(str_c(year, month, day, sep = "-")))

####____a. Boston  ####

# Number of cruises in a year
bost_counts <- cpr_dated %>% 
  filter(year < 2013) %>% 
  group_by(year) %>% 
  summarise(n_cruises = n_distinct(cruise))

# The day each cruise was sampled
bost_dates <- cpr_dated %>% 
  filter(year < 2013) %>% 
  group_by(year, cruise) %>% 
  summarise(start_date = first(sample_date))

# The locations of each sample
bost_locations <- cpr %>% 
  filter(year < 2013) %>% 
  select(1:10)

# # Average transect time-period ~ 1 day
# cpr_dated %>% 
#   filter(year < 2013) %>% 
#   group_by(cruise) %>% 
#   arrange(sample_date) %>% 
#   slice(1) %>% 
#   select(`Data Source`, cruise, station, start_date = sample_date) %>% 
#   left_join(cpr_dated) %>% 
#   filter(year < 2013) %>% 
#   group_by(cruise) %>% 
#   arrange(desc(sample_date)) %>% 
#   slice(1) %>% 
#   select(`Data Source`, cruise, station, start_date, end_date = sample_date) %>% 
#   mutate(sample_period = end_date - start_date) %>% 
#   ungroup() %>% 
#   count(sample_period)

# Hooray, they all occur on the same day
  

####____b. Portland  ####
  
#  Simulate portland transects          
portland_sims <- map(bost_counts$n_cruises, function(x){transects <- generate_transect(n_transects = x)}) %>% 
  setNames(bost_counts$year) %>% 
  bind_rows(.id = "year") %>% 
  mutate(cruise_id = transect_id,
         transect_id = str_c(year, "-", cruise_id),
         sample_date = factor(transect_id, labels = bost_dates$start_date))




#### Get Oceanographic DataSST / depth of all those points  ####

# Chlorophyll-a
# https://coastwatch.noaa.gov/cw/satellite-data-products/ocean-color/science-quality/viirs-3-sensor-gap-filled-chlorophyll-dineof.html

# Depth

# SST

# Ocean Color

####____a. Depth  ####

# Load etopo depth raster
res_path <- cs_path(box_group = "res")
etopo <- raster(str_c(res_path, "Shapefiles/NEShelf_Etopo1_bathy.tiff"))

# Extract depths
bost_locations$depth <- raster::extract(etopo, bost_locations[, c("lon", "lat")])
portland_sims$depth <- raster::extract(etopo, portland_sims[, c("lon", "lat")])






# Map the depths at each point?







####____b. SST  ####

####  Raster approach  ####

# pull the dates we need from each year
target_dates <- str_split(portland_sims$sample_date, "-") %>% 
  setNames(portland_sims$sample_date) %>% 
  dplyr::bind_cols() %>%
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "sample_date") %>% 
  setNames(c("sample_date", "year", "month", "day"))

# Make them valid raster dates
target_dates <- gmRi::make_stack_dates(target_dates, year, month, day)






# Load OISST Data


# Area to load sst for
sst_window <- data.frame(
  lon = c(-72, -65),
  lat = c(41.75,44),
  time = as.Date(c("2016-08-01", "2020-12-31")))


# load sst from GMRI cloudstorage
oisst_window_load(
  data_window = sst_window, 
  anomalies = FALSE, 
  box_location = "cloudstorage")






####  Bottom Temperature & Salinity

# SODA Reanalysis Data






####___________________####
####  Compare Sampling Environments  ####
# need to compare them in multivariate space
