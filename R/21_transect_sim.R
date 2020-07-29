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

#CCEL Boxpath
ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)

#### Spline function  ####
source(here("R", "cpr_helper_funs.R"))


####  Data  ####
cpr <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "zooplankton_combined.csv", sep = "/"), 
                guess_max = 1e6, col_types = cols()) %>% 
  rename(lon = `longitude (degrees)`, lat = `latitude (degrees)`) %>% 
  mutate_at(c("month", "day"), factor) %>% 
  mutate(lon = ifelse(lon > 0, lon * -1, lon))

# Sf polygons
northeast <- rnaturalearth::ne_states(country = "united states of america") %>% st_as_sfc(crs = 4326)
canada <- rnaturalearth::ne_states(country = "canada") %>% st_as_sfc(crs = 4326)

####  Model Transects  ####

# Portland Transect
pmod <- cpr %>% filter(year == "2017", cruise == "477MC") 
pmod_sf <- pmod %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Boston Transect
bmod <- cpr %>% filter(year == "1990", cruise == "YC9001")
bmod_sf <- bmod %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)


ggplot() +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  geom_sf(data = pmod_sf, aes(color = "Portland Transect - 2017")) +
  geom_sf(data = bmod_sf, aes(color = "Boston Transect - 1990")) +
  coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
  theme_bw() 


####  Generating transects from scratch

# what years went to portland - in 2013 they switched, but super awkwardly
cpr %>% 
  filter(year == 2013) %>% 
  ggplot(aes(lon, lat, color = factor(month))) +
    geom_point()

# take all portland transects and get a mean/var for the start/end coordinates to set up the end points

# West End
west <- cpr %>%
  filter(year >= 2014) %>% 
  split(.$cruise) %>% 
  map(function(x) {first_station <- x %>% arrange(desc(lon)) %>% slice(1) }) %>% 
  bind_rows() %>% 
  filter(lon < -65) 

west %>% 
  ggplot(aes(lon, lat, color = factor(cruise))) +
  geom_point()

# East End
east <- cpr %>%
  filter(year >= 2014) %>% 
  split(.$cruise) %>% 
  map(function(x) {first_station <- x %>% arrange(lon) %>% slice(1) }) %>% 
  bind_rows()

east %>% 
  ggplot(aes(lon, lat, color = factor(cruise))) +
  geom_point()


# Together
ggplot() +
  geom_point(data = west, aes(lon, lat)) +
  geom_point(data = east, aes(lon, lat))


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


####  Generate Transect Function  ####
generate_transect <- function(west_center = west_summ, 
                              east_center = east_summ, 
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


# And Voila
tester <- generate_transect(n_transects = 3)

tester %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  ggplot() +
    geom_sf(aes(color = transect_id)) +
    geom_sf(data = northeast) +
    geom_sf(data = canada) +
    coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
    theme_bw()+
    labs(subtitle = "Simulated Portland Transects")



####  Next Steps  ####
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




####__2. Get SST / depth of all those points  ####

####____a. Depth  ####

# Load etopo depth raster
etopo <- raster("~/Box/RES Data/Shapefiles/NEShelf_Etopo1_bathy.tiff")

# Extract depths
bost_locations$depth <- raster::extract(etopo, bost_locations[, c("lon", "lat")])
portland_sims$depth <- raster::extract(etopo, portland_sims[, c("lon", "lat")])


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

target_dates <- gmRi::make_stack_dates(target_dates, year, month, day)







# Load OISST Data - Set Raster naming
ncpath <- "~/Box/NSF OKN Demo Data/oisst/annual_observations/"
nc_files <- list.files(ncpath)
nc_files_full <- str_c(ncpath, "/", nc_files)

#pull what the years are to name layers and for 1981 behavior
nc_years <- str_sub(nc_files, -10, -7)

# Single year behavior
year_x <- stack(nc_files_full[1])
names(year_x) %>% str_replace("X", "") %>% str_replace_all(".", "_")



















####  ncdf4 Approach  ####

library(stars)
library(ncdf4)
#library(tidync)

# Load OISST Data
ncpath <- "~/Box/NSF OKN Demo Data/oisst/annual_observations/"
nc_files <- list.files(ncpath)

# Trying different netcdf access methods to work with tidync for subsetting and indexing
# These are all slightly problematic

#oisst <- raster::stack(str_c(ncpath, "/", nc_files))
#oisst <- read_stars(str_c(ncpath, "/", nc_files), var = "sst") # Exhausts memory
#oisst <- RNetCDF::open.nc(str_c(ncpath, "/", nc_files)) # Workflow undefined
# tidync(oisst) # Does not integrate with multiple file netcdf connections



####______1. Slicing by lat/lon

# # Load netcdfs with netcdf4
# oisst <- ncdf4::nc_open(str_c(ncpath, "/", nc_files)) # Workflow undefined
# 
# # Access the lat/lon values behind the indexing
# nc_lon <- ncvar_get(oisst, "lon")
# nc_lat <- ncvar_get(oisst, "lat")
# nc_time <- ncvar_get(oisst, "time")
# 
# # Build the indexing you want to 
# lon_index <- range(bost_locations$lon) + c(-1, 1)  + rep(180, 2) # add 1 degree buffer and convert 0-360
# lat_index <- range(bost_locations$lat) + c(-1, 1) 
# 
# # Get the netcdf indices that match
# lon_key <- which(between(oisst_lon, left = lon_index[1], right = lon_index[2]))
# lat_key <- which(between(nc_lat, left = lat_index[1], right = lat_index[2]))
# 
# # And Subset
# oisst_sst <- ncvar_get(oisst, "sst")
# oisst_sst[lon_key, lat_key, 1]



####  Stars workflow  ####
# doesn't accept multiple layers
# nc_files_full <- str_c(ncpath, "/", nc_files)
# oisst <- stars::read_ncdf(nc_files_full[2]) #Workflow undefined
# oisst_lon <- st_get_dimension_values(oisst, "lon")
# oisst_lat <- st_get_dimension_values(oisst, "lat")
# oisst_time <- st_get_dimension_values(oisst, "time")
# 
# # Build the indexing from value range you want
# lon_index <- range(bost_locations$lon) + c(-1, 1)  + rep(180, 2) # add 1 degree buffer and convert 0-360
# lat_index <- range(bost_locations$lat) + c(-1, 1) 
# 
# # Get the netcdf indices that match
# lon_key <- which(between(oisst_lon, left = lon_index[1], right = lon_index[2]))
# lat_key <- which(between(oisst_lat, left = lat_index[1], right = lat_index[2]))
# 
# #Slicing and stacking
# oisst[[1]]
# oisst_slice <- oisst %>% 
#   slice("lon", lon_key) %>% 
#   slice("lat", lat_key) %>% 
#   slice("time", 1) 
# 
# ##plot(oisst_) ## gives error about unique breaks
# ## remove NAs, zeros, and give a large number
# ## of breaks (used for validating in detail)
# qu_0_omit = function(x, ..., n = 5) {
#   x = units::drop_units(na.omit(x))
#   c(0, quantile(x[x > 0], seq(0, 1, length.out = n)))
# }
# 
# plot(oisst_slice, 
#      border = NA, 
#      breaks = qu_0_omit(oisst_slice[[1]]), 
#      reset = FALSE)



####___________________####
####  Compare Sampling Environments  ####
# need to compare them in multivariate space
