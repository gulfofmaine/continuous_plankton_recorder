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

# Polygons for mapping
new_england <- rnaturalearth::ne_states("united states of america") %>% st_as_sf(crs = 4326)
canada <- rnaturalearth::ne_states("canada") %>% st_as_sf(crs = 4326)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))
ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)
oisst_path <- shared.path(os.use = "unix", group = "RES_Data", folder = "OISST/oisst_mainstays")




####__####
####  Gulf of Maine Transect  ####

# Combined dataset from NOAA/SAHFOS, concentrations in common units: # / meters cubed
# Source: 17_noaa_sahfos_eda.R
gom_abund <- read_csv(str_c(ccel_boxpath, "Data/Gulf of Maine CPR/2020_combined_data/zooplankton_combined.csv"), 
                      guess_max = 1e6, col_types = cols())

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
    year = NULL,
    `longitude (degrees)` = `longitude (degrees)`*-1)


# map mab transect
gom_sf <- gom_abund %>% 
  distinct(date, station, `longitude (degrees)`, `latitude (degrees)`) %>% 
  st_as_sf(coords = c("longitude (degrees)", "latitude (degrees)"), crs = 4326, remove = FALSE)


ggplot() +
  geom_sf(data = new_england) +
  geom_sf(data = canada) +
  geom_sf(data = gom_sf) +
  coord_sf(xlim = c(-73, -64),
           ylim = c(41.75, 45), 
           expand = F)






####  Mid Atlantic Bight Transect  ####
# Source: 01_mab_firstlook.R
mab_abund <- read_csv(str_c(ccel_boxpath, "Data/Mid Atlantic CPR/noaa_mab_cpr_long.csv"), 
                      guess_max = 1e6, col_types = cols())



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



# map mab transect
mab_sf <- mab_abund %>% 
  distinct(date, sample, longitude, latitude) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)


ggplot() +
  geom_sf(data = new_england) +
  geom_sf(data = mab_sf) +
  coord_sf(xlim = c(-76, max(mab_sf$longitude)),
           ylim = c(36, 41.5), 
           expand = T)




####  Getting Daily Averages  ####

# Make it long format for grouping
gom_long <- gom_abund %>% 
  pivot_longer(names_to = "taxa", values_to = "abundance_per_100_cubic_meters", cols = 10:ncol(.))



# make date and station key to expand taxa * station
# need to do this to not create stations that didnt exist
gom_long <- mutate(gom_long, 
                   station = str_pad(station, side = "left", width = 2, pad = "0"),
                   sta_date = str_c(station, "-", date))

glimpse(gom_long)


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




##### mid-atlantic 
# already long
glimpse(mab_abund)


# make date and station key to expand taxa * station
# need to do this to not create stations that didnt exist
mab_long <- mab_abund %>% 
  mutate(sample = str_pad(sample, side = "left", width = 2, pad = "0"),
         sta_date = str_c(sample, "-", date)) 


mab_daily <- mab_long %>% 
  expand(sta_date, taxonomic_name, life_stage) %>% 
  left_join(mab_long) %>% 
  mutate(abundance_per_100_cubic_meters = ifelse(is.na(abundance_per_100_cubic_meters), 0, abundance_per_100_cubic_meters)) %>%
  group_by(date,taxonomic_name, life_stage) %>% 
  summarise(avg_abund_per_100m = mean(abundance_per_100_cubic_meters, na.rm = T),
            area = "Mid Atlantic Bight",
            .groups = "keep")




# same units? for sure?
ggplot(mab_daily, aes(x = avg_abund_per_100m)) + geom_histogram() + labs(subtitle = "mab")
ggplot(mab_daily, aes(x = (avg_abund_per_100m^3))) + geom_histogram() + labs(subtitle = "mab cubed")
ggplot(gom_daily, aes(x = avg_abund_per_100m)) + geom_histogram() + labs(subtitle = "gom")


####  SST  ####

# Gulf of Maine SST
gom_window <- data.frame(lon = c(min(gom_abund$`longitude (degrees)`), max(gom_abund$`longitude (degrees)`)),
                         lat = c(min(gom_abund$`latitude (degrees)`), max(gom_abund$`latitude (degrees)`)),
                         time = as.Date(c("2009-01-01", "2010-12-31")))

# load sst
gom_sst <- oisst_window_load(oisst_path = oisst_path, 
                             data_window = gom_window,
                             anomalies = FALSE) %>% stack()


# Make it a table
gom_sst_df <- raster::cellStats(gom_sst, mean, na.rm = T) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  setNames(c("date", "sst")) %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "[.]", "-"),
         date = as.Date(date),
         area = "Gulf of Maine")



####  Mid Atlantic Bight SST
mab_window <- data.frame(lon = c(min(mab_sf$longitude), max(mab_sf$longitude)),
                         lat = c(min(mab_sf$latitude), max(mab_sf$latitude)),
                         time = as.Date(c("2009-01-01", "2010-12-31")))


# Load OISST for MAB
mab_oisst <- oisst_window_load(oisst_path = oisst_path, 
                               data_window = mab_window,
                               anomalies = FALSE) %>% stack()

# Make it a table
mab_sst_df <- raster::cellStats(mab_oisst, mean, na.rm = T) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "date") %>% 
  setNames(c("date", "sst")) %>% 
  mutate(date = str_remove(date, "X"),
         date = str_replace_all(date, "[.]", "-"),
         date = as.Date(date),
         area = "Mid Atlantic Bight")



####  Prepare Export  ####
write_csv(gom_daily , here::here("maria_data/gom_cpr_mean_abund100m3.csv"))
write_csv(mab_daily , here::here("maria_data/mab_cpr_mean_abund100m3.csv"))
write_csv(gom_sst_df, here::here("maria_data/gom_daily_sst.csv"))
write_csv(mab_sst_df, here::here("maria_data/mab_daily_sst.csv"))
