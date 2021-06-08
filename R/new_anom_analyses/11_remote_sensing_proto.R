#####  Remote Sensing Model Prototyping  ####
# goal: Predict: calanus/percent diapausers/ diversity
# Use remote sensed things: OISST, chl-a, sea surface salinity
# use RNN or GAM or both.


####  Packages  ####
library(ncdf4)
library(raster)
library(gmRi)
library(tidyverse)
library(sf)
library(here)

####_______________________________####
####__________  Load Data  ___________####

# Path shortcuts

# Path to gmri OKN datasets
okn <- gmRi::shared.path(group = "NSF OKN", folder  = "")

# Path to climate change eccology lab
ccel_boxpath <- gmRi::shared.path(group = "Climate Change Ecology Lab", folder = "Data")

####  CPR Data  ####

# 1. CPR combined, un-transformed concentrations
# Combined dataset from NOAA/SAHFOS, concentrations in common units: # / meters cubed
cpr <- read_csv(str_c(ccel_boxpath, "Gulf of Maine CPR", "2020_combined_data", "zooplankton_combined.csv", sep = "/"), 
                guess_max = 1e6, col_types = cols())

cpr <- cpr %>% 
  mutate(station_date = as.Date(str_c(year, month, day, sep = "-")),
         `longitude (degrees)` = ifelse(`longitude (degrees)` > 0, `longitude (degrees)` * -1, `longitude (degrees)`)) %>% 
  select(`Data Source`, cruise, station, station_date, 
         year, month, day, hour, minute, lat = `latitude (degrees)`, 
         lon = `longitude (degrees)`, `phytoplankton color index`, everything())


###  Remotely Sensed Data  ####

 
#####_ 1. ERSST#### 
# 1. ERSST - starting point
# ersst_path <- str_c(okn, "ersst/ERSSTv5_merged.nc")
# lon_min <- min(-70) + 180 # rotation to 0-360
# lon_max <- max(-54) + 180 # rotation to 0-360
# lat_min <- min(37)
# lat_max <- max(44)
# time_min <- min(cpr$station_date)
# time_max <- max(cpr$station_date)
# my_nc <- nc_open(ersst_path)
# lon_idx  <- which( my_nc$dim$lon$vals > lon_min & my_nc$dim$lon$vals < lon_max)
# lat_idx  <- which( my_nc$dim$lat$vals > lat_min & my_nc$dim$lat$vals < lat_max)
# time_idx <- which( 
#   as.Date(my_nc$dim$time$vals, origin='1800-01-01', tz= "GMT") > time_min & 
#   as.Date(my_nc$dim$time$vals, origin='1800-01-01', tz= "GMT") < time_max)
# 
# # Pull ersst data you need
# ersst <- ncvar_get(nc = my_nc, varid = "sst")[lon_idx, lat_idx, time_idx]
# 
# # Get the dates that correspond in the least streamlined way possible
# dates <- as.Date(my_nc$dim$time$vals[time_idx], origin='1800-01-01', tz = "GMT")
# 
# # stack them up
# ersst_stack <- map(c(1:length(dates)), function(index){
#   
#   
#   # take the single time slice
#   single_date <- ersst[ , , index]
#   
#   # Change the dimension names of our matrix to "lon" and "lat",
#   # and the row and column names to the latitude and longitude values.
#   dimnames(single_date) <- list(lon = my_nc$dim$lon$vals[lon_idx],
#                                 lat = my_nc$dim$lat$vals[lat_idx])
#   single_date <- t(single_date)
#   single_date <- as.table(single_date)
#   single_date <- as.data.frame(single_date) %>% setNames(c("lat", "lon", "sst"))
#   
#   return(single_date)
#   
# }) %>% 
#   setNames(dates) %>% 
#   bind_rows(.id = "date")
# 
# 
# ersst_stack %>% filter(date == "2017-04-15") %>% 
#   ggplot(aes(lon, lat, fill = sst)) +
#   geom_tile()
# 
# 
# # close netcdf
# nc_close(my_nc)
#####_####






####_ 2. OISST  ####

# OISST
oisst_path <- list.files(str_c(okn, "oisst/annual_observations/"))
oisst_paths <- str_c(okn, "oisst/annual_observations/", oisst_path)
oisst_paths <- oisst_paths[!str_detect(oisst_paths, ".zarr")] # Paths  
oisst_years <- str_sub(oisst_paths, -10, -7)                  # Labels
oisst_paths <- setNames(oisst_paths, oisst_years)

# # try stack to validate data exists
# oisst_test <- stack(oisst_paths[2])
# plot(oisst_test$X1982.01.01)

###  Set limits based on cpr extent
lon_min <- floor(min(cpr$lon)) + 360 # rotation to 0-360
lon_max <- ceiling(max(cpr$lon)) + 360 # rotation to 0-360
lat_min <- floor(min(cpr$lat))
lat_max <- ceiling(max(cpr$lat))
time_min <- min(cpr$station_date)
time_max <- max(cpr$station_date)



####____ a.   As Rasters  ####
oisst_ras_list <- map(oisst_paths, function(nc_year){
  
  # Open connection, get subsetting indices from limits
  my_nc <- nc_open(nc_year)
  nc_year_label <- str_sub(nc_year, -10, -7)
  
  # Tester
  #my_nc <- nc_open(oisst_paths["2018"]) ; nc_year_label <- "2018"
  
  # Subset to area and times of interest
  lon_idx  <- which( my_nc$dim$lon$vals > lon_min & my_nc$dim$lon$vals < lon_max)
  lat_idx  <- which( my_nc$dim$lat$vals > lat_min & my_nc$dim$lat$vals < lat_max)
  time_idx <- which( 
    as.Date(my_nc$dim$time$vals, origin='1800-01-01', tz = "GMT") > time_min & 
    as.Date(my_nc$dim$time$vals, origin='1800-01-01', tz = "GMT") < time_max)
  
  if (length(time_idx) < 1) {
    message(paste0(nc_year_label, " outside data range."))
    return("Year outside time extent of data")}
  
  # Pull netcdf data you need
  nc_data <- ncdf4::ncvar_get(nc = my_nc, varid = "sst")[lon_idx, lat_idx, time_idx]
  
  # Make raster Stack from subset array
  
  #Get lon/lat/time dimensions
  xvals <- my_nc$dim$lon$vals[lon_idx] - 360
  yvals <- my_nc$dim$lat$vals[lat_idx]
  time_dims <- 1:dim(nc_data)[3]
  
  # Get the dates that correspond in the least streamlined way possible, for naming purposes
  dates <- as.Date(my_nc$dim$time$vals[time_idx], origin = '1800-01-01', tz = "GMT")
  
  
  # Convert Each day to a raster, rotate, and stack
  nc_stack <- map(time_dims, function(time_index){
    single_date <- nc_data[, , time_index]
    dimnames(single_date) <- list(lon = my_nc$dim$lon$vals[lon_idx],
                                  lat = my_nc$dim$lat$vals[lat_idx])
    single_date <- t(single_date)          # transpose
    
    # make/configure raster
    ras <- raster::raster(single_date,
                          crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                          xmn = min(xvals),
                          xmx = max(xvals),
                          ymn = min(yvals),
                          ymx = max(yvals))
    ras <- flip(ras, 2)
    return(ras)
  }) %>% 
    stack() %>% 
    setNames(dates)
    #setNames(time_dims)
  
  # Progress message
  message(paste0(nc_year_label, " done"))
  
  # Return the raster stack for the year
  return(nc_stack)
  
})

# Drop list elements that are outside date range
#oisst_ras_list <- oisst_ras_list %>%  
oisst_ras_list <- oisst_ras_list[ which(names(oisst_ras_list) %in% c("1981":"2017")) ]
 

# And plot tester
plot(oisst_ras_list$`1982`$X1982.01.01, main = "OISST - 1/1/1982", 
     col = rev(RColorBrewer::brewer.pal(n = 10, name = "RdBu")))

# # Put in single stack (might be easier to index as a list)
# oisst_stack <- stack(oisst_ras_list)


####____ b. As Table  ####
  
# oisst_list <- map(oisst_paths, function(nc_year){
#   
#   # Open connection, get subsetting indices from limits
#   my_nc <- nc_open(nc_year)
#   #my_nc <- nc_open(oisst_paths[2])
#   
#   # Subset to area and times of interest
#   lon_idx  <- which( my_nc$dim$lon$vals > lon_min & my_nc$dim$lon$vals < lon_max)
#   lat_idx  <- which( my_nc$dim$lat$vals > lat_min & my_nc$dim$lat$vals < lat_max)
#   time_idx <- which( 
#     as.Date(my_nc$dim$time$vals, origin='1800-01-01', tz = "GMT") > time_min & 
#       as.Date(my_nc$dim$time$vals, origin='1800-01-01', tz = "GMT") < time_max)
#   
#   # Pull netcdf data you need
#   nc_data <- ncdf4::ncvar_get(nc = my_nc, varid = "sst")[lon_idx, lat_idx, time_idx]
#   
#   
#   # Get the dates that correspond in the least streamlined way possible
#   dates <- as.Date(my_nc$dim$time$vals[time_idx], origin = '1800-01-01', tz = "GMT")
#   
#   ####  convert Each date to a dataframe
#   data_stack <- map(c(1:length(dates)), function(index){
#     
#     # take the single time slice
#     single_date <- nc_data[ , , index]
#     
#     # Change the dimension names of our matrix to "lon" and "lat",
#     # and the row and column names to the latitude and longitude values.
#     dimnames(single_date) <- list(lon = my_nc$dim$lon$vals[lon_idx],
#                                   lat = my_nc$dim$lat$vals[lat_idx])
#     single_date <- t(single_date)          # transpose
#     single_date <- as.table(single_date)   # convert to table
#     single_date <- as.data.frame(single_date) # dataframe
#     single_date <- setNames(single_date, c("lat", "lon", "sst")) # set names
#     
#     return(single_date)
#     
#   }) 
#   
#   # add the names and bind them
#   data_stack <- data_stack %>% 
#     setNames(dates) %>% 
#     bind_rows(.id = "date")
#   
#   # close connection
#   nc_close(my_nc)
#   
#   #return that table for the year
#   return(data_stack)
#   
# }) %>% setNames(oisst_years)
# 
# # Finally, append the years together, readjust longitude
# oisst_full <- bind_rows(oisst_list, .id = "year")
# oisst_full <- mutate(oisst_full, 
#                      lat = as.numeric(as.character(lat)),
#                      lon = as.numeric(as.character(lon)),
#                      lon = lon - 360)


# #plot an annual average
# oisst_full %>% 
#   mutate(month = lubridate::month(date)) %>% 
#   group_by(year, month, lon, lat) %>% 
#   summarise(mean_sst = mean(sst, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   filter(year == "2000", month == "7") %>% 
#   ggplot(aes(lon, lat, fill = mean_sst)) +
#   geom_tile() +
#   labs(caption = "OISST July 2000", x = NULL, y = NULL)


####_####
####_ 3. MODIS   ####
# Modis has datetime instead of just date for its time index so we'll need to match that
# format them as posixct


# MODIS
modis_path <- list.files(str_c(okn, "modis/"))
modis_paths <- str_c(okn, "modis/", modis_path)
modis_paths <- modis_paths[!str_detect(modis_paths, "2017")]
modis_years <- str_sub(modis_paths, -7, -4)


# Set limits
lon_min <- floor(min(cpr$lon)) 
lon_max <- ceiling(max(cpr$lon)) 
lat_min <- floor(min(cpr$lat))
lat_max <- ceiling(max(cpr$lat))
time_min <- min(cpr$station_date)
time_max <- max(cpr$station_date)
time_min <- as.POSIXct(time_min, tz = "GMT")
time_max <- as.POSIXct(time_max, tz = "GMT")


#####____ a. As Rasters  ####

modis_ras_list <- map(modis_paths, function(nc_year){
  
  # Open connection, get subsetting indices from limits
  my_nc <- nc_open(nc_year)
  nc_year_label <- str_sub(modis_paths, -7, -4)
  
  # tester
  # my_nc <- nc_open(modis_paths[3])
  # nc_year_label <- 2005
  
  # Subset to area and times of interest
  lon_idx  <- which( my_nc$dim$lon$vals > lon_min & my_nc$dim$lon$vals < lon_max)
  lat_idx  <- which( my_nc$dim$lat$vals > lat_min & my_nc$dim$lat$vals < lat_max)
  time_idx <- which( 
    as.POSIXct(my_nc$dim$time$vals, origin='1970-01-01', tz = "GMT") > time_min & 
      as.POSIXct(my_nc$dim$time$vals, origin='1970-01-01', tz = "GMT") < time_max)
  
  # Pull netcdf data you need
  nc_data <- ncvar_get(nc = my_nc, varid = "chlorophyll")[lon_idx, lat_idx, time_idx]
  
  # Get the dates that correspond in the least streamlined way possible
  dates <- as.POSIXct(my_nc$dim$time$vals[time_idx], origin = '1970-01-01', tz = "GMT")
  dates <- as.Date(dates)
  
  
  # Make raster Stack from subset array
  
  #Get lon/lat/time dimensions
  xvals <- my_nc$dim$lon$vals[lon_idx]
  yvals <- my_nc$dim$lat$vals[lat_idx]
  time_dims <- 1:dim(nc_data)[3]
  
  # Convert Each day to a raster, rotate, and stack
  nc_stack <- map(time_dims, function(time_index){
    
    single_date <- nc_data[, , time_index]
    dimnames(single_date) <- list(lon = my_nc$dim$lon$vals[lon_idx],
                                  lat = my_nc$dim$lat$vals[lat_idx])
    single_date <- t(single_date)          # transpose
    
    # make/configure raster
    ras <- raster::raster(single_date,
                          crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                          xmn = min(xvals),
                          xmx = max(xvals),
                          ymn = min(yvals),
                          ymx = max(yvals))
    #ras <- flip(ras, 2)
  }) %>% 
    stack() %>% 
    setNames(dates) #%>% 
    #setNames(time_dims)  
    
  
  
  # Return the raster stack for the year
  return(nc_stack)
  
  # Set names for each annual stack
}) %>%  setNames(modis_years)


# And plot
plot(modis_ras_list$`2003`$X2003.02.16, main = "MODIS - 2/16/2003", 
     col = rev(RColorBrewer::brewer.pal(n = 8, name = "GnBu")))

# # put in a single stack if you want.
# modis_stack <- stack(modis_ras_list)









####____ b. As Table  ####
modis_list <- map(modis_paths, function(nc_year){
  
  # Open connection, get subsetting indices from limits
  #my_nc <- nc_open(nc_year)
  my_nc <- nc_open(modis_paths[1])
  
  # Subset to area and times of interest
  lon_idx  <- which( my_nc$dim$lon$vals > lon_min & my_nc$dim$lon$vals < lon_max)
  lat_idx  <- which( my_nc$dim$lat$vals > lat_min & my_nc$dim$lat$vals < lat_max)
  time_idx <- which( 
    as.POSIXct(my_nc$dim$time$vals, origin='1970-01-01', tz = "GMT") > time_min & 
    as.POSIXct(my_nc$dim$time$vals, origin='1970-01-01', tz = "GMT") < time_max)
  
  # Pull netcdf data you need
  nc_data <- ncvar_get(nc = my_nc, varid = "chlorophyll")[lon_idx, lat_idx, time_idx]
  
  
  # Get the dates that correspond in the least streamlined way possible
  dates <- as.POSIXct(my_nc$dim$time$vals[time_idx], origin = '1970-01-01', tz = "GMT")
  dates <- as.Date(dates)
  
  ####  convert Each date to a dataframe
  data_stack <- map(c(1:length(dates)), function(index){
    
    
    # take the single time slice
    single_date <- nc_data[ , , index]
    
    # Change the dimension names of our matrix to "lon" and "lat",
    # and the row and column names to the latitude and longitude values.
    dimnames(single_date) <- list(lon = my_nc$dim$lon$vals[lon_idx],
                                  lat = my_nc$dim$lat$vals[lat_idx])
    single_date <- t(single_date)          # transpose
    single_date <- as.table(single_date)   # convert to table
    single_date <- as.data.frame(single_date) # dataframe
    single_date <- setNames(single_date, c("lat", "lon", "chlorophyll")) # set names
    
    return(single_date)
    
  }) 
  
  # add the names and bind them
  data_stack <- data_stack %>% 
    setNames(dates) %>% 
    bind_rows(.id = "date")
  
  # close connection
  # nc_close(my_nc)
  
  #return that table for the year
  return(data_stack)


}) %>% setNames(modis_years)

# Finally, append the years together
modis_full <- bind_rows(modis_list, .id = "year")
#rm(modis_list)

#plot an annual average
modis_full %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(year, month, lon, lat) %>% 
  summarise(mean_chla = mean(chlorophyll, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(lon = as.numeric(as.character(lon)),
         lat = as.numeric(as.character(lat))) %>% 
  filter(year == "2003", month == "7") %>% 
  ggplot(aes(lon, lat, fill = mean_chla, color = NULL)) +
  geom_tile() +
  labs(caption = "MODIS July 2003", x = NULL, y = NULL)









####__####


####_ 4. Study Area Polygons  ####


# Sf polygons
northeast <- rnaturalearth::ne_states(country = "united states of america") %>% st_as_sfc(crs = 4326)
canada <- rnaturalearth::ne_states(country = "canada") %>% st_as_sfc(crs = 4326)

area_bboxes <- tribble( #### bbox open   ####
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
) %>% arrange(area) #### bbox close  ####


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




####_______________________________####
####  Spatial-Temporal Formatting  ####



# Options

# CPR split into spatial strata?
# CPR temporal resolution by quarters?

# satellite data aggregated to gulf of maine as a whole
# satellite data reduced to single PC's for the region
# Time lags

####_ 1. Point Extraction from Satellite Data ####

# drop the fluff for this:
cpr_coords <- cpr %>% select(station_date, year, month, day, lon, lat) %>% 
  filter(year %in% modis_years)


# use map and extract to work through the years
cpr_sat <- cpr_coords %>% 
  split(.$year) %>% 
  imap_dfr(function(year_chunk, year_id){
    
    # For testing
    # year_id <- "2004"
    # year_chunk <- filter(cpr_coords, year == year_id)
    
    
    
    #make day of year column, we'll split on these to only extract once for each date
    year_chunk <- year_chunk %>% 
      #mutate(doy = lubridate::yday(station_date)) #Used when date was not included
      mutate(doy = str_replace_all(station_date, "-", "."))
    

    
    ####  Extract OISST by day, and with lags  ####
    oisst_extract <- year_chunk  %>% 
      split(.$doy) %>% 
      imap_dfr(function(daily_subset, day_id){
        
        # For testing
        #day_id <- "2004.01.21"
        #daily_subset <- filter(year_chunk, doy == day_id)
        
        
        # Extract the actual date
        layer_id <- str_c("X", day_id)
        daily_subset$oisst <- raster::extract(oisst_ras_list[[year_id]][[layer_id]], daily_subset[,c("lon", "lat")])
        
        
        # # Extracting from the month before:
        # # Build index for month lag, with year adjustment
        # day_id <-  as.character(daily_subset[1, "day"])
        # day_id <- ifelse(str_detect(day_id, "31"), str_replace(day_id, "31", "30"), day_id)
        # month_id <- as.character(daily_subset[1, "month"])
        # year_id <- ifelse(month_id == 1, 
        #                   as.character(as.numeric(year_id) - 1), 
        #                   year_id)
        # month_id <- ifelse(month_id == "1", "12", 
        #                    as.character(as.numeric(month_id) -1))
        # layer_id <- str_c("X", year_id, ".", month_id, ".", day_id)
        # 
        # # And extract the month lags
        # daily_subset$oisst_mlag <- raster::extract(oisst_ras_list[[year_id]][[layer_id]], daily_subset[,c("lon", "lat")])
        # 
        
        return(daily_subset)
        
        
      })
    
    
    #####  Extract MODIS by month  ####
    modis_extract <- oisst_extract %>% 
      split(.$month) %>% 
      imap_dfr(function(monthly_subset, month_id){
        
        # For Testing
        # month_id <- "02"
        # monthly_subset <- filter(oisst_extract, month == month_id) 
        
        # actual_date
        year_id <- as.character(monthly_subset[1, "year"])
        layer_id <- str_c("X", year_id, ".", str_pad(string = month_id, width = 2, side = "left", pad = "0"), ".16")
        monthly_subset$chla <- raster::extract(modis_ras_list[[year_id]][[layer_id]], monthly_subset[, c("lon", "lat")])
        
        # # month lag
        # month_id <- ifelse(month_id == "1", 
        #                    "12", 
        #                    as.character(as.numeric(month_id) - 1))
        # year_id <- ifelse(monthly_subset[1,"month"] == 1, 
        #                   as.character(as.numeric(as.character(year_id)) - 1), 
        #                   year_id)
        # layer_id <- str_c("X", year_id, ".", month_id, ".16")
        # 
        # if(year_id == "2002") {
        #   monthly_subset$chla_mlag <- NA
        # } else {
        #   monthly_subset$chla_mlag <- raster::extract(modis_ras_list[[year_id]][[layer_id]], monthly_subset[, c("lon", "lat")])
        # }
        
        return(monthly_subset)
      })
      
    
    return(modis_extract)
    
  })

is.nan(cpr_sat$chla)

cpr_sat %>% filter(year == "2005")



####_ 2. Joining Study Areas to Points  ####
cpr_sf <- cpr_coords %>% st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
cpr_area <- st_intersection(cpr_sf, area_polygons) %>% st_drop_geometry()
cpr_area <- cpr_area %>% select(station_date, lon, lat, area)




# And Finally the data for the model is born
model_cpr <- cpr_sat %>% 
  select(station_date, lon, lat, oisst, oisst_mlag, chla, chla_mlag) %>% 
  left_join(cpr_area, by = c("station_date", "lon", "lat")) %>% 
  left_join(cpr, by = c("station_date", "lon", "lat")) 


# # Save it out
# write_csv(x = model_cpr,
#           path = str_c(ccel_boxpath, "Gulf of Maine CPR", "2020_combined_data", "zooplankton_w_sstchla.csv", sep = "/"))





####_ 3. Resampling Rasters to Common Grid Size  ####

# Use raster::resample() to convert to largest grid size available (worst resolution)



# Steps:
# aggregate oisst as monthly averages
# Index numbers for each month
jan <- 1:31
feb <- 32:60
mar <- 61:91
apr <- 92:121
may <- 122:152
jun <- 153:182
jul <- 183:213
aug <- 214:244
sep <- 245:274
oct <- 275:305
nov <- 306:335
dec <- 336:366
plot(mean(oisst_ras_list$`1981`[[jan]]), 
     col = rev(RColorBrewer::brewer.pal(n = 10, name = "RdBu")), 
     main = "Jan 1981 Mean SST")



oisst_months <- imap(oisst_ras_list, function(year_ras, year_lab){
 
   if(year_lab != "1981"){
     month_stack <- raster::stack(
      x = list(
        jan = mean(year_ras[[which(names(year_ras) %in% str_c("X", 1:31))]]),
        feb = mean(year_ras[[which(names(year_ras) %in% str_c("X", 32:60))]]),
        mar = mean(year_ras[[which(names(year_ras) %in% str_c("X", 61:91))]]),
        apr = mean(year_ras[[which(names(year_ras) %in% str_c("X", 92:121))]]),
        may = mean(year_ras[[which(names(year_ras) %in% str_c("X", 122:152))]]),
        jun = mean(year_ras[[which(names(year_ras) %in% str_c("X", 153:182))]]),
        jul = mean(year_ras[[which(names(year_ras) %in% str_c("X", 183:213))]]),
        aug = mean(year_ras[[which(names(year_ras) %in% str_c("X", 214:244))]]),
        sep = mean(year_ras[[which(names(year_ras) %in% str_c("X", 245:274))]]),
        oct = mean(year_ras[[which(names(year_ras) %in% str_c("X", 275:305))]]),
        nov = mean(year_ras[[which(names(year_ras) %in% str_c("X", 306:335))]]),
        dec = mean(year_ras[[which(names(year_ras) %in% str_c("X", 336:365))]])
      )
     )} else {
       
       month_stack <- raster::stack(
         x = list(
           sep = mean(year_ras[[which(names(year_ras) %in% str_c("X", 1:30))]]),
           oct = mean(year_ras[[which(names(year_ras) %in% str_c("X", 31:61))]]),
           nov = mean(year_ras[[which(names(year_ras) %in% str_c("X", 62:91))]]),
           dec = mean(year_ras[[which(names(year_ras) %in% str_c("X", 92:122))]])
         )
       )
       
      
    }
  
  return(month_stack)
  
})

 
# match the timesteps up, resample modis to similar size