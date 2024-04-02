####  Data Exploration  #### 
# Accounting for diel migration variability in Calanus




####  Packages  ####
{
  library(gmRi)
  library(lubridate)
  library(patchwork)
  library(sf)
  library(raster)
  library(metR)
  library(here)
  library(tidyverse)
  library(targets)
  conflicted::conflict_prefer("select", "dplyr")
  conflicted::conflict_prefer("filter", "dplyr")
}


####  Data  ####

# ggtheme
theme_set(theme_bw())

####  Data  ####
# cpr <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "zooplankton_combined.csv", sep = "/"), 
#                 guess_max = 1e6, col_types = cols()) %>% 
#   rename(lon = `longitude (degrees)`, lat = `latitude (degrees)`) %>% 
#   mutate_at(c("year", "month", "day"), factor) %>% 
#   mutate(lon = ifelse(lon > 0, lon * -1, lon))

# Or use targets
tar_load(cpr_spline_prepped) 
cpr <- cpr_spline_prepped



####  Goal  ####
# Pull adult and small calanus
# look at time of day distribution
# see how the availability of calanus changes with tod, and with time of year
# Is there reason to be concerned that a seasonal cycle without tod should not be used


# Plot the kind of sampling coverage we have
yr_range <- c(1960:2020)
data.frame(
  "year" = yr_range,
  "month" = rep(c(1:12), length(yr_range))) %>% 
  left_join(cpr_spline_prepped) %>% 
  group_by(year, month) %>% 
  summarise(
    n_stations = n_distinct(station)-1, # There is always 1 because we completed all cases
    .groups = "drop") %>% #filter(n_stations == 1)
  mutate(
    year = as.numeric(as.character(year)),
    month = month.abb[month],
    month = factor(month, levels = month.abb),
    n_stations = ifelse(n_stations < 1, NA, n_stations)) %>% #filter(year == 1994)
  ggplot(aes(year, fct_rev(month))) +
  geom_tile(aes(fill = n_stations)) +
  scale_fill_steps(
    low = "#EDF8FB", 
    high = "#006D2C", 
    na.value = "gray40", breaks = c(1,5,10,15,20,25),
    oob = scales::oob_squish) +
  theme_gmri() +
  labs(title = "CPR Sampling Coverage", x = "Year", y = "Month", fill = "Samples")





# Pull Data for the Calanus Taxa
calanus <- cpr_spline_prepped %>% 
  select(1:13, starts_with("calanus")) %>% 
  select(-contains("hyper")) %>% 
  select(-contains("glaci")) %>% 
  select(-contains("spp")) %>% 
  select(-contains("helgo")) %>% 
  mutate(
    # Make a column that has both date and time
    dt = cal_date + hours(hour) + minutes(minute),
    dt = as.POSIXct(dt),
    # Make the opposite, where time is real but the date is bullshit,
    hr = str_pad(hour, 2, "left", 0),
    mn = str_pad(minute, 2, "left", 0),
    ft = as.POSIXct(
      str_c("2000-01-01 " , hr, ":", mn, ":01"), 
      format = "%Y-%m-%d %H:%M:%S"),
    .before = "year"
    ) %>% 
  select(-c(hr,mn))
  
str(calanus$cal_date)
str(calanus$dt)
str(calanus$ft)

# Why are these not processing at all...
bad_apples <- calanus %>% 
  # These are acting so weird...
  filter(is.na(dt)) %>% 
  pull(cal_date)

# Debug datetimes
some_datetimes <- as.POSIXct(c("1961-07-16 05:00:00", "1961-07-16 05:00:00", "1961-07-16 17:00:00"))
hms::as_hms(some_datetimes)

#### Time Gremlins  ####

# Calanus and time relationship...
calanus %>% 
  pivot_longer(starts_with("calanus"), names_to = "taxa", values_to = "abund") %>% 
  ggplot(aes(ft, abund)) +
  geom_point(aes(color = month), alpha = 0.3) +
  geom_smooth(linewidth = 1.5, se = FALSE, color = "orange") +
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%I %p") +
  scale_y_log10() +
  facet_wrap(~taxa, nrow = 2) +
  scale_color_stepsn(
    colors = c(
      "lightblue", 
      "#ABB400",
      "#407331",
      "#00736D",
      "#00608A",
      "#15B2D3",
      "#FFD700",
      "#F3872F",
      "#9C2706",
      "#D45B12",
      "#5F5426",
      "#B3DAF1"
      ),
    breaks = 1:12) +
  theme_gmri(legend.position = "bottom", legend.direction = "horizontal") +
  guides(color = guide_colorsteps(barwidth = unit(4, "in"))) +
  labs(x = "Time of Day")


  
calanus %>% 
  pivot_longer(starts_with("calanus"), names_to = "taxa", values_to = "abund") %>% 
  ggplot(aes(dt, log(abund))) +
  geom_point(aes(color = month), alpha = 0.3) +
  geom_smooth(linewidth = 1.5, se = FALSE, color = "orange") +
  scale_x_datetime(date_breaks = "10 year", date_labels = "%Y") +
  facet_wrap(~taxa, nrow = 2) +
  scale_color_stepsn(
    colors = c(
      "lightblue", 
      "#ABB400",
      "#407331",
      "#00736D",
      "#00608A",
      "#15B2D3",
      "#FFD700",
      "#F3872F",
      "#9C2706",
      "#D45B12",
      "#5F5426",
      "#B3DAF1"
    ),
    breaks = 1:12) +
  theme_gmri() +
  labs(x = "Date")



####  Moon Phase  ####

# Get the moon/sun position
library(lunar)
library(suncalc)
moon_info <- getMoonIllumination(date = as.Date(calanus$cal_date))


getMoonIllumination(date = as.Date(calanus$cal_date[[1]]), keep = "fraction")
# Look at how log abundance changes with time of day
  

         