####  CPR Zooplankton Abundance Anomaly Splines  ####
# 1/31/2020

####  Packages  ####
library(gmRi)
library(broom)
library(splines)
library(mgcv)
library(tidyverse)
library(patchwork)
library(sf)
library(here)
library(targets)

#CCEL Boxpath
ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)

#### Functions  ####
source(here("R", "cpr_helper_funs.R"))


####  Data  ####

# # Combined dataset from NOAA/SAHFOS, 
# # concentrations in common units: # / 100 meters cubed: 
# # Checked in 17_noaa_sahfos_eda.R
# cpr <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "zooplankton_combined.csv", sep = "/"), 
#                 guess_max = 1e6, col_types = cols())

# Load targets data
tar_load(gom_combined_zooplankton)
cpr <- gom_combined_zooplankton

#eliminate sample_id column that is mixed in
cpr <- cpr %>% mutate(sample_id = NULL)





####__####
####  Calanus Test Run  ####
#Calanus Test Set
cal_test <- cpr %>% 
  mutate(cal_date = as.POSIXct(str_c(year, month, day, sep = "/"), format = "%Y/%m/%d"),
         jday = lubridate::yday(cal_date),
         abundance = `calanus i-iv`,
         log_abundance = log(abundance)) %>% 
  select(cal_date, year, month, jday, `latitude (degrees)`, `longitude (degrees)`, 
         abundance, log_abundance)

####  EDA  ####

#Annual Spline - 10 knots
ggplot(cal_test, aes(jday, abundance)) +
  geom_point(alpha = 0.1) +
  geom_smooth(formula = y ~ s(x, bs = "cc", k = 10), 
              method = "gam")

#Within year cyclic splines - 4 seasons, 5 knots
ggplot(cal_test, aes(jday,abundance)) +
  geom_point(alpha = 0.1) +
  geom_smooth(formula = y ~ s(x, bs = "cc", k = 4), 
              method = "gam") #Working with the data


####  Test Splines  ####

#Set seasonal bin number
season_bins <- 4
bin_splits <- c(seq(0, 365, by = ceiling(365 / (season_bins))), 365)
#names(bin_splits) <- c(1:length(bin_splits))

#Set period number in data based on desired number of splits
period <- data.frame(period = rep(0, nrow(cal_test)))
for (n in 1:nrow(cal_test)) {
  for (i in 1:season_bins) {
    if( cal_test$jday[n] > bin_splits[i] & cal_test$jday[n] <=  bin_splits[i+1]) {
      period[n,1] <- i
    }
      
  }
}

cal_test <- bind_cols(cal_test, period)

# #Fit a cubic spline is fit to the seasonal cycle using desired number of seasonal bins
# seasonal_spline <- gam(abundance ~  s(jday, bs = "cc",  k = length(bin_splits)),
#                            data = cal_test)



#Set the knot values explicitly using the equal breaks in year-day
seasonal_spline2 <- gam(abundance ~  s(jday, bs = "cc", k = length(bin_splits)),
                       knots = list(jday = bin_splits),
                       data = cal_test)


#going with this one
cal_test$seasonal_preds  <- predict(seasonal_spline2, cal_test, type = "response")

#The seasonal cycle is removed from the data and mean anomalies are computed for each year.
cal_test$season_removed  <- cal_test$abundance - cal_test$seasonal_preds
cal_test$anomaly <- ifelse(cal_test$season_removed > 0, "positive anomaly", "negative anommaly")


(period_anoms <- cal_test %>% 
  group_by(period) %>% 
  summarise(mean_anom = mean(season_removed, na.rm = T)))
(yearly_anoms <- cal_test %>% 
  group_by(year) %>% 
  summarise(mean_anom = mean(season_removed, na.rm = T)))

#Plotting i
(plot_a <- ggplot(cal_test, aes(jday, abundance)) +
  geom_point(alpha = 0.1, shape = 1) +
  geom_line(aes(jday, seasonal_preds), color = "royalblue", size = 1) +
  labs(x = "Calendar Day", y = "Abundance", caption = "Cyclic penalized regression spline fit to all years by calendar date"))

(plot_b <- ggplot(cal_test, aes(cal_date, abundance)) +
  geom_point(alpha = 0.1, shape = 1) +
  geom_line(aes(cal_date, seasonal_preds), color = "royalblue", size = 1) +
  
  geom_segment(data = filter(cal_test, 
                             cal_date >=  as.Date("1980-03-01 EST"), 
                             cal_date <=  as.Date("1983-12-31 EST")),
               aes(x = cal_date, y = seasonal_preds,
                   xend = cal_date, yend = abundance,
                   color = anomaly),
               alpha = 0.5) +
  
  labs(x = "Date", y = "Abundance", caption = "Difference from cubic spline then becomes the anomaly value") +
  ggforce::facet_zoom(xy = cal_date >= as.Date("1980-01-01 EST") & cal_date <= as.Date("1983-12-31 EST"), horizontal = FALSE))

####__####

####  Study Area Bounding Boxes  ####
# Added to spline function
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
sfdf <- st_sf(area = unique(area_bboxes$area), st_sfc(area_polygons), crs = 4326)



####__####
#### Seasonal Anomaly Calculations  ####

# CPR dat should be an m-by-5 matrix of CPR observations: [year, jday, lat, lon, #]
cpr_dat <- cpr %>%
  mutate(cal_date = as.POSIXct(str_c(year, month, day, sep = "/"), format = "%Y/%m/%d"),
         jday = lubridate::yday(cal_date),
         abundance = `calanus i-iv`,
         log_abundance = log(abundance),
         `longitude (degrees)` = ifelse(`longitude (degrees)` > 0, 
                                        `longitude (degrees)` * -1, 
                                        `longitude (degrees)`)) %>%
  select(year, jday, lat = `latitude (degrees)`, lon = `longitude (degrees)`, abundance)




#Where does the data fall in relation to study areas?
northeast <- rnaturalearth::ne_states(country = "united states of america") %>% st_as_sfc(crs = 4326)
canada <- rnaturalearth::ne_states(country = "canada") %>% st_as_sfc(crs = 4326)


cpr_dat %>% st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  ggplot() + 
    geom_sf() +
    geom_sf(data = northeast) +
    geom_sf(data = canada) +
    geom_sf(data = filter(sfdf, area == "GOM_new"), aes(fill = area), alpha = 0.3) +
    guides(fill = guide_legend(title = "Gulf of Maine Study Area", label = FALSE)) +
    coord_sf(xlim = c(-71,-64.8), ylim = c(41, 44.3)) +
    theme_bw() +
    theme(legend.position = "bottom")


####  1. Make Taxa List  ####


#Identify the columns that represent abundances
taxa_cols <- names(cpr)[12:ncol(cpr)]
names(taxa_cols) <- taxa_cols

# Make a list with details on each taxa
# Crop to study area at the end
taxa_list <- map(taxa_cols, function(x){
  taxa_name <- sym(x)
  taxa_subset <- cpr %>% 
    mutate(
      cal_date = as.POSIXct(str_c(year, month, day, sep = "/"), format = "%Y/%m/%d"),
      jday = lubridate::yday(cal_date)) %>% 
    select(year, jday, lat =`latitude (degrees)`, lon = `longitude (degrees)`, abundance = !!taxa_name) %>% 
    cpr_area_crop(., study_area = "gom_new")
    
})



####  2. Calanus Test  ####
calanus_anoms <- cpr_spline_fun(cpr_dat = taxa_list$`calanus i-iv`, 
                                spline_bins = 10, 
                                season_bins = 4)




#Plotting the time series
calanus_anoms$period_summs %>% 
  expand(year, period) %>% 
  left_join(calanus_anoms$period_summs, by = c("year", "period")) %>% 
  ggplot(aes(year, period_anom_std, color = period)) + 
  geom_hline(yintercept = 0, color = "royalblue", alpha = 0.5, linetype = "dashed") +
  geom_line() + scale_color_gmri(palette = "mixed") +
  facet_grid(period ~ .) + 
  labs(x = NULL, y = "Detrended Abundance Anomaly", subtitle = "Calanus Copepodites I-IV")

#Checking how many obs contribute to each
calanus_anoms$period_summs %>% 
  expand(year, period) %>% 
  left_join(calanus_anoms$period_summs, by = c("year", "period")) %>% 
  ggplot(aes(year, period_anom_n, color = period)) + 
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10, aes(fill = "10 Records or Less"), alpha = 0.01, color = NA) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 25, aes(fill = "25 Records or Less"), alpha = 0.01, color = NA) +
  geom_line() + 
  scale_fill_manual(values = c("gray10", "gray")) +
  scale_color_gmri(palette = "mixed") +
  facet_grid(period ~ .) + labs(x = NULL, y = "Number of Recordings / Anomaly Record")


#### 3. Limit to taxa with full time-series  ####

#Find those pesky NA taxa
na_counts <- map(taxa_list, function(x){
  sum(is.na(x$abundance))
}) %>% bind_cols() %>%
  pivot_longer(names_to = "taxa", values_to = "total NA's", cols = everything()) %>%
  mutate(status = case_when(
    `total NA's` == 290 ~ "NOAA only",
    `total NA's` == 4799 ~ "SAHFOS only",
    `total NA's` == 0 ~ "Found in both",
    TRUE ~ "Only NA's"
  ))

#Taxa with full time series
keepers <- filter(na_counts, status == "Found in both")
fullts_taxa <- taxa_list[names(taxa_list) %in% keepers$taxa]



####  4. Calculate Detrended Abundances for All  ####
fullts_taxa$`calanus i-iv`
anomaly_list <- map(.x = fullts_taxa,
                    .f = cpr_spline_fun, 
                    spline_bins = 10, 
                    season_bins = 4)



####  5. Pulling out Anomalies
anomaly_list$`calanus i-iv` #two objects, point observations and period summaries


####  6. Periodic summaries
period_summs <- imap(anomaly_list, function(x,y){x$period_summs <- mutate(x$period_summs, taxa = y)})
period_summs <- bind_rows(period_summs)

# Pivot wider using standardized anomalies to fill matrix
# the names were changed in function, hence comment out
periodic_anoms <- period_summs %>% 
  select(taxa, year, period, datebounds, anom_z) %>% 
  pivot_wider(names_from = taxa, values_from = anom_z) %>% 
  janitor::clean_names()
  # select(-period_anom_mu, -period_anom_sd) %>% 
  # pivot_wider(names_from = taxa, values_from = period_anom_std) %>% 
  # janitor::clean_names()


####__####

####  Export Anomalies  ####
write_csv(periodic_anoms,
          str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "detrended_anomalies_noaa_sahfos.csv", sep = "/"),
          col_names = TRUE)



####  Spline Plots of 05 Paper Species  ####
species_05 <- c("calanus i-iv", 
                "calanus finmarchicus v-vi", 
                "centropages typicus", 
                "chaetognatha eyecount",
                "euphausiacea spp.", 
                "metridia lucens", 
                "oithona spp.", 
                "para-pseudocalanus spp.", 
                "paraeuchaeta norvegica", 
                "temora longicornis")


splines_05 <- anomaly_list[names(anomaly_list) %in% species_05]
names(splines_05)



####  Plotting the Spline Fits for Season and TS fits  ####
spline_plots <- splines_05 %>% imap(function(x, y){
  ts_data <- x$cprdat_predicted %>% 
    #transmute(
    mutate(
      year = year,
      jday = jday,
      new_month = lubridate::month(as.Date(str_c(year, "-01-01")) + jday),
      new_day  = lubridate::day(as.Date(str_c(year, "-01-01")) + jday),
      cal_date = as.POSIXct(str_c(year, new_month, new_day, sep = "/"), format = "%Y/%m/%d")
      ) 
  
  #Add confidence intervals
  #Add error
  ts_data$se <-predict(x$spline_model, x$cprdat_predicted, type = "response", se = TRUE)$se.fit
  ts_data <- ts_data %>% mutate(
    uplim = spline_pred + 1.96 * se,
    lowlim = spline_pred - 1.96 * se
  )
  
  # Seasonal Fit
  season_fit <- ts_data %>%
    ggplot(aes(jday, log_abund)) +
    geom_point(alpha = 0.5) +
    geom_line(aes(jday, spline_pred), color = gmri_cols("gmri blue"), size = 1) +
    geom_line(aes(jday, uplim), color = gmri_cols("gmri blue"), size = 1, linetype = "dashed") +
    geom_line(aes(jday, lowlim), color = gmri_cols("gmri blue"), size = 1, linetype = "dashed") +
    geom_ribbon(aes(jday, ymin = lowlim, ymax = uplim), fill = gmri_cols("gmri blue"), alpha = 0.4) +
    labs(x = "Calendar Day", subtitle = y)
  
  
  
  #All Years Plot
  all_days <- data.frame(cal_date = seq(min(ts_data$cal_date), max(ts_data$cal_date), by = "day"))
  all_days <- mutate(all_days, jday = lubridate::yday(cal_date))
  all_days$spline_pred_all <- predict(x$spline_model, all_days)

  ts_data <- full_join(all_days, ts_data, by = c("cal_date", "jday"))

  all_years <- ts_data %>%
    ggplot(aes(cal_date, log_abund)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(cal_date, spline_pred_all), color = gmri_cols("gmri blue"), size = 1) +
      labs(x = NULL, subtitle = y)
  
  return(list("seasonal fit" = season_fit,
         "timeseries" = all_years))
  
})

#Plot both
spline_plots$`calanus i-iv`$`seasonal fit` + spline_plots$`calanus finmarchicus v-vi`$`seasonal fit`




