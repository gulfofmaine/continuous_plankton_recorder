####  NOAA MAB CPR
####  Seasonal Splines and Anomalies



####   Packages  ####
library(gmRi)
library(rnaturalearth)
library(sf)
library(mgcv)
library(tidyverse)

# Project helper functions
source(here::here("R", "cpr_helper_funs.R"))



####  Data  ####
mab_cpr <- read_csv(str_c(ccel_boxpath, "Data", "Mid Atlantic CPR", "noaa_mab_cpr_long.csv", sep = "/"),
                    guess_max = 1e6, col_types = cols())

# Polygons for mapping
new_england <- ne_states("united states of america") %>% st_as_sf(crs = 4326) 

####  General Data Tweaks  ####

# label dates on same year for plot axes
base_date <- as.Date("2000-01-01")

mab_edit <- mab_cpr %>% 
  mutate(sample    = str_pad(sample, width = 2, pad =  "0", "left"),
         station   = paste(cruise, sample, sep = "_"),
         samp_date = as.Date(paste(year, month, day, sep = "-")),
         jday      = lubridate::yday(samp_date),
         flat_date = as.Date(jday - 1, origin = base_date)) %>% 
  rename(abundance = abundance_per_100_cubic_meters)


# Table with all the dates for plotting smooth without gaps
full_dates <- seq.Date(from = min(mab_edit$samp_date), to = max(mab_edit$samp_date), by = 1)
full_date_df <- data.frame("samp_date" = full_dates)
full_date_df$jday <- lubridate::yday(full_date_df$samp_date)





####____####
####  PCI Seasonality  ####
# Phytoplankton color index doesn't operate on the same scale as
# the rest of the cpr data so it will get its own special treatment.
# Values should be on ordinal scale of 0, 1, 2, 3

pci <- mab_edit %>% 
  filter(taxonomic_name == "phytoplankton_color_index")

# Station Map
ggplot() +
  geom_sf(data = new_england) +
  geom_point(data = pci, aes(longitude, latitude)) +
  coord_sf(xlim = c(-76.5, -69), 
           ylim = c(37, 41.5)) +
  labs(x = "", y = "")



# Intra-annual Timeline
set_knots <- 10

# 1. Plot Within year cyclic splines
ggplot(pci, aes(jday, abundance)) +
  geom_point(alpha = 0.05) +
  geom_smooth(formula = y ~ s(x, bs = "cc", k = set_knots), 
              method = "gam") #Working with the data
 
# Do GAM seperate for tuning
pci_gam <- gam(formula = abundance ~ s(jday, bs = "cc", k = set_knots), data = pci)
#gam.check(pci_gam)

# Grab predictions
gam_preds <- predict(pci_gam, pci, type = "response", se.fit = T)
pci$pci_clim <- gam_preds$fit

# 2. Plot everything with Month labels on x axis
ggplot(pci, aes(flat_date, abundance)) +
  geom_point(alpha = 0.05) +
  geom_smooth(formula = y ~ s(x, bs = "cc", k = set_knots), 
              method = "gam") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "", y = "Phytoplankton Color Index")


# add predictions for all dates
full_date_df$pci_clim <- predict(pci_gam, full_date_df, type = "response")

# 3. Plotting recurring spline with observed abundances smoothed
ggplot(pci, aes(samp_date, abundance)) +
  geom_point(alpha = 0.05) +
  geom_smooth(formula = y ~ s(x, k = 100),
              method = "gam",
              aes(color = "k = 100, Observed Variation Smooth")) +
  geom_line(data = full_date_df, aes(samp_date, pci_clim, color = "k = 10, seasonal climate")) +
  labs(x = "", y = "Phytoplankton Color Index") +
  theme(legend.position = "bottom")
  


# 4. Should anomalies come from raw, or a smooth on observed
# Plot one year. smooth on observed vs seasonal smooth
single_year <- c(2004:2005)
obs_2020  <- filter(pci, lubridate::year(samp_date) %in% single_year)
pred_2020 <- filter(full_date_df, lubridate::year(samp_date) %in% single_year)
day_avgs <- obs_2020 %>% group_by(samp_date) %>% summarise(mean_abund = mean(abundance)) %>% ungroup()

ggplot() +
  geom_line(data = pred_2020, 
            aes(samp_date, pci_clim, color = "40 yr Seasonal Average"),
            size = 1) +
  geom_smooth(data = obs_2020, 
              aes(samp_date, abundance, color = "Observed Abundance GAM Smooth"),
              formula = y ~ s(x, k = (5 * length(single_year))),
              method = "gam", se = FALSE) +
  geom_point(data = obs_2020, 
             aes(samp_date, abundance), alpha = 0.05) +
  geom_point(data = day_avgs, 
             aes(samp_date, mean_abund, color = "Mean Observed Daily Abundance")) +
  scale_x_date(date_labels = "%b", date_breaks = "3 month") +
  labs(x = "", y = "Phytoplankton Color Index", 
       title = "Test: Anomalies from smoothed abundance",
       subtitle = "Using smoothed GAM fit instead of abundance bins.\n2004-2005",
       caption = "Let the observed data be smoothed with same wigglyness as intra-annual climatology.") +
  theme(legend.position = "bottom") 



####____####
####  Calanus Seasonality  ####
calanus <- mab_edit %>% 
  filter(taxonomic_name == "Calanus finmarchicus") %>% 
  mutate(log_abund = log(abundance),
         log_abund = ifelse(is.infinite(log_abund), 0, log_abund))

calanus %>% 
  group_by(life_stage) %>% 
  summarise(mean_abund = mean(abundance))


####__ 1. GAM on observed abund  ####
# Fit gam
set_knots <- 10
cal_gam <- gam(formula = abundance ~ s(jday, bs = "cc", k = set_knots), data = pci)
full_date_df$calanus_clim <- predict(cal_gam, full_date_df, type = "response")


# Plot everything with Month labels on x axis
ggplot(calanus, aes(flat_date, abundance)) +
  #geom_point(alpha = 0.05) +
  geom_smooth(formula = y ~ s(x, bs = "cc", k = set_knots), 
              method = "gam") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "", y = "Abundance per 100 cubic meters")


# Plotting recurring spline with observed abundances smoothed
ggplot(calanus, aes(samp_date, abundance)) +
  geom_point(alpha = 0.05) +
  geom_smooth(formula = y ~ s(x, k = 100),
              method = "gam",
              aes(color = "k = 100, Observed Variation Smooth")) +
  geom_line(data = full_date_df, aes(samp_date, calanus_clim, color = "k = 10, seasonal climate")) +
  labs(x = "", y = "Zooplankton Abundance") +
  theme(legend.position = "bottom")



####__ 2. GAM on Log abund

# log abund gam model
log_gam <- gam(log_abund ~  s(jday, bs = "cc", k = 10),
               data = calanus)

full_date_df$calanus_log <- predict(log_gam, full_date_df, type = "response")


# compare the seasonal fits
single_year <- c(2005:2005)
obs_2020  <- filter(calanus, lubridate::year(samp_date) %in% single_year)
pred_2020 <- filter(full_date_df, lubridate::year(samp_date) %in% single_year)
cal_avgs <- obs_2020 %>% group_by(samp_date) %>% summarise(mean_abund = mean(abundance)) %>% ungroup()


# So which one is worse?
ggplot(pred_2020) +
  geom_line(aes(x = samp_date, y = exp(calanus_log), color = "Log Abundance GAM")) +
  geom_line(aes(x = samp_date, y = calanus_clim, color = "Raw Abundance GAM")) +
  geom_point(data = cal_avgs, aes(samp_date, mean_abund, color = "Daily Mean Abundance")) +
  scale_x_date(date_labels = "%b", date_breaks = "3 month") +
  labs(x = "", y = "Abundance / 100 cubic meters") +
  scale_color_gmri() +
  theme(legend.position = "bottom")






####  All Taxa Fits  ####

#### Split data by taxa
distinct_taxa <- sort(unique(mab_edit$taxonomic_name))
distinct_taxa <- setNames(distinct_taxa, distinct_taxa)
taxa_list <- map(distinct_taxa, function(taxa){filter(mab_edit, taxonomic_name == taxa)})



#### Get seasonal variation overall
seasonal_fits <- map(taxa_list, function(taxa){
  
  seasonal_gam  <- gam(abundance ~ s(jday, bs = "cc", k = 10),
                       data = taxa)
  taxa$gam_pred <- predict(seasonal_gam, taxa, type = "response")
  taxa$anom     <- taxa$abundance - taxa$gam_pred
  list_out      <- list("data" = taxa, "model" = seasonal_gam)
  return(list_out)
})




#### Plot the intra-annual variation, but color points by stage
names(seasonal_fits$`Calanus finmarchicus`$data)


#re-group to early and late stage copepodites
early_stage <- c("te i", "e ii", " iii", "e iv", "i-iv")
late_stage  <- c("te v", "e vi", "v-vi")


# Pull predictions
cal_predictions <- data.frame(samp_date = full_dates,
                              jday = lubridate::yday(full_dates))


# Apply Model
cal_predictions$pred_abund <- predict(
  object = seasonal_fits$`Calanus finmarchicus`$model,
  newdata = cal_predictions)


# Plot the early-late stages
seasonal_fits$`Calanus finmarchicus`$data %>% 
  mutate(
    group_stages = case_when(
      str_sub(life_stage, -4, -1) %in% early_stage ~ "Early Stages i-iv",
      str_sub(life_stage, -4, -1) %in% late_stage ~ "Late Stages v-vi",
      TRUE ~ "unstaged"
    )) %>% 
  ggplot(aes(samp_date, abundance, color = group_stages)) +
  geom_point() +
  geom_line(data = cal_predictions, aes(samp_date, pred_abund), color = "gray50") +
  scale_y_log10()

  
