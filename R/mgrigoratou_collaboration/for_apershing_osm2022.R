# Materials Prep for Andy Pershing
# Subject: Ocean Sciences Meeting


"

I'm wondering whether it is worth looking for lagged relationships between the zooplankton anomalies and the buoy data. 
Maria's modeling suggests that we might see stronger relationships if the physics leads the plankton by 1 or even 2 years 
(something that is consistent w/ my old NAO work)


I'd like to look at the phytoplankton colour index (PCI) time series. 
I'm not thinking of necessarily including it in the paper, but I think we should at least plot the time series.

  - It would be great to get the quarterly TS for the two Buoy PCAs, 
 
  - the taxa in Fig 7 (Calanus 1-4, Calanus 5-6, C. typ, Oithona, Para-Pseudo, Metridia, and Euphs). 
 
  - If possible, quarterly means of the PCI (no need to deal with the seasonal cycle).
"


####  Packages  ####
library(gmRi)
library(tidyverse)
library(targets)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

# Paths
cpr_boxpath <- gmRi::cs_path("root", "Adam Kemberling/Box_Projects/continuous_plankton_recorder")


####  Targets Data  ####


####  1. Buoy Data  ####

# Temperature anomaly data from NERACOOS Buoys
buoy <- read.csv(str_c(cpr_boxpath, "data/processed_data/buoy_anomalies.csv")) %>% 
  mutate(reading_depth = factor(
    reading_depth, levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters")))

# Just use annual and quarterly means
buoy_quarters <- buoy %>% filter(period %in% c("Annual", "Q1", "Q2", "Q3", "Q4"))

# Save for Andy
write_csv(buoy_quarters, here::here("andy_osm_data/neracoos_buoys_quarterly.csv"))


####  2. Quarterly Phytoplankton color index and Key Taxa  ####
tar_load(gom_seasonal_avgs)

#Key Taxa for analysis
key_taxa <- c("phytoplankton color index", "calanus i-iv", "calanus finmarchicus v-vi",
              "centropages typicus", "oithona spp.", "para-pseudocalanus spp.", "metridia lucens",
              "euphausiacea spp.")

# Pull taxa and PCI
taxa_avgs <- gom_seasonal_avgs %>% 
  filter(taxa %in% key_taxa) %>% 
  mutate(period = case_when(
    period == "annual" ~ "Annual",
    period == "1" ~ "Q1",
    period == "2" ~ "Q2",
    period == "3" ~ "Q3",
    period == "4" ~ "Q4"))



# save for Andy
write_csv(taxa_avgs, here::here("andy_osm_data/cpr_osm_taxa_quarterly.csv"))




####  3. CPR PCA  ####






####  4. Buoy PCA Quarterly Timeseries  ####

# # Matrix used for daily PCA
# buoy_pca_mat <- buoy_raw %>% 
#   column_to_rownames(var = "Date") %>% 
#   as.matrix()

# Data from PCA saved for later (this exact situation)
buoy_pca_timeseries <- read_csv(here("R/new_anom_analyses/derived_data/buoy_pca_raw.csv"), guess_max = 1e5) %>% 
  mutate(date_yday = lubridate::yday(Date))
buoy_pca_timeseries_i <- read_csv(here("R/new_anom_analyses/derived_data/buoy_pca_interp.csv"), guess_max = 1e5) %>% 
  mutate(date_yday = lubridate::yday(Date))

# Get the same date start/ends for the periods used in the taxa groups
quarter_ends <- taxa_avgs %>% 
  distinct(period, datebounds) %>% 
  filter(period != "Annual") %>% 
  separate(datebounds, into = c("yday_start", "yday_end"), sep = "-") %>% 
  split(.$period) 


# Add the quarters, join the two back together
buoy_pca_quarters <- map_dfr(quarter_ends, function(end_df){
  start <- as.numeric(end_df$yday_start[1])
  end <- as.numeric(end_df$yday_end[1])
  period <- end_df$period[1]
  
  buoy_pca_timeseries_i %>% 
    rename(gap_interpolated_loading = `Principal Component Loading`) %>% 
    left_join(buoy_pca_timeseries, by = c("Date", "PC_num", "Principal Component", "date_yday")) %>% 
    select(Date, pc_num = PC_num, perc_explained = `Principal Component`, 
           pc_loading = `Principal Component Loading`, gap_interpolated_loading, everything()) %>% 
    mutate(period = ifelse((date_yday >= start & date_yday <= end), period, NA)) %>% 
    filter(is.na(period) == FALSE) %>% 
    select(-date_yday)}) %>% 
  arrange(Date)


# Save it out for Andy:
write_csv(buoy_pca_quarters, here::here("andy_osm_data/buoy_pca_quarterly.csv"))




# ####  5. Buoy Physical Data  ####
# # Should we use the averages from: 
# 
# 
# # Reference Script: here(R/new_anom_analyses/12_paper_layout.Rmd)
# # PCA code: here(R/new_anom_analyses/09_new_anoms_buoy_cpr_PCA.R)
# 
# # Gappy Buoy data - source: 10_buoy_daily_interpolations
# buoy_raw <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_raw.csv", sep = "/"),
#                      col_types = cols(),
#                      guess_max = 1e5)
# 
# # Interpolated NA Buoy data - source: 10_buoy_daily_interpolations.R
# buoy_i <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_interpolated.csv", sep = "/"),
#                    col_types = cols())
# 



# Add the quarters in:


##### 6. SST  ####

# CPR Dataset with quarterly anomalies and SST with a one-period lag
# souce: 03_new_anoms_quarterly_sst.R
cpr_sst <- read_csv(str_c(ccel_boxpath, "Data/Gulf of Maine CPR/2020_combined_data/anomalies_w_quarterlysst.csv"),
                    col_types = cols(),
                    guess_max = 1e5)


# Add some lags:


# # save for Andy
# write_csv(cpr_sst_lagged, here::here("andy_osm_data/"))
