####  NOAA MAB CPR
####  Seasonal Splines and Anomalies


####   Packages  ####
library(splines)
library(mgcv)
library(gmRi)
library(tidyverse)
library(patchwork)
library(sf)


# Project helper functions
source(here::here("R", "cpr_helper_funs.R"))


####  Data  ####
noaa_mab <- read_csv(str_c(ccel_boxpath, "Data", "Mid Atlantic CPR", "noaa_mab_lifestages.csv", sep = "/"))












####  Export Anomalies  ####
# write_csv(mab_anoms,
#           str_c(ccel_boxpath, "Data", "Mid Atlantic CPR", "detrended_anomalies_mab.csv", sep = "/"),
#           col_names = TRUE)