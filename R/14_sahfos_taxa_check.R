####  Taxon diagnostics for NOAA and SAHFOS cpr data  ####
####  1/2/2020

####  Packages  ####
library(tidyverse)
library(here)
library(patchwork)
library(gmRi)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####____________________________####
noaa_phyto_key        <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/noaa_phyto_key_2019.csv")
noaa_phyto_abundances <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/noaa_phyto_abundances_2019.csv")

noaa_zoo_key          <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/noaa_zoo_key_2019.csv")
noaa_zoo_abundances   <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/noaa_zoo_abundances_2019.csv")

mc1_phyto <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/mc1_phyto_2019.csv")
mc1_eye   <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/mc1_eyecount_2019.csv")
mc1_trav  <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/mc1_traverse_2019.csv")
mc1_taxa  <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/mc1_taxa_key_2019.csv")

mc2_phyto <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/mc2_phyto_2019.csv")
mc2_eye   <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/mc2_eyecount_2019.csv")
mc2_trav  <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/mc2_traverse_2019.csv")
mc2_taxa  <- read_csv("/Data/Gulf of Maine CPR/2019_data_processing/mc2_taxa_key_2019.csv")
