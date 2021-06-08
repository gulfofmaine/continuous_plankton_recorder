#### 3/28/2020
#### Converting CPR Demnsities to Biomass


####  Packages  ####
library(tidyverse)
library(here)
library(gmRi)
library(patchwork)
library(sf)


####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))


####  Load Data  ####

# CPR Dataset with quarterly anomalies and SST with a one-period lag
# source: 03_new_anoms_quarterly_sst.R
cpr_sst <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "anomalies_w_quarterlysst.csv", sep = "/"),
                    col_types = cols(),
                    guess_max = 1e5)

# Length Regression Coefficients
# Source: 18_karen_s_analysis_funs.R
coefficient_table <- read_csv(file = str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "karen_s_coefficients.csv", sep = "/"),
                              col_types = cols())



####  Matching coefficients with the NOAA/SAHFOS groups  ####

#Drop stages with no coefficients
coefficient_table <- coefficient_table %>% drop_na()

#Determine which stages comprise NOAA/SAHFOS Groups
#coefficient_table %>% distinct(taxa_stage) %>% View("Karen Species")  #Taxa Karen has regression info for

#The following groups are present in the CPR data - special characters were a mistake...
karen_sp <- c(
  "acartia_spp.",
  "calanus_i-iv",
  "calanus_finmarchicus_v-vi",
  "candacia_spp.",
  "centropages_spp.",
  "centropages_hamatus",
  "centropages_typicus",
  "clausocalanus_spp.",
  "metridia_lucens",
  "metridia_i-iv",
  "nannocalanus_minor",
  "oithona_spp.",
  "paracalanus_spp.",
  "para-pseudocalanus_spp.",
  "paraeuchaeta_norvegica",
  "pleuromamma_spp.",
  "pseudocalanus_spp.",
  "temora_longicornis"
) 

#cpr_sst %>% distinct(taxa) %>% View("cpr taxa")
# # 18 taxa Working group
# cpr_sst %>% filter(taxa %in% karen_sp)




####  Load non-standardized density data  ####
cpr <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "zooplankton_combined.csv", sep = "/"), 
                guess_max = 1e6, col_types = cols())



#Identify the columns that represent abundances of the 18 taxa we have
taxa_cols <- str_replace_all(karen_sp, pattern = "_", " ")

#Pull the important stuff
cpr <- cpr %>% 
  mutate(
    cal_date = as.POSIXct(str_c(year, month, day, sep = "/"), format = "%Y/%m/%d"),
    jday = lubridate::yday(cal_date)
  ) %>% 
  select(year, jday, lat =`latitude (degrees)`, lon = `longitude (degrees)`, one_of(taxa_cols))







####  Add the SST to the Raw CPR  ####




###__Quarterly Mean SST  ####
cpr_sst







###__Daily SST  ####

ncpath <- "/Users/akemberling/Box/Adam Kemberling/Box_Projects/Convergence_ML/data/"
dailymu.stack <- raster::stack(str_c(ncpath, "sst.wkmean.1981-1989.nc", sep = "/"))
dailymu.stack <- raster::rotate(dailymu.stack) # Conversion from 0-360 to -180-180


###__Date Extraction for Points  ####
