# MARMAP Data Access
# Source:

# Libraries
library(readxl)
library(tidyvere)
library(here)
library(gmRi)


# Accessing MARMAP Data
# load data
URL <- 'ftp://ftp.nefsc.noaa.gov/pub/hydro/zooplankton_data/EcoMon_Plankton_Data_v3_0.xlsx'
ZPD <- openxlsx::read.xlsx(URL, sheet='Data')
