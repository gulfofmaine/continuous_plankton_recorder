#### Data Reshaping for BCO-DMO Submission 

# Mid-Atlantic Bight Data
# Award Info: NSF Award OCE-1851866




####  Load Packages  ####
library(tidyverse)
library(readxl)



####  Functions  ####
# source(here::here("R", "cpr_helper_funs.R"))

####  Load Data  ####

# Path to Excel File
ccel_boxpath <- "/Users/akemberling/Library/CloudStorage/Box-Box/Climate Change Ecology Lab"


####  1. Taxonomic Key  ####
# 4th sheet in xlsx file
# contains three groups of columns that provide the column name (number) and associated taxa
# This code skips empty columns and renames them
mb_key <- read_xlsx(
  path = str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "MB_CPRdata.xlsx", sep = "/"), 
  sheet = 4) %>% 
  select(c(1, 2, 6, 7, 12, 13))  %>% 
  setNames(c("eyecount", "eyecount_taxa", "trav", "traverse_taxa", "phyto", "phytoplankton_taxa")) 

# Pull just the columns relating to each size class
# Create a column to identify what measurement stage they are counted at
eye_key <- select(mb_key, 1,2) %>% setNames(c("taxa_num", "taxa_name")) %>% mutate(measurement_stage = "eyecount")
traverse_key <- select(mb_key, 3,4) %>% setNames(c("taxa_num", "taxa_name")) %>% mutate(measurement_stage = "traverse")
phyto_key <- select(mb_key, 5,6) %>% setNames(c("taxa_num", "taxa_name")) %>% mutate(measurement_stage = "phytoplankton")



# Make single key by appending the rows on top of one another
# Remove the '
tidy_key <- bind_rows(list(eye_key, traverse_key, phyto_key)) %>% 
  filter(is.na(taxa_num) == FALSE) %>% 
  mutate(taxa_num = as.character(taxa_num),
         taxa_name = str_remove_all(taxa_name, "'"))

# Clear environment of the building blocks
rm(eye_key, traverse_key, phyto_key)



####--------------------------####
####  Abundance Data Reshape  ####


####  Phytoplankton Abundances  ####
mb_phyto <- read_xlsx(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "MB_CPRdata.xlsx", sep = "/"), sheet = 3)


# Pivot Longer
phyto_long <- mb_phyto %>% 
  pivot_longer(cols = c(8:ncol(mb_phyto)),
               names_to = "taxa_num",
               values_to = "count", 
               values_drop_na = T)


####  Eyecount Zooplankton Abundances  ####
mb_eye <- read_xlsx(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "MB_CPRdata.xlsx", sep = "/"), sheet = 1) %>% 
  as.data.frame()

# Pivot Longer, no PCI this time
eyecount_long <- mb_eye %>% 
  pivot_longer(cols = c(7:ncol(mb_eye)),
               names_to = "taxa_num",
               values_to = "count", 
               values_drop_na = T)


####  Traverse Zooplankton Abundances  ####
mb_trav <- read_xlsx(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "MB_CPRdata.xlsx", sep = "/"), sheet = 2)

# Pivot Longer, no PCI this time
traverse_long <- mb_trav %>% 
  pivot_longer(cols = c(7:ncol(mb_trav)),
               names_to = "taxa_num",
               values_to = "count", 
               values_drop_na = T)


# Put all the counts together
counts_long <- 
  phyto_long %>% 
    full_join(traverse_long, by = join_by(sample_id, latitude, longitude, midpoint_UTC, year, month, taxa_num, count)) %>% 
    full_join(eyecount_long, by = join_by(sample_id, latitude, longitude, midpoint_UTC, year, month, taxa_num, count)) %>% 
    mutate(taxa_num = as.character(taxa_num)) 


# Clean environment of consituent parts
rm(mb_eye, mb_phyto, mb_trav)



####  Match Taxonomic ID to Taxa Number  ####
counts_tidy <- left_join(counts_long, tidy_key) %>% 
  arrange(midpoint_UTC, taxa_name)



# Done
counts_tidy


# SAVE it:
write_csv(counts_tidy, here::here("erddap_data/mid_atlantic_long_bco_dmo.csv"))



#### BONUS: Unit Conversion  ####

# CPR counts report number per silk transect
# This corresponds to 10 nautical miles travelled per transect
# With an opening aperture dimension of 5cm x 10 cm

# To convert to common units with NOAA ECOMON
# Need to calculate the volumn of water that passes through the CPR device per silk sample
# This is aperture area * 10 nauticalmiles

# Then convert counts for that volume to their equivalent for one cubic meter of water
conversion_rate <- 1/2.987091

# Then multiply that by 100
conversion_rate <- conversion_rate * 100

# Add new column
tidy_bonus <- counts_tidy %>% mutate(abundance_100m3 = count * conversion_rate)




####  BONUS: WORMS/Aphia ID  ####

library(worms)
