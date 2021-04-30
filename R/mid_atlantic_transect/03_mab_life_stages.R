####  NOAA MAB CPR
####  Early-late life stage cleanup

# prepare abundances for seasonal splines 
# using early and adult stage groups 



####   Packages  ####
library(gmRi)
library(tidyverse)

# Project helper functions
source(here::here("R", "cpr_helper_funs.R"))



####  Data  ####
mab_cpr <- read_csv(str_c(ccel_boxpath, "Data", "Mid Atlantic CPR", "noaa_mab_cpr_long.csv", sep = "/"),
                    guess_max = 1e6, col_types = cols())


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



#### Adjusting  Life Stages  ####

# Want a way to consolidate to early and adult stages for the copepods like calanus

#re-group to early and late stage copepodites
early_stage   <- c("te i", "e ii", " iii", "e iv", "i-iv", "-iii")
middle_stage <- c("ii-v", " i-v", "iv-v", "i-vi", " i-v")
late_stage    <- c("te v", "e vi", "v-vi")




# Consolidate life stages
mab_edit <- mab_edit %>% 
  mutate(
    life_stage_groups = case_when(
      
      # Group Stages
      str_sub(life_stage, -4, -1) %in% early_stage  ~ "i-iv",
      str_sub(life_stage, -4, -1) %in% late_stage   ~ "v-vi",
      str_sub(life_stage, -4, -1) %in% middle_stage ~ "stage uncertain i-vi",
      
      # Keyword Detection 
      str_detect(life_stage, "adult")               ~ "adult",
      str_detect(life_stage, "immature")            ~ "immature",
      str_detect(life_stage, "post calyptopis")     ~ "post calyptopis",
      str_detect(life_stage, "calyptopis")          ~ "calyptopis",
      str_detect(life_stage, "zoea")                ~ "zoea",
      str_detect(life_stage, "nauplius")            ~ "nauplius",
      str_detect(life_stage, "megalopa")            ~ "megalopa",
      str_detect(life_stage, "medusa")              ~ "medusa",
      str_detect(life_stage, "egg")                 ~ "egg",
      str_detect(life_stage, "veliger")             ~ "veliger",
      str_detect(life_stage, "larva")               ~ "larva",
      str_detect(life_stage, "cypris")              ~ "cypris",
      str_detect(life_stage, "parva")               ~ "post-larva",
      str_detect(life_stage, "cyphonautes")         ~ "cyphonautes",
      str_detect(life_stage, "copepodite")          ~ "stage uncertain i-vi",
      TRUE ~ "unstaged"),
    new_groups = paste(taxonomic_name, life_stage_groups)) 

# Check stages that were missed
mab_edit %>% 
  count(life_stage, life_stage_groups) %>% 
  #count(taxonomic_name, life_stage, group_stages) %>% 
  View("life stages")


# Check the consolidated names
mab_edit %>% distinct(new_groups) %>% arrange(new_groups)


####  Export with Groups  ####
write_csv(mab_edit,
          str_c(ccel_boxpath, "Data", "Mid Atlantic CPR", "noaa_mab_lifestages.csv", sep = "/"))

