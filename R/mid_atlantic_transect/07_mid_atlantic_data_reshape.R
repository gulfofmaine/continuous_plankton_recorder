####  Mid-Atlantic CPR Data Reshaping from Excel
#### Script is sourced to provide reshaped data for data exploration script
####  4/17/2020

####  Load Packages  ####
library(tidyverse)
library(gmRi)
library(readxl)
library(data.table)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####  Load Data  ####


#### Taxa Key  ####
mb_key <- read_xlsx(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "MB_CPRdata.xlsx", sep = "/"), sheet = 4) %>% 
  select(c(1, 2, 6, 7, 12, 13))  %>% 
  setNames(c("eyecount", "eyecount_taxa", "trav", "traverse_taxa", "phyto", "phytoplankton_taxa")) 

# Pull columns relating to each size class
eye_key <- select(mb_key, 1,2) %>% setNames(c("Accepted ID", "Taxon Name")) %>% mutate(taxa_class = "eyecount")
traverse_key <- select(mb_key, 3,4) %>% setNames(c("Accepted ID", "Taxon Name")) %>% mutate(taxa_class = "traverse")
phyto_key <- select(mb_key, 5,6) %>% setNames(c("Accepted ID", "Taxon Name")) %>% mutate(taxa_class = "phytoplankton")

# Make single key
mb_key <- bind_rows(list(eye_key, traverse_key, phyto_key)) %>% 
  filter(is.na(`Accepted ID`) == FALSE)
rm(eye_key, traverse_key, phyto_key)

####  Eyecount Size-class  ####
mb_eye <- read_xlsx(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "MB_CPRdata.xlsx", sep = "/"), sheet = 1) %>% 
  as.data.frame()

####  Traverse Data  ####
mb_trav <- read_xlsx(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "MB_CPRdata.xlsx", sep = "/"), sheet = 2)

####  Phytoplankton (size) Data  ####
mb_phyto <- read_xlsx(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "MB_CPRdata.xlsx", sep = "/"), sheet = 3)


####  Cleanup function for MC datasets
mc_cleanup <- function(messy_df = mb_phyto, taxon_key = mb_key) {
  
  #Create new columns to match NOAA data
  clean_df <- messy_df %>% 
    mutate(cruise = str_extract(sample_id, "[^-]*"),
           station = str_extract(sample_id, "-.*"),
           station = str_replace(station, "-", ""),
           day = lubridate::day(midpoint_UTC),
           hour = lubridate::hour(midpoint_UTC),
           minute = lubridate::minute(midpoint_UTC)) 
    
  # Grab all the important columns
    clean_df <- clean_df %>% 
      select(sample_id, cruise, station, year, month, day, hour, minute, 
             latitude, longitude, everything()) %>% 
      select(-midpoint_UTC) 
  
    # Swap names for each taxa using the name key key
    names(clean_df) <- ifelse(
      names(clean_df) %in%  taxon_key$`Accepted ID`, 
      taxon_key$`Taxon Name`, 
      names(clean_df)) %>% 
      str_remove_all(pattern = "'") 
    
    clean_df <- clean_df %>% 
      rename_all(tolower)
    
  
  return(clean_df)
}



#Clean the three pieces with the cleanup function:
mb_phyto <- mc_cleanup(messy_df = mb_phyto)
mb_trav  <- mc_cleanup(messy_df = mb_trav)
mb_eye   <- mc_cleanup(messy_df = mb_eye)






####__####
####  Combining Datasets (Phyto, Traverse, Transect)  ####



###__1. 100 Meters Cubed Conversion    ####

# NOAA ECOMON Records plankton density as #/100m3
# To match this unit we need to convert from the counting system of the CPR survey 


# These are the amount of each silk transect that are counted under a microscope
# Each transet is 10 nautical miles travelled
# A. subsample count to full transect
# phyto 1/8000th of transect counted
# traverse 1/40th of transect counted
# eyecount full transect counted

#Conversions - 

# Look at the two measurement scales to note the values recorded:
mb_trav %>% count(`centropages bradyi`)
mb_eye  %>% count(`centropages bradyi`)



# Original calculation to get to # per meters cubed
# Based on opening aperture area and the distance travelled per sample (i.e. water volume sampled)
# This rate resolves density per 1m3
conversion_rate <- 1/2.987091

# Multiply by 100 to get to 100 cubic meters
conversion_rate <- conversion_rate * 100

#Listed by source
mb_abundances <- list(
  "traverse" = mb_trav   %>% select(11:ncol(mb_trav)), 
  "eyecount" = mb_eye    %>% select(11:ncol(mb_eye)), 
  "phyto"    = mb_phyto  %>% select(12:ncol(mb_phyto)))



# Get the meters cubed numbers
mb_100_m3 <-  map(mb_abundances, function(x){
  x_100_meters_cubed <- x * conversion_rate
  return(x_100_meters_cubed)
})




####__2. Combine Traverse and eyecount  ####

trav_100_m3 <- mb_100_m3$traverse
eye_100_m3 <- mb_100_m3$eye


#Dataframe comparisons

#All have the same number of rows, but columns are present in some but not others
janitor::compare_df_cols(trav_100_m3, eye_100_m3)
janitor::compare_df_cols_same(trav_100_m3, eye_100_m3)

#Idea, create a dataframe with names found in both, 
# make the values the added contribution from one or both the subsample types
unique_names     <- sort(unique(c(names(trav_100_m3), names(eye_100_m3))))
new_df           <- data.frame(matrix(0, nrow = nrow(trav_100_m3), ncol = length(unique_names)))
colnames(new_df) <- unique_names


#Function to add columns as they appear in either set. 
#Overwrites NA's with zeros
taxa_fill <- function(
    empty_frame = new_df,  
    df_1 = trav_100_m3, 
    df_2 = eye_100_m3) {
  
  #Taxon Names We want for the output
  taxa_names <- colnames(empty_frame)
  
  # 1. Abundances from the traverse subsampling procedure
  
  #Make a list that matches output names
  traverse_counts <- vector(mode = "list", length = length(taxa_names))
  names(traverse_counts) <- taxa_names
  
  # Fill that list with traverse abundances when they match
  traverse_counts <- imap(traverse_counts, function(x,y) {
    
    #Baseline of 0
    taxa_counts <- rep(0, nrow(df_1))
    
    if(y %in% names(df_1)) {
      taxa_counts <- df_1 %>% select(one_of(y)) %>% pull()
      taxa_counts[is.na(taxa_counts)] <- 0
    }
    
    return(taxa_counts)
  })
  
  #Bind list to a dataframe
  traverse_out <- bind_cols(traverse_counts)
  
  # 2. Abundances from eyecount subsampling procedure
  
  #Make a list that matches output names
  eyecount_counts <- vector(mode = "list", length = length(taxa_names))
  names(eyecount_counts) <- taxa_names
  
  # Fill that list with eyecount abundances
  eyecount_counts <- imap(eyecount_counts, function(x,y) {
    #Baseline of zero
    taxa_counts <- rep(0, nrow(df_2))
    
    if(y %in% names(df_2)) {
      taxa_counts <- df_2 %>% select(one_of(y)) %>% pull()
      taxa_counts[is.na(taxa_counts)] <- 0
    }
    
    return(taxa_counts)
  })
  
  #Bind list to a dataframe
  eyecount_out <- bind_cols(eyecount_counts)
  
  
  #Add the two of them to get combined abundance
  zooplankton_abundances <- traverse_out + eyecount_out
  return(zooplankton_abundances)
  
  
  
  
}




####  Combined Traverse and Eyecounts  ####
# 1:1 sum of traverse and eyecount abunndances in # per cubic meter
mb_zoo <- taxa_fill(
  empty_frame = new_df, 
  df_1 = trav_100_m3, 
  df_2 = eye_100_m3)
mb_zoo <- bind_cols(mb_trav %>% select(1:10), mb_zoo)


# Visual Check
mb_zoo %>% 
  group_by(year) %>% 
  dplyr::summarise(`centropages typicus` = sum(`centropages typicus`, na.rm = T),
                   `centropages bradyi` = sum(`centropages bradyi`, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(year, `centropages typicus`, color = "centropages typicus")) +
  geom_point(aes(year, `centropages bradyi`, color = "centropages bradyi")) +
  scale_y_continuous(labels = scales::comma_format())


#remove unnecesary objects for use when sourcing
rm(new_df, mb_abundances, mb_eye, mb_100_m3, mb_phyto, mb_trav, eye_100_m3, trav_100_m3)
