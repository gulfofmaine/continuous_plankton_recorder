####  NOAA/SAHFOS CPR Data Wrangle  ####
####  12/19/2019

####  Packages  ####
library(tidyverse)
library(here)
library(patchwork)
library(gmRi)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####____________________________####

####   NOAA Data  ####
#NOTE: Observed in sample but not quantified during standard analysis is given as a value of -9999. Use care to filter these values when performing analysis.

####__####
####  NOAA Phyto  ####
noaa_phyto <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/NOAA_1961-2013/Gulf of Maine CPR (Feb 14, 2014 update).xlsx"),
                                skip = 2,
                                sheet = 1)

#Pull data out from the header
noaa_phyto_abundances <- noaa_phyto[2:nrow(noaa_phyto),]

#Fix the names
phyto_group_stage_names <- rep(NA, ncol(noaa_phyto) - 10)
for (i in 1:(ncol(noaa_phyto)-10)) {
  phyto_group_stage_names[i] <- names(noaa_phyto[i + 10])                                 #Just the names of the taxa
  phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "'")           #Drop 's
  phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], " \\d")        #Drop first digit
  phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\d")         #Drop second digit
  phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\d")         #Drop third digit
  phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\.")         #Drop first period
  phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\.")         #Drop second period
  phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\.")         #Drop third period
  phyto_group_stage_names[i] <- str_replace_all(phyto_group_stage_names[i], " _", "_")    #Drop space before _
}
head(phyto_group_stage_names)

#Replace old names with correct names
names(noaa_phyto_abundances)[11:ncol(noaa_phyto_abundances)] <- phyto_group_stage_names
names(noaa_phyto_abundances) <- str_replace_all(names(noaa_phyto_abundances), "'", "")
head(names(noaa_phyto_abundances), 12)

#Fix header, and make taxon key
noaa_phyto_header <- noaa_phyto[1, 10:ncol(noaa_phyto)]
names(noaa_phyto_header) <- c("code_type", phyto_group_stage_names)
head(noaa_phyto_header, 10)

#Reshape Taxon Key
noaa_phyto_key <- noaa_phyto_header %>% pivot_longer(cols = -code_type, names_to = "Taxon Name", values_to = "Accepted ID") %>% arrange(`Taxon Name`)

####  Repair MARMAP Key  ####
#MARMAP code key: https://www.nefsc.noaa.gov/nefsc/Narragansett/taxcodesA.html
noaa_phyto_key <- noaa_phyto_key %>% pivot_wider(names_from = code_type, values_from = `Accepted ID`, )
noaa_phyto_key <- noaa_phyto_key %>% rename_all(tolower)


#Check duplicate columns to compare values
noaa_p_duplicates <- noaa_phyto_key %>% filter(.copy == 2) %>% pull(`taxon name`)
noaa_phyto_key %>% filter(`taxon name` %in% noaa_p_duplicates)

#Reference the NEFSC key to find mismatches
noaa_phyto_key[noaa_phyto_key$`taxon name` %in% noaa_p_duplicates,]

#Repair taxon names using NEFSC marmap key
which(noaa_phyto[1,] == 9101) #Index for Trichodesmium
which(noaa_phyto[1,] == 9169) #Index for Ceratium ranipes
colnames(noaa_phyto_abundances)[which(noaa_phyto[1,] == 9101)] <- "Trichodesmium"
colnames(noaa_phyto_abundances)[which(noaa_phyto[1,] == 9169)] <- "Ceratium ranipes"

#Repair key using NEFSC marmap key
noaa_phyto_key <- noaa_phyto_key %>% mutate(
  `taxon name` = ifelse(`marmap code:` == 9169, "Ceratium ranipes", `taxon name`),
  `taxon name` = ifelse(`marmap code:` == 9101, "Trichodesmium", `taxon name`)
) %>% select(-.copy)


####  Prep for Export  ####
noaa_phyto_abundances <- noaa_phyto_abundances %>%  
  rename_all(tolower)  %>% 
  mutate(cruise = str_replace_all(cruise, "'", ""))

rm(noaa_phyto, noaa_phyto_header, phyto_group_stage_names, noaa_p_duplicates)



####  End NOAA Phyto  ####
####__####








#### NOAA  Zooplankton  ####
noaa_zoo <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/NOAA_1961-2013/Gulf of Maine CPR (Feb 14, 2014 update).xlsx"),
                              skip = 2,
                              sheet = 2)

#Pull data out from under the header
noaa_zoo_abundances <- noaa_zoo[4:nrow(noaa_zoo),]

#Fix names, there's 10 columns before we get to the zooplankton groups and stages
group_stage_names <- rep(NA, ncol(noaa_zoo) - 10)
for (i in 1:(ncol(noaa_zoo)-10)) {
  group_stage_names[i] <- str_c(names(noaa_zoo[i + 10]), noaa_zoo[1, i + 10], sep = "_")
  group_stage_names[i] <- str_remove_all(group_stage_names[i], "'")           #Drop 's
  group_stage_names[i] <- str_remove_all(group_stage_names[i], " \\d")        #Drop first digit
  group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\d")         #Drop second digit
  group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\d")         #Drop third digit
  group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\.")         #Drop first period
  group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\.")         #Drop second period
  group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\.")         #Drop third period
  group_stage_names[i] <- str_replace_all(group_stage_names[i], " _", "_")    #Drop space before _
}
head(group_stage_names)

#Replace old names with correct names
names(noaa_zoo_abundances)[11:ncol(noaa_zoo_abundances)] <- group_stage_names
names(noaa_zoo_abundances) <- str_replace_all(names(noaa_zoo_abundances), "'", "")
head(names(noaa_zoo_abundances), 12)

#Fix header, and make taxon key
noaa_zoo_header <- noaa_zoo[2:3, 10:ncol(noaa_zoo)]
names(noaa_zoo_header) <- c("code_type", group_stage_names)
head(names(noaa_zoo_header), 10)

#Reshape Taxon Key
noaa_zoo_key <- noaa_zoo_header %>% pivot_longer(cols = -code_type, names_to = "Taxon Name", values_to = "Accepted ID") %>% arrange(`Taxon Name`)


####  Repair MARMAP Key  ####
noaa_zoo_key <- noaa_zoo_key %>% pivot_wider(names_from = code_type, values_from = `Accepted ID`, )
noaa_zoo_key <- noaa_zoo_key %>% rename_all(tolower)


#Check duplicate columns to compare values
noaa_z_duplicates <- noaa_zoo_key %>% filter(.copy == 2) %>% pull(`taxon name`)
noaa_zoo_key %>% filter(`taxon name` %in% noaa_z_duplicates)

#Repair taxon names using NEFSC marmap key
which(noaa_zoo[2,] == 3300) #Index for Heteropoda
which(noaa_zoo[2,] == 4039) #Index for something...or nothing...
colnames(noaa_zoo_abundances)[which(noaa_zoo[2,] == 3300)] <- "Heteropoda"
colnames(noaa_zoo_abundances)[which(noaa_zoo[2,] == 4039)] <- "No record"

#Drop column that has no taxa match for the marmap code 
noaa_zoo_abundances[,"No record"] <- NULL

#Repair key using NEFSC marmap key
noaa_zoo_key <- noaa_zoo_key %>% mutate(
  `taxon name` = ifelse(`marmap taxonomic code:` == 3300, "Heteropoda", `taxon name`),
  `taxon name` = ifelse(`marmap taxonomic code:` == 4039, "No record", `taxon name`)) %>% 
  select(-.copy) %>% 
  filter(`taxon name` != "No record")

#Seperate taxa and stage
noaa_zoo_key <- noaa_zoo_key %>% mutate(
  taxa = str_extract(`taxon name`, "[^_]*"),
  stage = str_extract(`taxon name`, "_.*"),
  stage = str_replace(stage, "_", "")
)


####  Prep for Export  ####
noaa_zoo_abundances <- noaa_zoo_abundances %>%  
  rename_all(tolower) %>% 
  mutate(cruise = str_replace_all(cruise, "'", ""))

rm(noaa_zoo, noaa_zoo_header, group_stage_names, noaa_z_duplicates)



####  End NOAA Zooplankton  ####
####____________________________####

####  SAHFOS Data  ####
####__####
####  MC Part 1  ####

#MC part 1
mc1_phyto   <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part1.xlsx"), sheet = 1)
mc1_eye     <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part1.xlsx"), sheet = 2)
mc1_trav    <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part1.xlsx"), sheet = 3)

# Combining Taxon Keys for phytoplankton, eye, and trav groups
mc1_t_phyto <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part1.xlsx"), skip = 1, sheet = 5) %>% 
  mutate(taxa_class = "phyto") %>% 
  mutate(note = NA)
mc1_t_trav  <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part1.xlsx"), skip = 1, sheet = 6) %>% 
  rename(note = 3) %>% 
  mutate(taxa_class = "trav")
mc1_t_eye   <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part1.xlsx"), skip = 1, sheet = 7) %>% 
  rename(note = 3) %>% 
  mutate(taxa_class = "eye")

#Combine Taxa Key
mc1_taxa <- bind_rows(mc1_t_phyto, mc1_t_trav, mc1_t_eye) %>%
  arrange(taxa_class, `Taxon Name`) %>% 
  select(taxa_class, `Taxon Name`, `Accepted ID`, note)
rm(mc1_t_phyto, mc1_t_trav, mc1_t_eye)
#Combined key
head(mc1_taxa)

####  Cleanup function for MC datasets
mc_cleanup <- function(messy_df = mc1_phyto, taxon_key = mc1_taxa, taxa = "phyto") {
  
  #Create new columns to match NOAA data
  clean_df <- messy_df %>% 
    mutate(Cruise = str_extract(Sample_Id, "[^-]*"),
           Station = str_extract(Sample_Id, "-.*"),
           Station = str_replace(Station, "-", ""),
           Day = lubridate::day(Midpoint_Date_Local),
           Hour = lubridate::hour(Midpoint_Date_Local),
           Minute = lubridate::minute(Midpoint_Date_Local)) 
  
  #Phytoplankton sheet has the phytoplankton color index
  if(taxa == "phyto"){
    clean_df <- clean_df %>% 
      select(Sample_Id, Cruise, Station, Year, Month, Day, Hour, Minute, 
             `Latitude (degrees)` = Latitude, `Longitude (degrees)` = Longitude, 
             `Phytoplankton Color Index` = Chlorophyll_Index,  everything()) %>% 
      select(-Midpoint_Date_Local)
  } else {
    clean_df <- clean_df %>% 
      select(Sample_Id, Cruise, Station, Year, Month, Day, Hour, Minute, 
             `Latitude (degrees)` = Latitude, `Longitude (degrees)` = Longitude, everything()) %>% 
      select(-Midpoint_Date_Local)
  }
  
  #Pull taxon key for just phytoplankton
  tkey <- taxon_key %>% filter(taxa_class == taxa)
  
  #Swap column names using lookup table
  data.table::setnames(clean_df, old = as.character(tkey$`Accepted ID`), new = tkey$`Taxon Name`, skip_absent = TRUE)
  
  return(clean_df)
}

####_Phyto  ####
mc1_phyto <- mc_cleanup(messy_df = mc1_phyto, taxon_key = mc1_taxa, taxa = "phyto")

####_Eye  ####
mc1_eye <- mc_cleanup(messy_df = mc1_eye, taxon_key = mc1_taxa, taxa = "eye")

###_Trav  ####
mc1_trav <- mc_cleanup(messy_df = mc1_trav, taxon_key = mc1_taxa, taxa = "trav")


####  prep For Export  ####
mc1_phyto <- mc1_phyto %>% rename_all(tolower)  #Phytoplankton 
mc1_eye   <- mc1_eye   %>% rename_all(tolower)  #Eye sheet
mc1_trav  <- mc1_trav  %>% rename_all(tolower)  #Tav sheet
mc1_taxa  <- mc1_taxa  %>% rename_all(tolower)  #Taxonomic Key



####  End MC Part 1  ####
####__####

####  MC Part 2  ####
mc2_phyto   <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part 2.xlsx"), sheet = 1)
mc2_eye   <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part 2.xlsx"), sheet = 2)
mc2_trav   <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part 2.xlsx"), sheet = 3)

# Combining Taxon Keys for phytoplankton, eye, and trav groups
mc2_t_phyto   <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part 2.xlsx"), 
                                   sheet = 4, skip = 1) %>% 
  select(`Taxon Name` = 2, `Accepted ID` = 1, note = 5) %>% 
  mutate(taxa_class = "phyto") %>% filter(is.na(`Taxon Name`) == FALSE)
mc2_t_trav   <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part 2.xlsx"), 
                                  sheet = 4, skip = 1) %>% 
  select(`Taxon Name` = 8, `Accepted ID` = 7, note = 10) %>% 
  mutate(taxa_class = "trav") %>% filter(is.na(`Taxon Name`) == FALSE)
mc2_t_eye   <- readxl::read_xlsx(str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/SAHFOS-MBA_2013-2017/MC part 2.xlsx"), 
                                 sheet = 4, skip = 1) %>% 
  select(`Taxon Name` = 14, `Accepted ID` = 13, note = 16) %>% 
  mutate(taxa_class = "eye") %>% filter(is.na(`Taxon Name`) == FALSE)

mc2_taxa <- bind_rows(mc2_t_phyto, mc2_t_trav, mc2_t_eye) %>% 
  arrange(taxa_class, `Taxon Name`) %>% 
  select(taxa_class, `Taxon Name`, `Accepted ID`, note)
rm(mc2_t_phyto, mc2_t_trav, mc2_t_eye)

#Combined Key
head(mc2_taxa)

####_Phyto  ####
mc2_phyto <- mc_cleanup(mc2_phyto, mc2_taxa, "phyto")

####_Eye  ####
mc2_eye  <- mc_cleanup(mc2_eye, mc2_taxa, "eye")


####_Trav  ####
mc2_trav <- mc_cleanup(mc2_trav, mc2_taxa, "trav")

####  Prep for Export  ####
mc2_phyto <- mc2_phyto %>% rename_all(tolower)  #Phytoplankton 
mc2_eye   <- mc2_eye   %>% rename_all(tolower)  #Eye sheet
mc2_trav  <- mc2_trav  %>% rename_all(tolower)  #Tav sheet
mc2_taxa  <- mc2_taxa  %>% rename_all(tolower)  #Taxonomic Key

####  End MC Part 2  ####




####____________________________####

####  Combining SAHFOS "Trav" & "Eye"  ####



####____________________________####



####  Data Export  ####
noaa_out  <- str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/2019_data_processing/")
mc1_out   <- str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/2019_data_processing/")
mc2_out   <- str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/2019_data_processing/")


# NOAA phytoplankton
write_csv(noaa_phyto_abundances, str_c(noaa_out, "noaa_phyto_abundances_2019.csv"), col_names = TRUE)
write_csv(noaa_phyto_key, str_c(noaa_out, "noaa_phyto_key_2019.csv"), col_names = TRUE)

# NOAA zooplankton
write_csv(noaa_zoo_abundances, str_c(noaa_out, "noaa_zoo_abundances_2019.csv"), col_names = TRUE)
write_csv(noaa_zoo_key, str_c(noaa_out, "noaa_zoo_key_2019.csv"), col_names = TRUE)


# mc1 export
write_csv(mc1_phyto, str_c(mc1_out, "mc1_phyto_2019.csv"),    col_names = TRUE)
write_csv(mc1_trav,  str_c(mc1_out, "mc1_traverse_2019.csv"), col_names = TRUE)
write_csv(mc1_eye,   str_c(mc1_out, "mc1_eyecount_2019.csv"), col_names = TRUE)
write_csv(mc1_taxa,  str_c(mc1_out, "mc1_taxa_key_2019.csv"), col_names = TRUE)

# mc2 export
write_csv(mc2_phyto, str_c(mc2_out, "mc2_phyto_2019.csv"),    col_names = TRUE)
write_csv(mc2_trav,  str_c(mc2_out, "mc2_traverse_2019.csv"), col_names = TRUE)
write_csv(mc2_eye,   str_c(mc2_out, "mc2_eyecount_2019.csv"), col_names = TRUE)
write_csv(mc2_taxa,  str_c(mc2_out, "mc2_taxa_key_2019.csv"), col_names = TRUE)


####____________________________####
