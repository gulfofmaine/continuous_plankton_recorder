#### Mid Atlantic CPR Survey
#### First Look
#### 12/15/2020



####  Packages  ####
library(gmRi)
library(readxl)
library(janitor)
library(tidyverse)




####  Data  ####
source(here::here("R", "cpr_helper_funs.R"))


# Load Data

# The following excel file was obtained from:
# Chris Melrose, NOAA on 11/20/2020 
mab_cpr <- read_xlsx( path = str_c(cpr_boxpath, "data","mid_atlantic_survey", "MID ATLANTIC CPR DATA.XLSX", sep = "/"), skip = 5)
mab_cpr <- clean_names(mab_cpr)

# Header Notes:
# Note 1- Values of -9999 for abundace indicate that a taxa was observed in the sample but not counted during quantitative analysis
# Note 2- Date for both the Gulf of Maine and Mid Atlantic cpr routes are included. All data north of 41 degrees are from the GOM route.
# Note 3- Values of NaN for phytoplankton color index indicate no data.







#### Data cleanup  ####
# cleanup will follow process of 15_NOAA_CPR_Cleanup.R



####  1. Remove unused Taxa  ####

# These taxonomic names have no abundances at all in our data
no_records <- mab_cpr %>% 
  group_by(taxonomic_name) %>% 
  summarise(presence = ifelse(sum(abundance_per_100_cubic_meters, na.rm = T) > 0, "present", "absent")) %>% 
  ungroup() %>% 
  filter(presence == "absent") %>% 
  pull(taxonomic_name)


# drop GOM transects
# Take empty taxon groups out
mab_inuse <- mab_cpr %>% 
  filter(latitude <= 41,
         taxonomic_name %not in% no_records) %>% 
  mutate(longitude = longitude * -1)



# 3% of records were present but not counted
sum(mab_inuse$abundance_per_100_cubic_meters == -9999) / nrow(mab_inuse)



# Change these to minimum value for abundance to preserve presence information
mab_inuse %>% count(abundance_per_100_cubic_meters) # display the values used
mab_inuse %>% count(abundance_per_100_cubic_meters) %>% arrange(desc(abundance_per_100_cubic_meters))



#are they consistently certain taxa
mab_inuse %>% 
  mutate(pres_not_counted = ifelse(abundance_per_100_cubic_meters == -9999, 1, 0)) %>% 
  group_by(taxonomic_name) %>% 
  summarise(n_9999 = sum(pres_not_counted)) %>% arrange(desc(n_9999))# %>% ggplot(aes(x = n_9999)) + geom_histogram()



# replace the -9999
mab_inuse[mab_inuse$abundance_per_100_cubic_meters == -9999, "abundance_per_100_cubic_meters"] <- NA



# Map them
mab_inuse %>% 
  distinct(latitude, longitude) %>% 
  ggplot(aes(longitude, latitude)) + 
    geom_point()




 ####  2. Make Column Key  ####
#column names to allocate into list
sort(unique(mab_inuse$taxonomic_name))
# change caps
mab_inuse <- mab_inuse %>% 
  mutate(taxonomic_name = str_to_sentence(taxonomic_name))


# plot each taxa and its stages with mean abundance
taxa_plots <- mab_inuse %>% 
  split(.$taxonomic_name) %>% 
  imap(function(taxa, tname){
    taxa %>% 
      group_by(life_stage) %>% 
      summarise(total_dens = sum(abundance_per_100_cubic_meters, na.rm = T),
                mean_dens = mean(abundance_per_100_cubic_meters, na.rm = T),
                .groups = "keep") %>% 
      ggplot(aes(mean_dens, life_stage,  fill = life_stage)) +
        geom_col(show.legend = F) + labs(x = "Stage", y = "Mean Abundance", subtitle = tname)
  })



####  Coarse Metrics  ####
top_ranks <- mab_inuse %>% 
  mutate(samp_occurrence = ifelse(abundance_per_100_cubic_meters > 0, 1, 0)) %>% 
  group_by(taxonomic_name) %>%
  summarise(mean_dens = mean(abundance_per_100_cubic_meters, na.rm = T),
            perc_occurrence = mean(samp_occurrence, na.rm = T),
            total_occurrence = sum(samp_occurrence, na.rm = T),
            total_dens = sum(abundance_per_100_cubic_meters, na.rm = T), .groups = "keep") %>% 
  ungroup()

# Percent Occurrence in CPR - NO zeros apparently
top_ranks %>% arrange(desc(perc_occurrence)) %>% pull(taxonomic_name) %>% head(10)

# Avg. density
top_ranks %>% slice_max(n = 10, order_by = mean_dens) %>% pull(taxonomic_name)

# Highest Occurrence
top_ranks %>% slice_max(n = 10, order_by = total_occurrence) %>% pull(taxonomic_name)



# Some front runners
taxa_plots$`Penilia avirostris`
taxa_plots$`Temora longicornis`
taxa_plots$Gastropoda
taxa_plots$`Centropages typicus`




####  Pivot Wide  ####

# Have cases where there are more than one row of a stage group at a station
names(mab_inuse)
wide_prep <- mab_inuse %>% 
  mutate(life_stage = str_to_lower(life_stage),
         stage_group = paste(taxonomic_name, life_stage, sep = "____"),
         taxonomic_name = NULL, 
         life_stage = NULL, 
         marmap_taxonomic_code = NULL,
         marmap_stage_code = NULL)  %>% 
  group_by(cruise, sample, year, month, day, hour, minute, latitude, longitude,
           phytoplankton_color_index, stage_group) %>% 
  summarise(abundance = sum(abundance_per_100_cubic_meters, na.rm = T)) %>% 
  ungroup()



# Spread wide
cpr_wide <- wide_prep %>% 
  pivot_wider(names_from = stage_group, values_from = abundance, values_fill = 0)


# Spread back to maintain zeros in long form
long_full <- cpr_wide %>% 
  mutate(phytoplankton_color_index = ifelse(is.nan(phytoplankton_color_index) == TRUE, NA, phytoplankton_color_index),
         phytoplankton_color_index = as.numeric(phytoplankton_color_index)) %>% 
  pivot_longer(names_to = "stage_group", values_to = "abundance_per_100_cubic_meters",
               cols = 10:ncol(.))



####  Coarse Metrics  ####


# Split the names back for more detailed work
names_distinct <- unique(long_full$stage_group)
names_split <- str_split(names_distinct, pattern = "____") 
names_split_df <- names_split %>% map_dfr(function(name_group){
  if(length(name_group) == 1){
    df_out <- data.frame("taxonomic_name" = name_group[1], "life_stage" = NA)
  } else if(length(name_group) > 1){
    df_out <- data.frame("taxonomic_name" = name_group[1], "life_stage" = name_group[2])
  }
  return(df_out)
})
names_split_df$stage_group <- names_distinct

# assign them back to long df
long_full$taxonomic_name <- rep("no math", nrow(long_full))
long_full$life_stage <- rep("no math", nrow(long_full))
for (i in 1:nrow(names_split_df)) {
  stage_group_i                       <- names_split_df$stage_group[i]
  rows_i                              <- which(long_full$stage_group == stage_group_i)
  long_full[rows_i, "taxonomic_name"] <- names_split_df[i,"taxonomic_name"]
  long_full[rows_i, "life_stage"]     <- names_split_df[i,"life_stage"]
  rm(stage_group_i, rows_i)
}

head(long_full[,c("stage_group", "taxonomic_name", "life_stage")])




# Summarise Coarse Metrics again
top_ranks <- long_full %>% 
  mutate(samp_occurrence = ifelse(abundance_per_100_cubic_meters > 0, 1, 0)) %>% 
  group_by(taxonomic_name) %>%
  summarise(mean_dens = mean(abundance_per_100_cubic_meters, na.rm = T),
            perc_occurrence = mean(samp_occurrence, na.rm = T),
            total_occurrence = sum(samp_occurrence, na.rm = T),
            total_dens = sum(abundance_per_100_cubic_meters, na.rm = T), .groups = "keep") %>% 
  ungroup()

# Percent Occurrence in CPR - NO zeros apparently
top_ranks %>% arrange(desc(perc_occurrence)) %>% pull(taxonomic_name) %>% head(10)

# Avg. density
top_ranks %>% slice_max(n = 10, order_by = mean_dens) %>% pull(taxonomic_name)

# Highest Occurrence
top_ranks %>% slice_max(n = 10, order_by = total_occurrence) %>% pull(taxonomic_name)



# Some new, correct front runners
taxa_plots$Eupahausiacea
taxa_plots$`Temora longicornis`
taxa_plots$Gastropoda
taxa_plots$`Centropages typicus`



####  Export Long Form Data
long_full %>% 
  select(cruise, sample, year, month, day, hour, minute, latitude, longitude,
         taxonomic_name, life_stage, abundance_per_100_cubic_meters) %>% 
write_csv(path = str_c(ccel_boxpath, "Data", "Mid Atlantic CPR", "noaa_mab_cpr_long.csv", sep = "/"), 
          col_names = TRUE)
