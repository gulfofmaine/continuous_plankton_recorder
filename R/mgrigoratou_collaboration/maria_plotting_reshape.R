# Maria Data Reshaping


####  Packages  ####
library(tidyverse)
library(gmRi)
library(ggstream)
library(readxl)
library(treemap)

####  Data from Maria  ###

####__ 1. Major plankton groups, by biomass  ####
# responses to heatwave event

# # The identity of the plankton groups in the matrix
# species_name <- read_csv(here::here("maria_data/maria_supplied/species_name.csv"), col_names = F)
# 
# # Matrix of relative biomass
# rel_biomass_species <- read_csv(here::here("maria_data/maria_supplied/rel_biomass_species.csv"), col_names = F)
# 
# # species name matrix
# # rows = years
# # columns are dominant species, each number is a code for a taxa
# species_name %>% head()
# 
# # each row is the relative biomass
# rel_biomass_species %>% head()




####__  2. Individual Plankton Groups  ####

# protists, matches species name, is the taxa identifier in a matrix
ind_p <- read_csv(here::here("maria_data/maria_supplied/ind_P.csv"), col_names = F)

# relative biomass, matches rel_biomass
rel_bio_prot <- read_csv(here::here("maria_data/maria_supplied/bio_Prot.csv"), col_names = F)


# Passive Copepods
ind_cp <- read_xlsx(here::here("maria_data/maria_supplied/ind_Cp.xlsx"), col_names = F)
rel_bio_cp <- read_csv(here::here("maria_data/maria_supplied/bio_Cp.csv"), col_names = F)


# Active Copepods
ind_ca <- read_csv(here::here("maria_data/maria_supplied/ind_Ca.csv"), col_names = F)
rel_bio_ca <- read_csv(here::here("maria_data/maria_supplied/bio_Ca.csv"), col_names = F)


####__  3. Key to which species matches taxa number  ####
group_index <- read_xlsx(here::here("maria_data/maria_supplied/group_index.xlsx"))



####__ 4. Absolute abundances  ####

# abs bio
abs_bio_pr <- read_csv(here::here("maria_data/maria_supplied/bio_P_4dg_HW_summer_abs_bio.csv"), col_names = F)
abs_bio_ca <- read_csv(here::here("maria_data/maria_supplied/bio_Ca_4dg_HW_summer_abs_bio.csv"), col_names = F)
abs_bio_cp <- read_csv(here::here("maria_data/maria_supplied/bio_Cp_4dg_HW_summer_abs_bio.csv"), col_names = F)

# their index information
abs_pr_idx  <- read_csv(here::here("maria_data/maria_supplied/idx_P_4dg_HW_summer_abs_bio.csv"), col_names = F)
abs_ca_idx  <- read_csv(here::here("maria_data/maria_supplied/idx_Ca_4dg_HW_summer_abs_bio.csv"), col_names = F)
abs_cp_idx  <- read_csv(here::here("maria_data/maria_supplied/idx_Cp_4dg_HW_summer_abs_bio.csv"), col_names = F)



#### Data exploration:  ####



#### Reshaping Function  ####




# Need to reshaope into a datafram3
# Can make a key from the row-column positions 
# to reshaphe and join them together

# Function to match matrix of ID's to mmatrix of relative Biomass
label_bio_matrix <- function(id_matrix, bio_matrix, meta_df, bio_type){
  
  # Rename, reshape, and Join
  col_nums  <-  c(1:ncol(id_matrix))
  col_names <- str_c("col_", col_nums)
  year_ids  <- c(1:(nrow(id_matrix)))
  
  # Rebuild with column names and year labels
  species_2 <- map2(col_names, col_nums, function(nm, idx){ 
    data.frame(x = id_matrix[,idx]) %>% setNames(nm)}) %>% 
    bind_cols() %>% 
    mutate(year = year_ids, .before = col_names[1])%>% 
    pivot_longer(cols = -year, 
                 names_to = "position", 
                 values_to = "taxa_id")
  
  # Do the same with the biomass information
  bio_col <- switch(bio_type,
                    "relative" = "relative_bio",
                    "absolute" = "abs_bio")
  bio_2 <- map2(col_names, col_nums, function(nm, idx){ 
    data.frame(x = bio_matrix[,idx]) %>% setNames(nm)}) %>% 
    bind_cols() %>% 
    mutate(year = year_ids, .before = col_names[1]) %>% 
    pivot_longer(cols = -year, 
                 names_to = "position", 
                 values_to = bio_col)
  
  
  
  # This was a really dumb way to do this:
  # But anyways, join the species ID's to the relative biomass info
  dat_together <- full_join(species_2, bio_2, by = c("year", "position")) %>% 
    mutate(taxa_id = factor(taxa_id),
           year = factor(year))
  
  # Now add in the meta
  dat_together <- mutate(dat_together, taxa_id = as.character(taxa_id))
  meta_df <- mutate(meta_df, index = as.character(index))
  
  # Join with meta
  dat_labelled <- dat_together %>% left_join(meta_df, by = c("taxa_id" = "index")) %>% 
    select(-position)
  
  return(dat_labelled)
  
  
}


####  Reshape the Data  ####

# These are all protists

# # Reshape the assets into a dataframe
# main_biomass <- label_bio_matrix(id_matrix = species_name, bio_matrix = rel_biomass_species, meta_df = group_index)


#  a. Protists

# Relative biomass
prot_rel <- label_bio_matrix(id_matrix = ind_p, bio_matrix = rel_bio_prot, meta_df = group_index, bio_type = "relative") 
# Absolute biomass
prot_abs <- label_bio_matrix(id_matrix = abs_pr_idx, bio_matrix = abs_bio_pr, meta_df = group_index, bio_type = "absolute")


# b. Active Copepods
ca_rel <- label_bio_matrix(id_matrix = ind_ca, bio_matrix = rel_bio_ca, meta_df = group_index, bio_type = "relative")
ca_abs <- label_bio_matrix(id_matrix = abs_ca_idx, bio_matrix = abs_bio_ca, meta_df = group_index, bio_type = "absolute")

# c. Passive Copepods
cp_rel <- label_bio_matrix(id_matrix = ind_cp, bio_matrix = rel_bio_cp, meta_df = group_index, bio_type = "relative")
cp_abs <- label_bio_matrix(id_matrix = abs_cp_idx, bio_matrix = abs_bio_cp, meta_df = group_index, bio_type = "absolute")


# Join the relative and absolute bio information
prot_all <- full_join(prot_rel, prot_abs, by = c("year", "taxa_id", "species", "min_size", "max_size", "temp_opt"))
ca_all   <- full_join(ca_rel, ca_abs, by = c("year", "taxa_id", "species", "min_size", "max_size", "temp_opt"))
cp_all   <- full_join(cp_rel, cp_abs, by = c("year", "taxa_id", "species", "min_size", "max_size", "temp_opt"))


# e. Aggregate and track changes between years
bio_groups <- bind_rows(
  list(
    #"Major Taxa" = main_biomass,
    "Protists"         = prot_all,
    "Active Copepods"  = ca_all,
    "Passive Copepods" = cp_all
  ), .id = "classification") 




####  Supplementing  Data  ####


# Add a scaled biomass column: 
# within a taxa how have they changed relative to their average
bio_groups <- bio_groups %>% 
  split(.$taxa_id) %>% 
  map_dfr(function(x){
    avg_bio <- mean(x$abs_bio, na.rm = T)
    base_bio <- filter(x, year == 1) %>% pull(abs_bio)
    x %>% 
      mutate(avg_bio = avg_bio,
             y1_bio = base_bio,
             bio_z = (avg_bio - abs_bio) / avg_bio,
             y1_diff = abs_bio - y1_bio)
    
    
    
  })



# Add a change in biomass column
bio_groups <- bio_groups %>% 
  split(.$taxa_id) %>% 
  map_dfr(function(x){
    x %>% 
      arrange(year) %>% 
      mutate(
        rel_lagged = lag(relative_bio),
        rel_change = rel_lagged - relative_bio  ,
        rel_change = ifelse(is.na(rel_change), 0, rel_change),
        abs_lagged = lag(abs_bio),
        abs_change =  abs_lagged - abs_bio  ,
        abs_change = ifelse(is.na(abs_change), 0, abs_change)
      )
    
    
    
  })





# Classification
# Can we identify/classify response patterns based 
# on scaled changes


# What traits are associated with these
# winner/loser classifications
# This can be descriptive


####________####
#### Plotting  ####


####__ 1. Stacked Bars  ####

# Facet - Relative  Bio
ggplot(bio_groups) +
  geom_col(aes(year, relative_bio, fill = taxa_id), color = "black", size = .1) + 
  scale_fill_gmri() +
  facet_wrap(~ classification, nrow = 3, scales = "free") +
  labs(x = "Year", 
       y = "Relative Biomass",
       color = "Taxon ID") +
  theme(legend.position = "none") 


# Facet - Absolute  Bio
ggplot(bio_groups) +
  geom_col(aes(year, abs_bio, fill = taxa_id), color = "black", size = .1) + 
  scale_fill_gmri() +
  facet_wrap(~ classification, nrow = 3, scales = "free") +
  labs(x = "Year", 
       y = "Relative Biomass",
       color = "Taxon ID") +
  theme(legend.position = "none") 


####__ 2. Line Plots  ####


# Relative Bio
ggplot(bio_groups, aes(year, relative_bio, color = taxa_id)) +
  geom_point() +
  geom_line(aes(group = taxa_id)) +
  scale_color_gmri() +
  facet_wrap(~ classification, nrow = 3, scales = "free") +
  labs(x = "Year", 
       y = "Relative Biomass",
       color = "Taxon ID") +
  theme(legend.position = "none") 

# Absolute Biomass
ggplot(bio_groups, aes(year, abs_bio, color = taxa_id)) +
  geom_point() +
  geom_line(aes(group = taxa_id)) +
  scale_color_gmri() +
  facet_wrap(~ classification, nrow = 3, scales = "free") +
  labs(x = "Year", 
       y = "Absolute Biomass",
       color = "Taxon ID") +
  theme(legend.position = "none") 


# Scaled to mean Biomass
ggplot(bio_groups, aes(year, bio_z, color = taxa_id)) +
  #geom_point() +
  geom_line(aes(group = taxa_id)) +
  scale_color_gmri() +
  facet_wrap(~ classification, nrow = 3,scales = "free") +
  labs(x = "Year", 
       y = "Biomass Scaled to Mean",
       color = "Taxon ID") +
  theme(legend.position = "none") 

####__ 3. Streamplots  ####


# Protists
ggplot(prot_all) +
  geom_stream(aes(year, relative_bio, fill = taxa_id), 
              #type = "proportional",
              #sorting = "inside_out", 
              bw = 1) +
  scale_fill_gmri() +
  labs(x = "Year", 
       y = "Relative Biomass",
       subtitle = "Protists") +
  annotate(x = 2, y = 1.1, geom = "text", label = "Heatwave\nEvent") +
  geom_segment(aes(x = 2, xend = 2, y = 1.025, yend = 0)) +
  theme(legend.position = "bottom")




####__ 4. Heatmap  ####

# Relative Biomass
ggplot(bio_groups, aes(x = year, y = taxa_id, fill = relative_bio)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu") + 
  facet_wrap(~classification, nrow = 2, scales = "free") +
  labs(x = "Year", y = "Taxa ID")

# Absolute Biomass
ggplot(bio_groups, aes(x = year, y = taxa_id, fill = abs_bio)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", na.value = "white") + 
  facet_wrap(~classification, nrow = 2, scales = "free") +
  labs(x = "Year", y = "Taxa ID")

# Relative Change
ggplot(bio_groups, aes(x = year, y = taxa_id, fill = rel_change)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu") + 
  facet_wrap(~classification, nrow = 3, scales = "free") +
  labs(x = "Year", y = "Taxa ID")

# Absolute Change
bio_groups %>% 
  filter(classification == "Protists") %>% 
  ggplot(aes(x = year, y = taxa_id, fill = abs_change)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu") + 
  facet_wrap(~classification, nrow = 3, scales = "free") +
  labs(x = "Year", y = "Taxa ID")


hm_pas <- bio_groups %>% 
  filter(classification == "Passive Copepods") %>% 
  ggplot(aes(x = year, y = taxa_id, fill = abs_bio)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu") + 
  facet_wrap(~classification, nrow = 3, scales = "free") +
  labs(x = "Year", y = "Taxa ID")


hm_act <- bio_groups %>% 
  filter(classification == "Active Copepods") %>% 
  ggplot(aes(x = year, y = taxa_id, fill = abs_bio)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu") + 
  facet_wrap(~classification, nrow = 3, scales = "free") +
  labs(x = "Year", y = "Taxa ID")

hm_pr <- bio_groups %>% 
  filter(classification == "Protists") %>% 
  ggplot(aes(x = year, y = taxa_id, fill = abs_bio)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu") + 
  facet_wrap(~classification, nrow = 3, scales = "free") +
  labs(x = "Year", y = "Taxa ID")




hm_act / hm_pas / hm_pr

####__ 6. Treemap  ####

# Year 1
bio_groups %>%
  filter(
    #classification == "Protists",
    year == 1) %>%
treemap(
  dtf = .,
  index = "taxa_id",
  vSize = "abs_bio",
  vColor = "classification",
  title = "Absolute Biomass : Year 1",
  type = "categorical"
  )

bio_groups %>%
    filter(
      year == 2) %>%
    treemap(
      dtf = .,
      index = "taxa_id",
      vSize = "abs_bio",
      vColor = "classification",
      title = "Absolute Biomass : Year 2",
      type = "categorical"
    )

# ggplot treemaps
library(treemapify)
library(patchwork)

#y1_p | y2_p & plot_layout(guides = "collect")



####__ 5. Temperature Optimum  ####

# Each taxa has a temperature optima:
# How does this relate to its heatwave response

# Pull just how relative biomass changed from year 1-2 for each taxa
hw_response <- bio_groups %>% 
  group_by(taxa_facet = str_c(taxa_id, "_", "classification")) %>% 
  split(.$taxa_facet) %>% 
  map_dfr(function(x){
    x %>% filter(year %in% c("2"))
  })

# Plot response against size
ggplot(hw_response, aes(rel_change, max_size, color = classification)) +
  geom_point() +
  labs(x = "Max Size",
       y = "Biomass year 1 -> year 2 Change") +
  facet_wrap(~classification)


ggplot(hw_response, aes(temp_opt, max_size, color = classification)) +
  geom_point() +
  labs(x = "Temperature Optimum",
       y = "Biomass year 1 -> year 2 Change") +
  facet_wrap(~classification)


####__ 6. Size Responses  ####


####

# Ideas for plot(s):
# Want to see how diversity has changed in terms of total number of groups
# And if we see a lot of changes in specific groups, and also specific groups in terms of temperature
# year and temperature are related
# year 1 = before hw, 
# year 2 = hw, 
#  years 3+ = years after hw


# impact of warming:
# change from hw, and how long it takes to recover


# Components: Diversity, Heatwaves, Size Structure
