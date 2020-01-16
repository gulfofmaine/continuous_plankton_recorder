####  NOAA  / SAHFOS Allspecies EDA  ####


####  Packages  ####
library(patchwork)
library(sf)
library(janitor)
library(tidyverse)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))


####  Data  ####

####  1. NOAA Consolidated  ####
#Reduced NOAA dataset obtained from 15_NOAA_CPR_Key.R
source("R/15_NOAA_CPR_Key.R")


####  2. SAHFOS Consolidated  ####

#SAHFOS data is converted to number per meters cubed and combined across subsampling methods in:
# 16_SAHFOS_CPR_Cleanup.R
source("R/16_SAHFOS_CPR_Cleanup.R")


####__####
####  Data Exploration ####



####  1. Spatial Coverage  ####

#devtools::install_github("https://github.com/ropensci/rnaturalearthhires") #need this to use ne_states()

#Shapefiles
northeast <- ne_states(country = "united states of america") %>% st_as_sf() %>% filter(region == "Northeast")
canada    <- ne_states(country = "canada") %>% st_as_sf()

#Noaa spatial coverage
noaa_coverage <- noaa_zoo_2 %>% 
  mutate(`longitude (degrees)` = -1 * `longitude (degrees)`,
         decade = floor_decade(year)) %>% 
  st_as_sf(coords = c("longitude (degrees)", "latitude (degrees)"), crs = 4326, remove = FALSE) %>% 
  ggplot() +
  geom_sf(aes(color = decade)) +
  geom_sf(data = northeast) + 
  geom_sf(data = canada) + 
  coord_sf(xlim = c(-71, -62.5), ylim = c(41,44.5)) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_blank()) +
  labs(caption = "NOAA CPR data")


#Sahfos data spatial coverage
sahfos_coverage <- sahfos_zoo %>% 
  mutate(year = factor(year)) %>% 
  st_as_sf(coords = c("longitude (degrees)", "latitude (degrees)"), crs = 4326, remove = FALSE) %>% 
  ggplot() +
  geom_sf(aes(color = year)) +
  geom_sf(data = northeast) +
  geom_sf(data = canada) +
  coord_sf(xlim = c(-71, -62.5), ylim = c(41,44.5)) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_blank()) +
  labs(caption = "SAHFOS CPR data")


#Plot them together
noaa_coverage / sahfos_coverage

#If we decide to clip the SAHFOS data we can use this:
noaa_zoo_2 %>% 
  mutate(`longitude (degrees)` = -1 * `longitude (degrees)`,
         decade = floor_decade(year)) %>% 
  st_as_sf(coords = c("longitude (degrees)", "latitude (degrees)"), crs = 4326, remove = FALSE) %>% 
  st_bbox(noaa_zoo_2 %>% st_as_sf())


###  2. Abundance Units  ####

#Catch differences 
noaa_calanus <- noaa_zoo_2 %>% 
  group_by(year) %>% 
  dplyr::summarise(`calanus_v-vi` = sum(`calanus finmarchicus v-vi`, na.rm = T),
                   `calanus_i-iv` = sum(`calanus finmarchicus i-iv`, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(year, `calanus_v-vi`, color = "calanus_v-vi")) +
  geom_point(aes(year, `calanus_i-iv`, color = "calanus_i-iv")) +
  scale_y_continuous(labels = scales::comma_format()) +
  #xlim(c(2010,2018)) +
  labs(caption = "source: NOAA",
       y = "C. finmarchicus",
       x = NULL)

sahfos_calanus <-  sahfos_zoo %>% 
  group_by(year) %>% 
  dplyr::summarise(`calanus_v-vi` = sum(`calanus finmarchicus`, na.rm = T),
                   `calanus_i-iv` = sum(`calanus i-iv`, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(year, `calanus_v-vi`, color = "calanus_v-vi")) +
  geom_point(aes(year, `calanus_i-iv`, color = "calanus_i-iv")) +
  scale_y_continuous(labels = scales::comma_format()) +
  #xlim(c(2010,2018)) +
  labs(caption = "source: SAHFOS",
       y = "C. finmarchicus",
       x = NULL)

noaa_calanus / sahfos_calanus



####  Taxon Mismatches  ####


#Quick comparison test of which columns match and/or are missing from either
compare_df_cols(noaa_zoo_2, sahfos_zoo)


#Renaming of sahfos data to match the refined noaa list
sahfos_zoo_2 <- sahfos_zoo %>% 
  rename(
    `acartia spp.` = `acartia spp. (unidentified)`,
    `amphipoda spp.` = `amphipoda (unidentified)`,
    `appendicularia spp.` = appendicularia,
    `bivalvia spp.` = `bivalvia larvae`,
    `calanus finmarchicus v-vi` = `calanus finmarchicus`,
    `calanus finmarchicus i-iv` = `calanus i-iv`,
    `calanus spp.` = `calanus v-vi unidentified`, 
    `centropages spp.` = `centropages spp. (unidentified)`,
    `cumacea spp.` = cumacea,
    `doliolidae spp.` = doliolidae,
    `euchaeta spp.` = `euchaetidae (unidentified)`,
    `euphausiacea spp.` = `euphausiacea total`,
    `foraminifera spp.` = `foraminifera (total)`,
    `gammaridea spp.` = gammaridea,
    `gastropoda spp.` = `gastropoda (unidentified)`,
    `gymnosomoata spp.` = `gymnosomata (unidentified)`,
    `harpacticoida spp.` = `harpacticoida total traverse`,
    `hyperiidea spp.` = `hyperiidea (total)`,
    `ischnocalanus spp.` =  ischnocalanus,
    `lepas spp.` = `lepas nauplii`,
    `metridia spp.` = `metridia spp. (v-vi) (unidentified)`,
    `monstrilloida spp.` = monstrilloida,
    `ostracoda spp.` = ostracoda,
    `pleuromamma spp.` = `pleuromamma spp. (unidentified)`,
    `polychaeta larva` = `polychaete larvae (unidentified)`,
    `salpidae spp.` = `salpidae (total)`,
    `siphonostomatoida spp.` = siphonostomatoida,
    `thecosomata spp.` = `thecosomata (north atlantic)`,
    `tintinnidae spp.` = `tintinnida total`
    
    
    ) %>% 
  #This section is for when multiple columns need to be reduced to a single aggregate
  mutate(
    `candacia spp.` = `candacia i-iv` + `candacia spp. (unidentified)`, 
    `candacia i-iv` = NULL,
    `candacia spp. (unidentified)` = NULL,
    `copepoda spp.` = `copepod eggs` + `copepod nauplii`,
    `copepod eggs` = NULL,
    `copepod nauplii` = NULL,
    `decapoda spp.` = `decapod megalopa` + `decapod zoea` + `decapoda larvae (total)`,
    `decapod megalopa` = NULL, 
    `decapod zoea` = NULL,
    `decapoda larvae (total)` = NULL,
    `fish eggs` = `fish eggs (total)` + `fish eggs with oil globules` + `fish eggs without oil globules`,
    `fish eggs (total)` = NULL,
    `fish eggs with oil globules` = NULL,
    `fish eggs without oil globules` = NULL,
    `pseudocalanus spp.` = `pseudocalanus spp. adult atlantic` + `pseudocalanus spp. adult total`,
    `pseudocalanus spp. adult atlantic` = NULL, 
    `pseudocalanus spp. adult total` = NULL,
    `radiolaria spp.` = `radiolaria non-acantharian` + `radiolaria total`,
    `radiolaria non-acantharian` = NULL,
    `radiolaria total` = NULL,
    
  )




####  And Voila  ####
compare_df_cols(noaa_zoo_2, sahfos_zoo_2)


combined_set <- bind_rows(list("NOAA" = noaa_zoo_2, "SAHFOS" =  sahfos_zoo_2), .id = "agency")
combined_set %>%
  ggplot(aes(year, `calanus finmarchicus i-iv`, color = agency)) +
    geom_point()


