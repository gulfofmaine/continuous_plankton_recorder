####  NOAA  / SAHFOS Allspecies EDA  ####


####  Packages  ####
library(patchwork)
library(sf)
library(tidyverse)


####  Data  ####

####  1. NOAA Consolidated  ####
#Reduced NOAA dataset obtained from 15_NOAA_CPR_Key.R
source("R/15_NOAA_CPR_Key.R")


####  2. SAHFOS Consolidated  ####

#SAHFOS data is converted to number per meters cubed and combined across subsampling methods in:
# 16_SAHFOS_CPR_Cleanup.R
source("R/16_SAHFOS_CPR_Cleanup.R")


####__####
####  Data Exploration - Spatial and temporal coverage  ####

#devtools::install_github("https://github.com/ropensci/rnaturalearthhires") #need this to use ne_states()

#Shapefiles
northeast <- ne_states(country = "united states of america") %>% st_as_sf() %>% filter(region == "Northeast")
canada <- ne_states(country = "canada") %>% st_as_sf()

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





###__####

#SAHFOS NOAA Abundance Comparisons

#Catch differences 
noaa_calanus <- noaa_zoo_2 %>% 
  group_by(year) %>% 
  dplyr::summarise(`calanus_v-vi` = sum(`calanus finmarchicus v-vi`, na.rm = T),
                   `calanus_i-iv` = sum(`calanus finmarchicus i-iv`, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(year, `calanus_v-vi`, color = "calanus_v-vi")) +
  geom_point(aes(year, `calanus_i-iv`, color = "calanus_i-iv")) +
  scale_y_continuous(labels = scales::comma_format()) +
  xlim(c(2010,2018)) +
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
  xlim(c(2010,2018)) +
  labs(caption = "source: SAHFOS",
       y = "C. finmarchicus",
       x = NULL)

noaa_calanus / sahfos_calanus