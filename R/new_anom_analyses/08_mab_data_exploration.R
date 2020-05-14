####  Mid Atlantic Bight Exploratory Data Analysis  ####
####  5/5/2020


####  Packages  ####
library(patchwork)
library(sf)
library(janitor)
library(tidyverse)
library(rnaturalearth)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))


####  Data  ####

####  1. MAB Consolidated  ####
#Reshaped Mid-Atlantic Bight dataset obtained from new_anom_analyses/07_mid_atlantic_data_reshape.R
source("R/new_anom_analyses/07_mid_atlantic_data_reshape.R")


####__####
####  Data Exploration ####


####  1. Spatial Coverage  ####

#devtools::install_github("https://github.com/ropensci/rnaturalearthhires") #need this to use ne_states()

#Shapefiles
usa <- ne_states(country = "united states of america") %>% st_as_sf() 
canada    <- ne_states(country = "canada") %>% st_as_sf()




#Mid-Atlantic data spatial coverage
mb_coverage <- mb_zoo %>% 
  mutate(year = factor(year)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  ggplot() +
  geom_sf(aes(color = year)) +
  geom_sf(data = usa) +
  geom_sf(data = canada) +
  coord_sf(xlim = c(-77, -66), ylim = c(35, 42)) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_blank()) +
  labs(caption = "Mid-Atlantic Bight CPR data")
mb_coverage

