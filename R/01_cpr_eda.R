#### CPR Dataset Exploratory Data Analysis
#### Adam A. Kemberling
#### 11/11/2019

####  Packages  ####
library(tidyverse)
library(here)

####  Functions  ####
find_box_data <- function(box_project_name) {
  box_project <- as.character(box_project_name)
  box_path <- str_c("/Users/akemberling/Box/Adam Kemberling/Box_Projects/", paste(box_project))
  return(box_path)
}

cpr_boxpath <- find_box_data("continuous_plankton_recorder")

#Set ggplot theme
theme_set(theme_bw())

####  Load Data  ####
# Long form
cpr_all <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "cpr_allspecies_long.csv", sep = "/"))
# List form if we want to dig into any or perform operations to all
cpr_species <- cpr_all %>% group_by(species) %>% split(.$species)

####  Exploratory Data Analysis  ####
cpr_all %>% 
  filter(period == "annual") %>% 
  ggplot(aes(year, anomaly)) +
  geom_hline(yintercept = 0, color = "darkred", alpha = 0.5, linetype = 2) +
  geom_point(
    #aes(color = period)
    ) +
  geom_smooth(method = "loess", color = "gray25") +
  facet_wrap(~species)


cpr_all %>% filter(period == "annual") %>% 
  ggplot(aes(year, anomaly)) +
  geom_hline(yintercept = 0, color = "darkred", alpha = 0.5, linetype = 2) +
  geom_point(aes(color = species)) +
  geom_line(aes(color = species))



####  Wide form for each period  ####
cpr_wide <- cpr_all %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly)

annual_wide <- cpr_all %>% 
  filter(period == "annual") %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly)


#Make list of all of them
periods_wide <- cpr_all %>% 
  group_by(period) %>% 
  split(.$period) %>% 
  map(function(x) {
    data_wide <- x %>% pivot_wider(names_from = species, 
                                   values_from = anomaly)
    return(data_wide)
  })
