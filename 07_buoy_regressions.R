# Buoy x CPR data Relationship
# 12/3/2019

#### CPR Dataset - Principal Component Analysis - QUARTERLY Anomalies
#### Adam A. Kemberling
#### 12/02/2019

####  Packages  ####
library(ggpmisc)
library(tidyverse)
library(here)
#devtools::install_github("vqv/ggbiplot")

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

#Set ggplot theme
theme_set(theme_classic())

####  Load Data  ####
cpr_long <- read_csv(str_c(cpr_boxpath,"data", "processed_data", "cpr_allspecies_long_quarters.csv", sep = "/")) %>% 
  mutate(
    period = case_when(
      period == "annual" ~"Annual",
      period == "q1" ~"Q1",
      period == "q2" ~"Q2",
      period == "q3" ~"Q3",
      period == "q4" ~"Q4",
      TRUE ~ "Missed One"
    )
  )

buoy <- read.csv(str_c(cpr_boxpath, "data/processed_data/buoys_aggregated.csv", sep = "/"))



####  Pair cpr data with quarterly measurements  ####
cpr_buoys <- cpr_long %>%
  filter(period != "Annual") %>% 
  left_join(buoy, by = c("year", "period")) %>% 
  mutate(reading_depth = factor(reading_depth), 
         levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters"))


#What information do we want to highlight?
cpr_buoys %>% 
  filter(is.na(buoy_id) == FALSE,
         #period  == "Q1",
         species == "calanus") %>% 
  ggplot(aes(mean_strat_index, anomaly)) +
    geom_smooth(method = "lm", se = FALSE, color = "gray50") +
    geom_point() +
    stat_poly_eq(formula = y ~ x, 
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +  
    facet_grid(period ~ buoy_id) +
    labs(x = "Stratification Index ()",
         y = "Population Anomaly (sd)") +
    theme_bw()
    
