# Buoy x CPR data Relationship
# 12/3/2019

#### CPR Dataset - Principal Component Analysis - QUARTERLY Anomalies
#### Adam A. Kemberling
#### 12/02/2019

####  Packages  ####
library(ggpmisc)
library(tidyverse)
library(here)

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
  mutate(reading_depth = factor(reading_depth, 
         levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters"))
  )


####  Export cpr_buoys  ####
write_csv(cpr_buoys, path = str_c(cpr_boxpath,"data", "processed_data", "cpr_quarters_buoys.csv", sep = "/"), col_names = TRUE)
write_csv(cpr_buoys, path = here::here("R", "cpr_buoy_DE", "Data", "cpr_quarters_buoys.csv"), col_names = TRUE)


####  Data Exploration  ####

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
    facet_grid(period ~ buoy_id, scales = "free") +
    labs(x = "Stratification Index ()",
         y = "Population Anomaly (sd)") +
    theme_bw()
    


#Buoy N is the furthest offshore
#Buoy M isn't loading correctly - but would be the next best
# B, E, & I are a little offshore
# F is in penobscot bay

#Offshore Stratification - Buoy N
species_l <- list(
  species_01 =  c("calanus", "calanus1to4", "centropages", "euphausiacea"),
  species_02 = c("metridia", "oithona", "para_pseudocalanus", "paraeucheata"))

cpr_buoys %>% 
  #Choose a Buoy
  filter(buoy_id == "Buoy_N") %>% 
  #Choose a species group
  filter(species %in% species_l$species_01) %>% 
  ggplot(aes(mean_strat_index, anomaly)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray50") +
  geom_point() +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  
  facet_grid(period ~ species) +
  labs(x = "Stratification Index",
       y = "Population Anomaly (sd)") +
  theme_bw()



#Buoy Regression super table - Attempt 1
confusing_matrix <- cpr_buoys %>% 
  filter(is.na(buoy_id) == FALSE) %>% 
  split(.$species) %>% 
  map(~ .x %>% split(.$period) %>% 
    map(~ .x %>% split(.$buoy_id) %>% 
      map(~ .x %>%  split(.$reading_depth) %>% 
        map(~ .x %>% 
              select(anomaly, mean_temp, mean_sal, mean_dens) %>% 
              drop_na() %>% 
              cor())
              )
        
          )
    )


#We now have correlation matrices for any combination of taxa, quarter, buoy, and depth
confusing_matrix$calanus$Q2$Buoy_M$`1 meter`

####  Attempt 2  ####
confusing_matrix <- cpr_buoys %>% 
  filter(is.na(buoy_id) == FALSE) %>% 
  split(.$year) %>% 
      map(~ .x %>% split(.$period) %>% 
        map(~ .x %>% 
              pivot_wider(names_from = species, values_from = anomaly) %>% 
                select(-year, -period, -buoy_id, -reading_depth) %>% 
                drop_na() %>% 
                cor()
              )
          
        )


#Patching them together
ggplot(cpr_buoys %>% filter(is.na(buoy_id) == FALSE, species == "calanus"), aes(year, anomaly, color = period)) +
  geom_line()
append(confusing_matrix$`2001`$Q3
,confusing_matrix$`2001`$Q4)
