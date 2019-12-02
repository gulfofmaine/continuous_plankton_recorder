#### CPR Dataset - Principal Component Analysis - QUARTERLY Anomalies
#### Adam A. Kemberling
#### 12/02/2019

####  Packages  ####
library(ggbiplot)
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

cpr_wide <- cpr_long %>%
  filter(is.na(anomaly) == FALSE) %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly) 



####  Quarterly Species Abundance Timelines  ####
species_list <- list(
  "calanus" = filter(cpr_long, species == "calanus"),
  "calanus1to4" = filter(cpr_long, species == "calanus1to4"),
  "centropages" = filter(cpr_long, species == "centropages"),
  "euphausiacea" = filter(cpr_long, species == "euphausiacea"),
  "metridia" = filter(cpr_long, species == "metridia"),
  "oithona" = filter(cpr_long, species == "oithona"),
  "para_pseudocalanus" = filter(cpr_long, species == "para_pseudocalanus")
)

#Export Timelines
imap(species_list, function(x, y) {
  species_plot <- ggplot(x, aes(year, anomaly)) +
    geom_hline(yintercept = 0, alpha = 0.3, color = "darkred", linetype = 2) +
    geom_point(color = gmri_cols("gmri blue")) +
    geom_line(color = gmri_cols("gmri blue"), group = 1) +
    labs(x = NULL, y = NULL, caption = y) +
    facet_wrap(~period, ncol = 2)
  
  ggsave(plot = species_plot, filename = here::here("R", "presentations", "quarterly_timelines", str_c(y, ".png")), device = "png")
  
})
