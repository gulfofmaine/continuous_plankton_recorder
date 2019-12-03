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

# #Export Timelines
# imap(species_list, function(x, y) {
#   species_plot <- ggplot(x, aes(year, anomaly)) +
#     geom_hline(yintercept = 0, alpha = 0.3, color = "darkred", linetype = 2) +
#     geom_point(color = gmri_cols("gmri blue")) +
#     geom_line(color = gmri_cols("gmri blue"), group = 1) +
#     labs(x = NULL, y = NULL, caption = y) +
#     facet_wrap(~period, ncol = 2)
#   
#   ggsave(plot = species_plot, filename = here::here("R", "presentations", "quarterly_timelines", str_c(y, ".png")), device = "png")
#   
# })





####  Quarterly SST Measures  ####
sst <- read.table(str_c(cpr_boxpath, "data", "ENV", "GoMSST_quartlery.txt", sep = "/")) %>% 
  dplyr::rename(year = V1,
                Q1   = V2,
                Q2   = V3,
                Q3   = V4,
                Q4   = V5)

#Pair cpr data with quarterly anomalies
sst_long <- sst %>% pivot_longer(names_to = "period", cols = Q1:Q4, values_to = "temp_anomaly")


cpr_sst <- cpr_long %>%
  filter(period != "Annual") %>% 
  left_join(sst_long, by = c("year", "period"))


#Timelines
cpr_sst %>% 
  filter(species == "calanus") %>% 
  ggplot(aes(year, anomaly, fill = temp_anomaly)) + 
    geom_hline(yintercept = 0, linetype = 2, color = "gray", alpha = 0.3) +
    geom_col(color = "gray50", size = 0.1) +
    scale_fill_gradientn(colours = c("steelblue", "white", "darkred")) +
    scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
    facet_grid(period~ species) +
    labs(x = NULL, y = NULL) +
    theme(panel.grid.major.x = element_line(color = "gray80", size = 0.2),
          panel.grid.minor.x = element_line(color = "gray80", size = 0.2))

#Correlations

cpr_sst %>% 
  split(.$species) %>% 
  map(~
    .x %>% 
      ggplot(aes(anomaly, temp_anomaly, fill = temp_anomaly)) + 
      geom_point(shape = 21, color = "gray50") +
      scale_fill_gradientn(colours = c("steelblue", "white", "darkred")) +
      facet_grid(period ~ species) +
      theme_bw()
)

