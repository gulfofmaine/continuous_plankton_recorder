#### Exploratory Data Analysis of NOAA+SAHFOS Anomalies
#### Anomalies Calculated using splines generated in R
#### Adam A. Kemberling
#### 2/10/2019

####  Packages  ####
library(tidyverse)
library(here)
library(gmRi)
library(patchwork)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))
ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)


####  New Data  ####
cpr_wide <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "detrended_anomalies_noaa_sahfos.csv", sep = "/"), 
                     guess_max = 1e6, 
                     col_types = cols())
cpr_long <- cpr_wide %>% pivot_longer(names_to = "taxa", values_to = "anomaly", cols = 5:ncol(.))

#Reference Taxa
species_05 <- c("calanus_i_iv", 
                "calanus_finmarchicus_v_vi", 
                "centropages_typicus", 
                "chaetognatha_eyecount",
                "euphausiacea_spp", 
                "metridia_lucens", 
                "oithona_spp", 
                "para_pseudocalanus_spp", 
                "paraeuchaeta_norvegica", 
                "temora_longicornis")

####  Load Refernce Data  ####
# Long form
cpr_all <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "cpr_allspecies_long.csv", sep = "/"),
                    col_types = cols())
# List form if we want to dig into any or perform operations to all
cpr_species <- cpr_all %>% group_by(species) %>% split(.$species)


####  Exploratory Data Analysis  ####
refs <- cpr_all %>% 
  filter(period == "annual") %>% 
  ggplot(aes(year, anomaly)) +
  geom_hline(yintercept = 0, color = "darkred", alpha = 0.5, linetype = 2) +
  geom_point() +
  geom_smooth(method = "loess", color = "gray25") +
  facet_wrap(~species) +
  labs(title = "Reference", x = NULL, y = NULL)

newbies <- cpr_long %>% 
  filter(period == "annual",
         taxa %in% species_05) %>% 
  ggplot(aes(year, anomaly)) +
  geom_hline(yintercept = 0, color = "darkred", alpha = 0.5, linetype = 2) +
  geom_point() +
  geom_smooth(method = "loess", color = "gray25") +
  facet_wrap(~taxa) +
  labs(title = "New Anomalies", x = NULL, y = NULL)

refs + newbies
#Looks pretty Good!


#What about correlation plots
ref_anoms <- cpr_all %>% filter(period == "annual") %>% mutate(source = "MATLAB splines")
new_anoms <- cpr_long %>% 
  filter(
    period == "annual",
    taxa %in% species_05) %>% 
  mutate(source = "R splines",
         taxa = case_when(
           taxa == "calanus_i_iv" ~ "calanus1to4",
           taxa == "calanus_finmarchicus_v_vi" ~ "calanus",
           taxa == "centropages_typicus" ~ "centropages",
           taxa == "chaetognatha_eyecount" ~ "chaetognatha",
           taxa == "euphausiacea_spp" ~ "euphausiacea",
           taxa == "metridia_lucens" ~ "metridia",
           taxa == "oithona_spp" ~ "oithona",
           taxa == "para_pseudocalanus_spp" ~ "para_pseudocalanus",
           taxa == "paraeuchaeta_norvegica" ~ "paraeucheata",
           taxa == "temora_longicornis" ~ "temora",
           TRUE ~ "missed one"
         )
) %>% rename(species = taxa) 

comparison_df <- bind_rows(ref_anoms, new_anoms)

#Overlay Plot
comparison_df %>% 
  ggplot(aes(year, anomaly, color = source)) +
  geom_point() + 
  facet_wrap(~species) +
  theme(legend.position = c(0.8,0.15)) +
  labs(x = NULL, y = NULL) 

#R-squared plot
library(ggpmisc)
source_compare <- function(taxa) {
  ref_df <- comparison_df %>% filter(species == taxa, source == "MATLAB splines") %>% 
    select(species, year, period, anom_matlab = anomaly,-c(datebounds, period_anom_n))
  new_df <- comparison_df %>% filter(species == taxa, source == "R splines") %>% 
    select(species, year, period, anom_r = anomaly,-c(datebounds, period_anom_n))
  
  both_df <- left_join(ref_df, new_df, by = c("species", "year", "period"))
  
  ggplot(both_df, aes(anom_r, anom_matlab)) +
    geom_point() +
    labs(x ="R Anomalies",
         y ="MATLAB Anomalies",
         title = taxa) +
    geom_smooth(formula = y ~ x,
                method = "lm", 
                se = FALSE) +
    stat_poly_eq(formula = y ~ x,
                 aes(x = anom_r, y = anom_matlab, label = ..rr.label..),
                 label.y = 0.1,
                 label.x = 0.9,
                 parse = TRUE)
    
}


source_compare("calanus1to4")


rsquare_list <- map(unique(ref_anoms$species), source_compare)
names(rsquare_list) <- unique(ref_anoms$species)
length(rsquare_list)

r1 <- rsquare_list[[1]] + labs(x = "") | rsquare_list[[2]] + labs(x = "", y = "") | rsquare_list[[3]] + labs(x = "", y = "")
r2 <- rsquare_list[[4]] + labs(x = "") | rsquare_list[[5]] + labs(x = "", y = "") | rsquare_list[[6]] + labs(x = "", y = "")
r3 <- rsquare_list[[7]] + labs(x = "") | rsquare_list[[8]] + labs(y = "") | rsquare_list[[9]] + labs(y = "")
r4 <- rsquare_list[[10]] | plot_spacer() | plot_spacer()
r1 / r2 / r3 / r4
