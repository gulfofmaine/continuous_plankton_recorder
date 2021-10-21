#### Exploratory Data Analysis of NOAA+SAHFOS Anomalies
#### Anomalies Calculated using splines generated in R
#### Adam A. Kemberling
#### 2/10/2019

####  Packages  ####
library(tidyverse)
library(here)
library(gmRi)
library(ggpmisc)
library(patchwork)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))
ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)


####  New Data  ####

# Combined dataset from NOAA/SAHFOS, concentrations in common units: # / meters cubed
# Source: 17_noaa_sahfos_eda.R
cpr_abund <- read_csv(str_c(ccel_boxpath, "Data/Gulf of Maine CPR/2020_combined_data/zooplankton_combined.csv"), 
                      guess_max = 1e6, col_types = cols())


# Anomalies from seasonal splines: source: 19_cpr_splines.R
cpr_wide <- read_csv(str_c(ccel_boxpath, "Data/Gulf of Maine CPR/2020_combined_data/detrended_anomalies_noaa_sahfos.csv"), 
                     guess_max = 1e6, 
                     col_types = cols())

# Long form
cpr_long <- cpr_wide %>% 
  pivot_longer(names_to = "taxa", values_to = "anomaly", cols = 5:ncol(.))


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

####  Load Reference Data  ####
# Long form
cpr_all <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "cpr_allspecies_long.csv", sep = "/"),
                    col_types = cols())
# List form if we want to dig into any or perform operations to all
cpr_species <- cpr_all %>% group_by(species) %>% split(.$species)


####  Exploratory Data Analysis  ####




####__1.  Direct Comparison of Anomaly Timeseries  ####
refs <- cpr_all %>% 
  filter(period == "annual") %>% 
  ggplot(aes(year, anomaly)) +
  geom_hline(yintercept = 0, color = "darkred", alpha = 0.5, linetype = 2) +
  geom_point() +
  geom_smooth(formula = y~x, method = "loess", color = gmri_cols("orange")) +
  facet_wrap(~species) +
  labs(title = "Reference", x = NULL, y = NULL)

newbies <- cpr_long %>% 
  filter(period == "annual",
         taxa %in% species_05) %>% 
  ggplot(aes(year, anomaly)) +
  geom_hline(yintercept = 0, color = "darkred", alpha = 0.5, linetype = 2) +
  geom_point() +
  geom_smooth(formula = y~x, method = "loess", color = gmri_cols("orange")) +
  facet_wrap(~taxa) +
  labs(title = "New Anomalies", x = NULL, y = NULL)

refs + newbies
#Looks pretty Good!




####__2. Species Correlation Plots  ####
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

####
#R-squared compaarison  plot
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
                 parse = TRUE)}

####

# test with calanus
source_compare("calanus1to4")


rsquare_list <- map(unique(ref_anoms$species), source_compare)
names(rsquare_list) <- unique(ref_anoms$species)
length(rsquare_list)

r1 <- rsquare_list[[1]] + labs(x = "") | rsquare_list[[2]] + labs(x = "", y = "") | rsquare_list[[3]] + labs(x = "", y = "")
r2 <- rsquare_list[[4]] + labs(x = "") | rsquare_list[[5]] + labs(x = "", y = "") | rsquare_list[[6]] + labs(x = "", y = "")
r3 <- rsquare_list[[7]] + labs(x = "") | rsquare_list[[8]] + labs(y = "") | rsquare_list[[9]] + labs(y = "")
r4 <- rsquare_list[[10]] | plot_spacer() | plot_spacer()
r1 / r2 / r3 / r4








####  Coarse Overalll Metrics of New Data  ####



####__1. Most Abundant Taxa in GOM  ####
abund_long <- cpr_abund  %>% pivot_longer(names_to = "taxa", values_to = "cpr_density", cols = `phytoplankton color index`:`radiolaria spp.`)

# overall ranks
top_ranks <- abund_long %>% 
  mutate(samp_occurrence = ifelse(cpr_density > 0, 1, 0)) %>% 
  group_by(taxa) %>%
  summarise(mean_dens = mean(cpr_density, na.rm = T),
            perc_occurrence = mean(samp_occurrence, na.rm = T),
            total_occurrence = sum(samp_occurrence, na.rm = T),
            total_dens = sum(cpr_density, na.rm = T)) %>% 
  ungroup()

# Percent Occurrence in CPR
top_ranks %>% slice_max(n = 10, order_by = perc_occurrence) %>% select(taxa, perc_occurrence)

# Avg. density
top_ranks %>% slice_max(n = 10, order_by = mean_dens) %>% select(taxa, mean_dens)

# Highest Occurrence
top_ranks %>% slice_max(n = 15, order_by = total_occurrence) %>% select(taxa, total_occurrence)

# Yearly summaries
abund_summs <- abund_long %>% 
  mutate(samp_occurrence = ifelse(cpr_density > 0, 1, 0)) %>% 
  group_by(year, taxa) %>%
  summarise(n_transects = n_distinct(station),
            mean_dens = mean(cpr_density, na.rm = T),
            perc_occurrence = mean(samp_occurrence, na.rm = T),
            total_occurrence = sum(samp_occurrence, na.rm = T),
            total_dens = sum(cpr_density, na.rm = T)) %>% 
  ungroup()






# Top Ten density
abund_summs %>% 
  group_by(year) %>% 
  slice_max(n = 5, order_by = mean_dens, .preserve = T) %>% 
  ungroup() %>% 
  mutate(taxa = fct_reorder(.f = taxa, .x = mean_dens, .fun = mean, .desc = T)) %>% 
  ggplot(aes(year, mean_dens, color = taxa)) +
    geom_line(aes(group = 1), show.legend = F) +
    geom_point(show.legend = F) +
    facet_wrap(~taxa) + 
    labs(x = "", y = "Avg. Transect Density", caption = "Top 5 taxa taken from each year. Gaps in timeseries indicate absence from top 5.")



####__2. Seasonal Peaks  ####





####__3. Phytoplankton Color Index  ####






