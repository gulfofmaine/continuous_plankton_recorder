#### Investigating NERACOOS Buoy Data Relationships 
# with NOAA/SAHFOS CPR Data  ####
#### Adam A. Kemberling
#### 3/11/2020
####  NOTE: Buoy daily measurements and the buoy daily PCA are unaffected by CPR data and are still valid as is

####  Packages  ####
library(ggbiplot)
library(tidyverse)
library(here)
library(gmRi)
library(patchwork)
library(ggpmisc)



####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))
#ccel_boxpath <- shared.path(group = "Climate Change Ecology Lab", folder = "")
#cpr_boxpath <- "/Users/akemberling/Box/Adam Kemberling/Box_Projects/continuous_plankton_recorder"

####  Load Data  ####

# CPR Dataset with quarterly anomalies and SST with a one-period lag
# souce: 03_new_anoms_quarterly_sst.R

cpr_sst <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "anomalies_w_quarterlysst.csv", sep = "/"),
                    col_types = cols(),
                    guess_max = 1e5)


#Reference Taxa
species_05 <- c(
  "calanus_finmarchicus_v_vi", 
  "centropages_typicus", 
  "oithona_spp",
  "para_pseudocalanus_spp", 
  "metridia_lucens", 
  "calanus_i_iv", 
  "euphausiacea_spp")

# Subset to the main species
cpr_sst <- cpr_sst %>% filter(taxa %in% species_05)


# Load Buoy Data
buoy <- read.csv(str_c(cpr_boxpath, "data/processed_data/buoy_anomalies.csv", sep = "/")) %>% 
  mutate(reading_depth = factor(
    reading_depth, levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters")
  ))


####  Data Reshaping  ####

# Pair cpr data with quarterly measurements
cpr_buoys <- cpr_sst %>%
  filter(period != "annual") %>% 
  left_join(buoy, by = c("year", "period")) %>% 
  filter(is.na(buoy_id) == FALSE)

# clean environment so there's no name conflicts
rm("buoy")



# Reshape data so we have a column for each buoy-depth-measurement
corr_df <- cpr_buoys %>% 
  mutate(buoy_id = str_replace_all(buoy_id, "Buoy_", ""),
         reading_depth = str_replace_all(reading_depth, " meter", ""),
         reading_depth = str_replace_all(reading_depth, "s", ""),
         reading_depth = str_pad(reading_depth, 3, side = "left", pad = "0")) %>% 
  select(taxa, year, period, pop_anom = anomaly, buoy = buoy_id, depth = reading_depth, t = temp_anom,s = sal_anom)


# Final datasets for PCA, and for correlation tables by quarter
corr_setup_full <- corr_df %>% 
  pivot_wider(names_from = taxa, values_from = pop_anom) %>% 
  pivot_wider(names_from = c(buoy, depth), values_from = c(t, s))

Q1 <- corr_setup_full %>% filter(period == "Q1") %>% drop_na()
Q2 <- corr_setup_full %>% filter(period == "Q2") %>% drop_na()
Q3 <- corr_setup_full %>% filter(period == "Q3") %>% drop_na()
Q4 <- corr_setup_full %>% filter(period == "Q4") %>% drop_na()


# data for corrplots
Q1_corrs <- corr_plot_setup(select(Q1, -period))
Q2_corrs <- corr_plot_setup(select(Q2, -period))
Q3_corrs <- corr_plot_setup(select(Q3, -period))
Q4_corrs <- corr_plot_setup(select(Q4, -period))


####__####

####  Quarterly Correlogram  ####
q1_t <- cpr_corr_plot(Q1_corrs, period = "Q1", plot_style = "tall", taxa = species_05)
q2_t <- cpr_corr_plot(Q2_corrs, period = "Q2", plot_style = "tall", taxa = species_05) + theme(axis.text.y = element_blank())
q3_t <- cpr_corr_plot(Q3_corrs, period = "Q3", plot_style = "tall", taxa = species_05) + theme(axis.text.y = element_blank())
q4_t <- cpr_corr_plot(Q4_corrs, period = "Q4", plot_style = "tall", taxa = species_05) + theme(axis.text.y = element_blank())

#Patch them together
quarterly_corrplot <- q1_t | q2_t | q3_t | q4_t
quarterly_corrplot <- quarterly_corrplot & theme(legend.position = "none")
quarterly_corrplot

# #Export
# ggsave(quarterly_corrplot, 
#        filename =  here::here("R", "new_anom_analyses", "figures", "buoy_quarterly_corrplot.png"), 
#        device = "png",
#        height = 8, width = 8, units = "in")




####__####

####  Quarterly Buoy PCA - not in paper  ####

buoy_pca_dat <- corr_setup_full %>% drop_na()

#Perform PCA
pca_buoys <- prcomp(x = select(buoy_pca_dat, -year, -period), 
                    center = F, 
                    scale. = F)
summary(pca_buoys)

#Bi-Plot
buoy_pca_dat$decade <- factor(floor_decade(buoy_pca_dat$year))
ggbiplot(pca_buoys, 
         ellipse=TRUE, 
         labels = buoy_pca_dat$year,
         groups = buoy_pca_dat$decade, 
         obs.scale = T, 
         var.scale = T)




