#### Quarterly SST Anomaly Correlation Plots with NOAA/SAHFOS CPR Data  ####
#### Adam A. Kemberling
#### 3/11/2020

####  Packages  ####
library(tidyverse)
library(here)
library(gmRi)
library(patchwork)
library(ggpmisc)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####  Load Data  ####
# CPR Dataset with quarterly anomalies and SST with a one-period lag
# souce: 03_new_anoms_quarterly_sst.R
cpr_sst <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "anomalies_w_quarterlysst.csv", sep = "/"),
                    col_types = cols(),
                    guess_max = 1e5)

# Reference Taxa
species_05 <- c("calanus_finmarchicus_v_vi", "centropages_typicus", "oithona_spp","para_pseudocalanus_spp", 
                "metridia_lucens", "calanus_i_iv", "euphausiacea_spp")

# Correlation vars - same but with temp
corr_vars <- c("calanus_finmarchicus_v_vi", "centropages_typicus", "oithona_spp","para_pseudocalanus_spp", 
               "metridia_lucens", "calanus_i_iv", "euphausiacea_spp", "temp_anomaly")

# Correlogram Color Scale
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))


####  Build Correlograms  ####

#Make list of the data groups
period_df_list <- list(
  "Q1"  = pull_period(cpr_long_df = cpr_sst, time_period = "Q1"),
  "Q2"  = pull_period(cpr_long_df = cpr_sst, time_period = "Q2"),
  "Q3"  = pull_period(cpr_long_df = cpr_sst, time_period = "Q3"),
  "Q4"  = pull_period(cpr_long_df = cpr_sst, time_period = "Q4")
)

# Pull individual periods and prep for correlation matrix
Q1 <- period_df_list[["Q1"]] %>% drop_na()
Q2 <- period_df_list[["Q2"]] %>% drop_na()
Q3 <- period_df_list[["Q3"]] %>% drop_na()
Q4 <- period_df_list[["Q4"]] %>% drop_na()


# Pull correlations with p-values - data for corrplots
Q1_corrs <- corr_plot_setup(Q1)
Q2_corrs <- corr_plot_setup(Q2)
Q3_corrs <- corr_plot_setup(Q3)
Q4_corrs <- corr_plot_setup(Q4)


# corrplots
q1_t <- cpr_corr_plot(Q1_corrs, period = "Q1", plot_style = "wide", taxa = species_05) 
q2_t <- cpr_corr_plot(Q2_corrs, period = "Q2", plot_style = "wide", taxa = species_05) + theme(axis.text.y = element_blank())
q3_t <- cpr_corr_plot(Q3_corrs, period = "Q3", plot_style = "wide", taxa = species_05) + theme(axis.text.y = element_blank())
q4_t <- cpr_corr_plot(Q4_corrs, period = "Q4", plot_style = "wide", taxa = species_05) + theme(axis.text.y = element_blank())

#Patch them together
quarterly_corrplot <- q1_t | q2_t | q3_t | q4_t
quarterly_corrplot <- quarterly_corrplot & theme(legend.position = "none")
quarterly_corrplot <- quarterly_corrplot + 
  labs(caption = "Correlations between quarterly mean SST and simulataneous abundance anomalies for focal taxa from CPR data.\n Years used in this analysis were from 1982 to 2017.")
quarterly_corrplot


# Save Figure
ggsave(quarterly_corrplot, 
       filename =  here::here("R", "new_anom_analyses", "figures", "sst_quarterly_corrplot.png"), 
       device = "png")