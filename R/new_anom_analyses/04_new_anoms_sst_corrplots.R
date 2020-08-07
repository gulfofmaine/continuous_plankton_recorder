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

# # Reference Taxa
# species_05 <- c("calanus_finmarchicus_v_vi", "centropages_typicus", "oithona_spp","para_pseudocalanus_spp", 
#                 "metridia_lucens", "calanus_i_iv", "euphausiacea_spp")

#New levels for taxa plots, ordered by size with calanus together
species_05 <- c("Calanus I-IV", "Calanus finmarchicus V-VI", "Centropages typicus",
                "Oithona spp.","Para-Pseudocalanus spp.",
                "Metridia lucens",  "Euphausiacea spp.")
species_05 <- factor(species_05, levels = species_05)


# Correlation vars - same but with temp
corr_vars <- c(species_05, "temp_anomaly")

# Correlogram Color Scale
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))


# Add some label formatting
cpr_sst <- cpr_sst %>% 
  mutate(taxa = stringr::str_to_sentence(taxa),
         taxa = str_replace_all(taxa, "Para_pseu", "Para-Pseu"),
         taxa = str_replace_all(taxa, "i_iv", "I-IV"),
         taxa = str_replace_all(taxa, "v_vi", "V-VI"),
         taxa = str_replace_all(taxa, "_", " "),
         taxa = str_replace_all(taxa, "spp", "spp."),
         taxa = factor(taxa, levels = species_05)) %>% 
  filter(taxa %in% species_05)





####  Build Correlograms  ####

####__Original Corrplot  ####

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
  labs(caption = "No temporal lag")
quarterly_corrplot


# Save Figure
ggsave(quarterly_corrplot, 
       filename =  here::here("R", "new_anom_analyses", "figures", "sst_quarterly_corrplot.png"), 
       device = "png", 
       height = 6, width = 8, units = "in")



###__Temporal Lag  ####

pull_lag_period <- function(cpr_long_df = cpr_sst, time_period = NULL) {
  
  plankton_ts <- cpr_long_df %>% 
    filter(period %in% c("Annual", "annual")) %>% 
    pivot_wider(names_from = taxa, values_from = anomaly) %>% 
    select(year, one_of(species_05))
  
  temp_ts <- cpr_sst %>% 
    distinct(year, period, .keep_all = T) %>% 
    pivot_wider(names_from = period, values_from = lag_temp) %>% 
    select(year, one_of(time_period))
  
  df_out <- inner_join(plankton_ts, temp_ts, by = "year") %>% drop_na()
  return(df_out)
  
}


#Make list of the data groups
lagged_df_list <- list(
  "Q1"  = pull_lag_period(time_period = "Q1"),
  "Q2"  = pull_lag_period(time_period = "Q2"),
  "Q3"  = pull_lag_period(time_period = "Q3"),
  "Q4"  = pull_lag_period(time_period = "Q4")
)




# Pull individual periods and prep for correlation matrix
Q1 <- lagged_df_list[["Q1"]] %>% drop_na()
Q2 <- lagged_df_list[["Q2"]] %>% drop_na()
Q3 <- lagged_df_list[["Q3"]] %>% drop_na()
Q4 <- lagged_df_list[["Q4"]] %>% drop_na()


# Pull correlations with p-values - data for corrplots
Q1_corrs <- corr_plot_setup(Q1)
Q2_corrs <- corr_plot_setup(Q2)
Q3_corrs <- corr_plot_setup(Q3)
Q4_corrs <- corr_plot_setup(Q4)


# Lagged corrplots
q1_t <- cpr_corr_plot(Q1_corrs, period = "Q1", plot_style = "wide", taxa = species_05) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
q2_t <- cpr_corr_plot(Q2_corrs, period = "Q2", plot_style = "wide", taxa = species_05) + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())
q3_t <- cpr_corr_plot(Q3_corrs, period = "Q3", plot_style = "wide", taxa = species_05) + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())
q4_t <- cpr_corr_plot(Q4_corrs, period = "Q4", plot_style = "wide", taxa = species_05) + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

#Patch them together
lagged_corrplot <- q1_t | q2_t | q3_t | q4_t
lagged_corrplot <- lagged_corrplot & theme(legend.position = "none")
lagged_corrplot <- lagged_corrplot + 
  labs(caption = "CPR correlation with SST of prior 3-month period")
lagged_corrplot


# Save Figure
ggsave(lagged_corrplot, 
       filename =  here::here("R", "new_anom_analyses", "figures", "sst_lagged_corrplot.png"), 
       device = "png", 
       height = 6, width = 8, units = "in")
