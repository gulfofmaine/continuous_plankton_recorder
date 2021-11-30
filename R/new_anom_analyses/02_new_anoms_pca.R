####  New Anomalies PCA w/ Quarterly SST Regressions  ####
#### Adam A. Kemberling
#### 2/11/2020

####  Packages  ####
library(tidyverse)
library(here)
library(gmRi)
library(patchwork)
library(magrittr)
library(targets)

#ggplot theme
theme_set(theme_minimal())

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))


####  NOAA + SAHFOS Anomaly Data  ####

# Using targets
tar_load(gom_seasonal_avgs)

# reshape to wide format using standardized anomalies
var_col <- sym("anom_z") #standardized anomalies
cpr_wide <- gom_seasonal_avgs %>% 
  select(taxa, year, period, datebounds, anom_z) %>% 
  pivot_wider(names_from = taxa, values_from = {{var_col}}) %>% 
  janitor::clean_names()

# # Load data from original workflow
# cpr_wide <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "detrended_anomalies_noaa_sahfos.csv", sep = "/"),
#                      guess_max = 1e6,
#                      col_types = cols())




####  Pull the taxa from 2005 paper

# #Reference Taxa
# species_05 <- c("Calanus finmarchicus V-VI", "Centropages typicus",
#                 "Oithona spp.","Para-Pseudocalanus spp.",
#                 "Metridia lucens", "Calanus I-IV", "Euphausiacea spp.")
# species_05 <- factor(species_05, levels = species_05)

#New levels for taxa plots, ordered by size with calanus together
species_05 <- c("Calanus I-IV", "Calanus finmarchicus V-VI", "Centropages typicus",
                "Oithona spp.","Para-Pseudocalanus spp.",
                "Metridia lucens",  "Euphausiacea spp.")
species_05 <- factor(species_05, levels = species_05)
  

# Add some label formatting
cpr_long <- cpr_wide %>% 
  pivot_longer(names_to = "taxa", values_to = "anomaly", cols = 5:ncol(.)) %>% 
  mutate(taxa = stringr::str_to_sentence(taxa),
         taxa = str_replace_all(taxa, "Para_pseu", "Para-Pseu"),
         taxa = str_replace_all(taxa, "i_iv", "I-IV"),
         taxa = str_replace_all(taxa, "v_vi", "V-VI"),
         taxa = str_replace_all(taxa, "_", " "),
         taxa = str_replace_all(taxa, "spp", "spp."),
         taxa = factor(taxa, levels = species_05)) %>% 
  filter(taxa %in% species_05)



#### Figure 1 from Pershing et al. 2005

# add gap years for plot of annual anomalies
gap_years <- data.frame(year = c(1975, 1976),
                        anomaly = c(NA, NA))

gap_anoms <- map_dfr(species_05, function(x){
  df_out <- mutate(gap_years, taxa = x)})

# Plot Anomalies for 
(fig1 <- cpr_long %>% 
    filter(period == "annual") %>% 
    full_join(gap_anoms) %>% 
    ggplot(aes(year, anomaly)) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.4) +
    geom_line(aes(group = taxa), color = gmri_cols("gmri blue")) + 
    facet_wrap(~taxa, ncol = 2) + 
    labs(x = NULL, y = "Abundance Index"))

# # Export
# ggsave(plot = fig1,
#        filename = here::here("R", "new_anom_analyses", "figures", "Figure1_recreation.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")


####  1. Figure 2 PCA Modes  ####


#2005 paper setup 1961-2003
# annual anomalies only
# 1961-2003
cpr_2005 <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual",
         between(year, 1961, 2003)) %>% 
  pivot_wider(names_from = taxa, 
              values_from = anomaly) 

# Pull just the anomaly values
cpr_2005_vals <- cpr_2005 %>% 
  select(one_of(species_05))


#Perform PCA
pca_2005 <- prcomp(cpr_2005_vals, center = F, scale. = F)




# Get the two leading modes, and pull variance explained
#These are the weights we would use to adjust the values
leading_modes <- rownames_to_column(as.data.frame(pca_2005$rotation), var = "species") %>% 
  dplyr::select(species, PC1, PC2)

percent_explained <- pull_deviance(pca_2005$sdev)


# finally the plot
(fig2a <- rownames_to_column(as.data.frame(pca_2005$rotation)) %>% 
    dplyr::select(taxa = rowname, PC1, PC2)%>% 
    gather(key = "PC", value =  "Principal Component Weight", PC1, PC2) %>%
    mutate(species = factor(taxa, levels = species_05),
           PC = if_else(PC == "PC1", 
                        as.character(percent_explained$PC1),
                        as.character(percent_explained$PC2)),
           PC = fct_rev(PC)) %>% 
    ggplot(aes(species, `Principal Component Weight` * -1, fill = PC)) +
    geom_col(position  = "dodge") +
    geom_vline(data = data.frame(vlines = seq(1.5, 6.5, by = 1)),
               aes(xintercept = vlines), linetype = 2, show.legend = FALSE, alpha = 0.5) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_fill_gmri(palette = "mixed") +
    labs(x = "", y = "Principal Component Weight") +
    guides(fill = guide_legend(title = NULL)) +
    theme(legend.position = c(0.825, 0.095),
          legend.box.background = element_rect(fill = "white")))

# # Export
# ggsave(plot = fig2a,
#        filename = here::here("R", "new_anom_analyses", "figures", "Figure2a_recreation.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")


####  2. Figure 2b PCA Time-series  ####
#Fill in Year Gap
gap_years <- tibble(year = rep(c(1975, 1976), 2),
                    PC = c(rep("First Mode", 2), c(rep("Second Mode", 2)))
)

  
pc1 <- cpr_2005_vals %>% 
  #Add a filler column because function expects years to be column 1
  mutate(filler = NA) %>% 
  select(filler, everything()) %>% 
  apply_pca_load(pca_load = .,
                 pca_rotations = pca_2005$rotation,
                 mode_num = 1) %>% 
      rowSums() %>% 
      as.data.frame()  %>% 
      mutate(PC = "First Mode",
             `Principal component value` = `.` * -1) %>% select(2,3)

#colnames(pc1)[1] <- "Principal component value"
pc1 <- bind_cols(year = cpr_2005$year, pc1)
pc1 <- full_join(gap_years, pc1) %>% arrange(year)


pc2 <- cpr_2005_vals %>% 
  #Add a filler column because function expects years to be column 1
  mutate(filler = NA) %>% select(filler, everything()) %>% 
  apply_pca_load(pca_load = .,
                 pca_rotations = pca_2005$rotation,
                 mode_num = 2) %>% 
  rowSums() %>% 
  as.data.frame()  %>% 
  mutate(PC = "Second Mode")

colnames(pc2)[1] <- "Principal component value"
pc2 <- bind_cols(year = cpr_2005$year, pc2)
pc2 <- full_join(gap_years, pc2) %>% arrange(year)

#bind pca modes
pc_modes <- bind_rows(pc1, pc2)


(fig_2b <- ggplot(pc_modes, aes(year, `Principal component value`, color = PC)) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
    geom_line() +
    scale_color_gmri(palette = "mixed") +
    labs(x = NULL, y = "Principal Component Loading") + 
    theme(legend.position = c(0.85, 0.12),
          legend.box.background = element_rect(fill = "white")))

# ggsave(plot = fig_2b,
#        filename = here::here("R", "new_anom_analyses", "figures", "Figure2b_recreation.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")



####__ 2b. Stacked figure  ####
fig_2_stacked <- fig2a / fig_2b + theme(legend.position = "none")
fig_2_stacked
# ggsave(fig_2_stacked,
#        filename = here::here("R", "new_anom_analyses", "figures", "Figure2_stacked.png"),
#        device = "png",
#        height = 10, width = 8, units = "in")

####  3. Figure 3 - Temperature and PCA Modes  ####

# Bring in SST
sst_long_lagged <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "SST_with_lags.csv", sep = "/"),
                            col_types = cols())
# matched them up
cpr_pca_sst <- pc_modes %>% 
  left_join(sst_long_lagged, by = "year") %>% 
  filter(period == "annual")

# Pearson correlation with temp anoms
cpr_pca_sst %>% filter(PC == "First Mode") %>% drop_na(`Principal component value`, temp_anomaly) %$% cor(`Principal component value`, temp_anomaly, method = "pearson")
cpr_pca_sst %>% filter(PC == "Second Mode") %>% drop_na(`Principal component value`, temp_anomaly) %$% cor(`Principal component value`, temp_anomaly, method = "pearson")


# Plot
(fig3 <- cpr_pca_sst %>% 
   ggplot(aes(x = year, y = NULL)) +
   geom_hline(yintercept = 0, color = "darkred", alpha = 0.3, linetype = 2) +
   geom_line(aes(year, `Principal component value` * -1, color = PC)) +
   geom_line(aes(year, temp_anomaly, color = "Temperature Anomaly")) +
   scale_color_manual(values = c(as.character(gmri_cols("orange")), as.character(gmri_cols("teal")), "gray")) +
   guides(color = guide_legend(title = NULL)) +
   theme(legend.position = "bottom") +
   facet_wrap(~PC, nrow = 2) +
   labs(y = "Magnitude", x = NULL))

# ggsave(plot = fig3, 
#        filename = here::here("R", "new_anom_analyses", "figures", "2005_PCAts_wtemps.png"), 
#        device = "png",
#        height = 6, width = 8, units = "in")




####______________________________________####
####  Projecting Forward to Recent Years  ####

#Prep dataset with all years
cpr_full <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual") %>% 
  pivot_wider(names_from = taxa, 
              values_from = anomaly) 

# Pull the focal taxa
cpr_full_vals <- cpr_full %>% 
  select(one_of(species_05))


#Apply pca modes
pc1 <- cpr_full_vals %>% 
  #Add a filler column because function expects years to be column 1
  mutate(filler = NA) %>% 
  select(filler, everything()) %>% 
  apply_pca_load(pca_load = .,
                 pca_rotations = pca_2005$rotation,
                 mode_num = 1) %>% 
  rowSums() %>% 
  as.data.frame()  %>% 
  mutate(PC = "First Mode")
colnames(pc1)[1] <- "Principal component value"
pc1 <- bind_cols(year = cpr_full$year, pc1)
pc1 <- full_join(gap_years, pc1) %>% arrange(year)


pc2 <- cpr_full_vals %>% 
  #Add a filler column because function expects years to be column 1
  mutate(filler = NA) %>% 
  select(filler, everything()) %>% 
  apply_pca_load(pca_load = .,
                 pca_rotations = pca_2005$rotation,
                 mode_num = 2) %>% 
  rowSums() %>% 
  as.data.frame()  %>% 
  mutate(PC = "Second Mode")
colnames(pc2)[1] <- "Principal component value"
pc2 <- bind_cols(year = cpr_full$year, pc2)
pc2 <- full_join(gap_years, pc2) %>% arrange(year)

#bind pca modes
pc_modes <- bind_rows(pc1, pc2)

# Flip the second mode for consistency with the other period
pc_modes <- pc_modes %>% 
  mutate(`Principal component value` = ifelse(
    PC == "Second Mode",
    `Principal component value` * -1,
    `Principal component value`))


####  1. Figure 4a Full PCA Time-Series  ####
(fig_4a <- ggplot(pc_modes) +
    geom_rect(xmin = 1998, xmax = 2003, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
    geom_rect(xmin = 2009, xmax = 2017, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
    geom_line(aes(year, `Principal component value` * -1, color = PC)) +
    scale_color_gmri(palette = "mixed") +
    labs(x = NULL, y = "Principal Component Loading") + 
    theme(legend.position = c(0.85, 0.12), 
          legend.box.background = element_rect(fill = "white"))
 )

# # Export
# ggsave(plot = fig_4a,
#        filename = here::here("R", "new_anom_analyses", "figures", "Figure2c_extended_timeline.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")




####__ b. Stacked figure  ####
# Stack full timeseries with original loadings
full_stack <- fig2a / fig_4a + theme(legend.position = "none")
full_stack
# ggsave(plot = full_stack,
#        filename = here::here("R", "new_anom_analyses", "figures", "original_cpr_modes_fullts.png"),
#        device = "png",
#        height = 10, width = 8, units = "in")


####__ c. Stacked with Temperature  ####

temp_tl <- sst_long_lagged %>% 
  filter(period == "annual") %>% 
  ggplot(aes(year, temp_anomaly)) +
  geom_rect(xmin = 1998, xmax = 2003, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
  geom_rect(xmin = 2009, xmax = 2017, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
  geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
  geom_line() +
  scale_x_continuous(limits = c(1961, 2017)) +
  #scale_y_continuous(limits = c(-2.1, 2.1)) +
  labs(x = "", 
       y = expression(paste("Sea Surface Temperature Anomaly ", degree, "C")))
temp_tl


trip_stack <- (fig2a + theme(legend.position = c(0.5, 0.15))) / (fig_4a + theme(legend.position = "none")) / temp_tl
trip_stack

# ggsave(plot = trip_stack,
#        filename = here::here("R", "new_anom_analyses", "figures", "original_cpr_modes_fullts_temp.png"),
#        device = "png",
#        height = 11, width = 8, units = "in")




####  2. Figure 4b Full TS w/ SST  ####
pca_modes_master <- pc_modes %>% 
  left_join(sst_long_lagged, by = "year") 


(fig4b <- pca_modes_master %>% 
    filter(period == "annual") %>% 
    ggplot(aes(x = year, y = NULL)) +
    geom_hline(yintercept = 0, color = "darkred", alpha = 0.3, linetype = 2) +
    geom_line(aes(year, `Principal component value` * -1, color = PC)) +
    geom_line(aes(year, temp_anomaly, color = "Temperature Anomaly")) +
    scale_color_manual(name = NULL, 
                       values = c(as.character(gmri_cols("orange")),
                                  as.character(gmri_cols("teal")),
                                  "gray")) +
    guides(color = guide_legend(title = NULL)) +
    theme(legend.position = "bottom") +
    facet_wrap(~PC, nrow = 2) +
    labs(y = "Magnitude", x = NULL))

# # Export
# ggsave(plot = fig4b, 
#        filename = here::here("R", "new_anom_analyses", "figures", "full_PCAts_wtemps.png"), 
#        device = "png",
#        height = 6, width = 8, units = "in")







####_________________________####
####_________________________####
####  All Years, Focal Species Only  ####
# NOT used in paper
# Focal Species, all years PCA
# Not for paper

# Start fresh from target
tar_load(gom_seasonal_avgs)

# import pipeline functions
source(here("R/support/gom_cpr_pipeline_support.R"))

# Prep for PCA
full_period_prepped <- prep_PCA_periods(cpr_anomalies_long = gom_seasonal_avgs, 
                                        matrix_var = "standardized anomalies", 
                                        use_focal_species = TRUE, 
                                        year_subsets = list("1961-2017" = c(1961, 2017)))



# Pull Time Period & periodicity
full_period_pca <- prep_PCA_matrices(period_list = full_period_prepped, periodicity = "annual")
meta <- full_period_pca$`1961-2017`$metadata
pca_mat <- full_period_pca$`1961-2017`$pca_matrix 


# 1. Perform PCA using function
full_pca <- perform_PCA(pca_matrix = pca_mat,
                        pca_meta = meta)




#####  PCA Timeseries Plot  ####
ggplot(full_pca$PC_timeseries, aes(year, `Principal component value`, color = PC)) +
  geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
  geom_line() +
  scale_color_gmri(palette = "mixed") +
  labs(x = NULL, y = "Principal Component Loading", subtitle = "PCA Time Period: 1961-2017") + 
  theme(legend.position = c(0.85, 0.12),
        legend.box.background = element_rect(fill = "white"))


#####  PCA Taxa Weight Plot  ####
full_pca$PC_weights %>% 
  gather(key = "PC", value =  "Principal Component Weight", PC1, PC2) %>%
  ggplot(aes(taxa, `Principal Component Weight`, fill = PC)) +
  geom_col(position  = "dodge") +
  geom_vline(data = data.frame(vlines = seq(1.5, 6.5, by = 1)),
             aes(xintercept = vlines), linetype = 2, show.legend = FALSE, alpha = 0.5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_gmri(palette = "mixed") +
  labs(x = "", y = "Principal Component Weight") +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = c(0.825, 0.825),
        legend.box.background = element_rect(fill = "white"))


# #### Export: CPR PCA Timeseries  ####
# full_pca$PC_timeseries %>%
#   pivot_wider(names_from = PC, values_from = `Principal component value`) %>%
#   mutate(taxa_used = "seven focal taxa", pca_period = "1961-2017") %>%
#   write_csv(here("results_data/cpr_focal_pca_timeseries_period_1961-20017.csv"))
