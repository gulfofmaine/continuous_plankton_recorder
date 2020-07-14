####  New Anomalies PCA w/ Quarterly SST Regressions  ####
#### Adam A. Kemberling
#### 2/11/2020

####  Packages  ####
library(tidyverse)
library(here)
library(gmRi)
library(patchwork)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))


####  NOAA + SAHFOS Anomaly Data  ####
cpr_wide <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "detrended_anomalies_noaa_sahfos.csv", sep = "/"), 
                     guess_max = 1e6, 
                     col_types = cols())


#Reference Taxa
# species_05 <- c("calanus_finmarchicus_v_vi", "centropages_typicus", "oithona_spp","para_pseudocalanus_spp", 
#                 "metridia_lucens", "calanus_i_iv", "euphausiacea_spp")
species_05 <- c("Calanus finmarchicus V-VI", "Centropages typicus", 
                "Oithona spp.","Para-Pseudocalanus spp.", 
                "Metridia lucens", "Calanus I-IV", "Euphausiacea spp.")
  

# Add some label formatting
cpr_long <- cpr_wide %>% pivot_longer(names_to = "taxa", values_to = "anomaly", cols = 5:ncol(.)) %>% 
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
  df_out <- mutate(gap_years, taxa = x)}
)

(fig1 <- cpr_long %>% 
    filter(period == "annual") %>% 
    full_join(gap_anoms) %>% 
    ggplot(aes(year, anomaly)) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.4) +
    geom_line(aes(group = taxa), color = gmri_cols("gmri blue")) + 
    facet_wrap(~taxa, ncol = 1) + 
    scale_y_continuous(breaks = c(-2, 0, 2), limits = c(-2, 2)) +
    labs(x = NULL, y = "Abundance Index"))

# Export
ggsave(plot = fig1, 
       filename = here::here("R", "new_anom_analyses", "figures", "Figure1_recreation.png"), 
       device = "png", 
       height = 8, width = 6, units = "in")


####  1. Figure 2 PCA Modes  ####
#2005 paper setup 1961-2003
cpr_2005 <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual",
         between(year, 1961, 2003)) %>% 
  pivot_wider(names_from = taxa, 
              values_from = anomaly) 

cpr_2005_vals <- cpr_2005 %>% 
  select(one_of(species_05))

#Perform PCA
pca_2005 <- prcomp(cpr_2005_vals, center = F, scale. = F)


# Get the two leading modes, and pull variance explained
#These are the weights we would use to adjust the values
leading_modes <- rownames_to_column(as.data.frame(pca_2005$rotation)) %>% dplyr::select(species = rowname, PC1, PC2)
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
    ggplot(aes(taxa, `Principal Component Weight` * -1, fill = PC)) +
    geom_col(position  = "dodge") +
    geom_vline(data = data.frame(vlines = seq(1.5, 6.5, by = 1)),
               aes(xintercept = vlines), linetype = 2, show.legend = FALSE, alpha = 0.5) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_fill_gmri(palette = "mixed") +
    labs(x = "") +
    guides(fill = guide_legend(title = NULL)) +
    theme(legend.position = c(0.825, 0.925)))

# Export
ggsave(plot = fig2a, 
       filename = here::here("R", "new_anom_analyses", "figures", "Figure2a_recreation.png"), 
       device = "png", 
       height = 6, width = 8, units = "in")


####  2. Figure 2b PCA Time-series  ####
#Fill in Year Gap
gap_years <- tibble(year = rep(c(1975, 1976), 2),
                    PC = c(rep("First Mode", 2), c(rep("Second Mode", 2)))
)


pc1 <- cpr_2005_vals %>% 
  #Add a filler column because function expects years to be column 1
  mutate(filler = NA) %>% select(filler, everything()) %>% 
      apply_pca_load(pca_load = .,
                     pca_rotations = pca_2005$rotation,
                     mode_num = 1) %>% 
      rowSums() %>% 
      as.data.frame()  %>% 
      mutate(PC = "First Mode")
colnames(pc1)[1] <- "Principal component value"
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


(fig_2b <- ggplot(pc_modes, aes(year, `Principal component value` * -1, color = PC)) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
    geom_line() +
    scale_color_gmri(palette = "mixed") +
    labs(x = NULL) + 
    theme(legend.position = c(0.85, 0.12)))

ggsave(plot = fig_2b, 
       filename = here::here("R", "new_anom_analyses", "figures", "Figure2b_recreation.png"), 
       device = "png",
       height = 6, width = 8, units = "in")



# Stacked figure
fig_2_stacked <- fig2a / fig_2b + theme(legend.position = "none")
ggsave(fig_2_stacked,
       filename = here::here("R", "new_anom_analyses", "figures", "Figure2_stacked.png"), 
       device = "png",
       height = 10, width = 8, units = "in")

####  3. Figure 3 - Temperature and PCA Modes  ####

# Bring in SST
sst_long_lagged <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "SST_with_lags.csv", sep = "/"),
                            col_types = cols())


# Plot
(fig3 <- pc_modes %>% 
   left_join(sst_long_lagged, by = "year") %>% 
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

ggsave(plot = fig3, 
       filename = here::here("R", "new_anom_analyses", "figures", "2005_PCAts_wtemps.png"), 
       device = "png",
       height = 6, width = 8, units = "in")

####__####
####  Projecting Forward to Recent Years  ####

#Prep dataset with all years
cpr_full <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual") %>% 
  pivot_wider(names_from = taxa, 
              values_from = anomaly) 

cpr_full_vals <- cpr_full %>% 
  select(one_of(species_05))


#Apply pca modes
pc1 <- cpr_full_vals %>% 
  #Add a filler column because function expects years to be column 1
  mutate(filler = NA) %>% select(filler, everything()) %>% 
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
  mutate(filler = NA) %>% select(filler, everything()) %>% 
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


####  1. Figure 4a Full PCA Time-Series  ####
(fig_4a <- ggplot(pc_modes) +
    geom_rect(xmin = 1990, xmax = 2000, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
    geom_rect(xmin = 2010, xmax = 2017, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
    geom_line(aes(year, `Principal component value` * -1, color = PC)) +
    scale_color_gmri(palette = "mixed") +
    labs(x = NULL) + 
    theme(legend.position = c(0.85, 0.12), 
          legend.box.background = element_rect(fill = "transparent"))
 )

# Export
ggsave(plot = fig_4a, 
       filename = here::here("R", "new_anom_analyses", "figures", "Figure2c_extended_timeline.png"), 
       device = "png",
       height = 6, width = 8, units = "in")

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

# Export
ggsave(plot = fig4b, 
       filename = here::here("R", "new_anom_analyses", "figures", "full_PCAts_wtemps.png"), 
       device = "png",
       height = 6, width = 8, units = "in")



