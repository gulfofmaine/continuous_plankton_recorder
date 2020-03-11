####  New Anomalies PCA w/ Quarterly SST Regressions  ####
#### Adam A. Kemberling
#### 2/11/2019

####  Packages  ####
library(tidyverse)
library(here)
library(gmRi)
library(patchwork)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))
ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)


####  NOAA + SAHFOS Anomaly Data  ####
cpr_wide <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "detrended_anomalies_noaa_sahfos.csv", sep = "/"), 
                     guess_max = 1e6, 
                     col_types = cols())

cpr_long <- cpr_wide %>% pivot_longer(names_to = "taxa", values_to = "anomaly", cols = 5:ncol(.))

#Reference Taxa
species_05 <- c("calanus_finmarchicus_v_vi", "centropages_typicus", "oithona_spp","para_pseudocalanus_spp", 
                "metridia_lucens", "calanus_i_iv", "euphausiacea_spp")
  


#### Figure 1 from Pershing et al. 2005
(fig1 <- cpr_long %>% 
    filter(taxa %in% species_05,
           #is.na(anomaly) == FALSE,
           period == "annual",
           between(year, 1961, 2003)) %>% 
    mutate(taxa = factor(taxa, levels = species_05)) %>% 
    ggplot(aes(year, anomaly)) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
    geom_line(aes(group = taxa)) + 
    facet_wrap(~taxa, ncol = 1) +
    labs(x = NULL, y = NULL))
ggsave(plot = fig1, filename = here::here("R", "new_anom_analyses", "figures", "Figure1_recreation.png"), device = "png")


####  Figure 2 PCA Modes  ####
#2005 paper setup 1961-2003
cpr_2005 <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual",
         between(year, 1961, 2003)) %>% 
  pivot_wider(names_from = taxa, 
              values_from = anomaly) 

cpr_2005_vals <- cpr_2005 %>% 
  select(calanus_finmarchicus_v_vi, centropages_typicus, oithona_spp, para_pseudocalanus_spp,
         metridia_lucens, calanus_i_iv, euphausiacea_spp)

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
                        as.character(percent_explained$PC2))
           ) %>% 
    ggplot(aes(taxa, `Principal Component Weight` * -1, fill = PC)) +
    geom_col(position  = "dodge") +
    scale_fill_gmri(palette = "mixed") +
    labs(x = "") +
    guides(fill = guide_legend(title = NULL)) +
    theme(legend.position = c(0.825, 0.925)))
ggsave(plot = fig2a, filename = here::here("R", "new_anom_analyses", "figures", "Figure2a_recreation.png"), device = "png")


####  Figure 2b PCA Time-series  ####
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
  labs(x = NULL))
ggsave(plot = fig_2b, filename = here::here("R", "new_anom_analyses", "figures", "Figure2b_recreation.png"), device = "png")
