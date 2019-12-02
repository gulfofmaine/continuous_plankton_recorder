#### CPR Dataset - Principal Component Analysis
#### Adam A. Kemberling
#### 11/11/2019

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
cpr_long <- read_csv(str_c(cpr_boxpath,"data", "processed_data", "cpr_allspecies_long.csv", sep = "/"))
cpr_wide <- cpr_long %>%
  filter(is.na(anomaly) == FALSE) %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly) 
  
# ####_####
# ####  PCA Method Test  ####
# cpr.pca <- prcomp(cpr_wide[,3:12], 
#                   center = TRUE,
#                   scale. = TRUE)
# 
# 
# summary(cpr.pca)
# 
# 
# # Plots
# ggbiplot(cpr.pca)
# ggbiplot(cpr.pca, labels = cpr_wide$year)
# 
# #Grouping with ellipses
# cpr_decade   <- factor(floor_decade(cpr_wide$year))
# 
# ggbiplot(cpr.pca, ellipse=TRUE, groups = cpr_decade)

####_####
####  2005 Methodology  ####

#### Figure 1  ####
species_05 <- c("calanus", "centropages", "oithona", "para_pseudocalanus",
                "metridia", "calanus1to4", "euphausiacea")

(fig1 <- cpr_long %>% 
  filter(species %in% species_05,
         #is.na(anomaly) == FALSE,
         period == "annual",
         between(year, 1961, 2003)) %>% 
  mutate(species = factor(species, levels = c("calanus", "centropages", "oithona","para_pseudocalanus", 
                                              "metridia", "calanus1to4", "euphausiacea"))) %>% 
  ggplot(aes(year, anomaly)) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
    geom_line(aes(group = species)) + 
    facet_wrap(~species, ncol = 1) +
    labs(x = NULL, y = NULL))
ggsave(plot = fig1, filename = here::here("R", "presentations", "Figure1_recreation.png"), device = "png")


#2005 paper setup 1961-2003
cpr_2005 <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual",
         between(year, 1961, 2003)) %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly) 

cpr_2005_vals <- cpr_2005 %>% 
  select(calanus, centropages, oithona, para_pseudocalanus,
           metridia, calanus1to4, euphausiacea)

#Perform PCA
pca_2005 <- prcomp(cpr_2005_vals, center = F, scale. = F)
summary(pca_2005)

#Plot
cpr_decade_2005 <- factor(floor_decade(cpr_2005$year))
ggbiplot(pca_2005, ellipse=TRUE, groups = cpr_decade_2005, obs.scale = T, var.scale = T)




####  Figure 2 from Pershing et al. 2005  ####
#### NOTE: percents explained by PCA are hard-coded in and need to be manually changed


#Figure 2a 
#These are the weights we would use to adjust the values
leading_modes <- rownames_to_column(as.data.frame(pca_2005$rotation)) %>% dplyr::select(species = rowname, PC1, PC2)


# figure 2a
(fig2a <- rownames_to_column(as.data.frame(pca_2005$rotation)) %>% 
    dplyr::select(species = rowname, PC1, PC2)%>% 
    dplyr::rename("First Mode (65.4%)" = PC1,
                  "Second Mode (17.5%)" = PC2) %>% 
    gather(key = "PC", value =  "Principal Component Weight", 
           `First Mode (65.4%)`, `Second Mode (17.5%)`) %>% 
    mutate(species = factor(species, 
                            levels = c("calanus", "centropages", "oithona","para_pseudocalanus", 
                                       "metridia", "calanus1to4", "euphausiacea"))) %>% 
    ggplot(aes(species, `Principal Component Weight` * -1, fill = PC)) +
      geom_col(position  = "dodge") +
      scale_fill_gmri(palette = "mixed") +
      labs(x = "") +
      theme(legend.position = c(0.85, 0.2)))
ggsave(plot = fig2a, filename = here::here("R", "presentations", "Figure2a_recreation.png"), device = "png")

# Figure 2b pca time-series
pca_timeseries <- cpr_2005_vals

# Adjust each species by its weights
pca_timeseries_adjust <- function(pca_load, pca_rotations, mode_num = 1) {
  
  rotations <- as.data.frame(pca_rotations)
  principal_component_number <- mode_num
  pca_adjusted <- pca_load
  
  pca_adjusted$calanus            <- pca_load$calanus            * rotations["calanus", principal_component_number]
  pca_adjusted$centropages        <- pca_load$centropages        * rotations["centropages", principal_component_number]
  pca_adjusted$oithona            <- pca_load$oithona            * rotations["oithona", principal_component_number]
  pca_adjusted$para_pseudocalanus <- pca_load$para_pseudocalanus * rotations["para_pseudocalanus", principal_component_number]
  pca_adjusted$metridia           <- pca_load$metridia           * rotations["metridia", principal_component_number]
  pca_adjusted$calanus1to4        <- pca_load$calanus1to4        * rotations["calanus1to4", principal_component_number]
  pca_adjusted$euphausiacea       <- pca_load$euphausiacea       * rotations["euphausiacea", principal_component_number]
  return(pca_adjusted)
}

mode_1_timeseries <- pca_timeseries_adjust(pca_load = pca_timeseries,
                                           pca_rotations = pca_2005$rotation,
                                           mode_num = 1) %>% rowSums() %>% as.data.frame()
  
mode_2_timeseries <- pca_timeseries_adjust(pca_load = pca_timeseries,
                                           pca_rotations = pca_2005$rotation,
                                           mode_num = 2) %>% rowSums() %>% as.data.frame()

# Set up dataframe for plot
colnames(mode_1_timeseries) <- "Principal component value"
colnames(mode_2_timeseries) <- "Principal component value"
mode_1_timeseries$year <- cpr_2005$year
mode_2_timeseries$year <- cpr_2005$year
mode_1_timeseries$PC <- "First Mode"
mode_2_timeseries$PC <- "Second Mode"
all_years <- tibble(year = rep(c(1975, 1976),2),
                        PC = c(rep("First Mode", 2), c(rep("Second Mode", 2)))
                        )


# figure 2b
(fig2b <- bind_rows(mode_1_timeseries, mode_2_timeseries) %>% 
    full_join(all_years) %>% 
    ggplot(aes(year, `Principal component value` * -1, color = PC)) +
      geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
      geom_line() +
      scale_color_gmri(palette = "mixed") +
      #theme(legend.position = c(0.15, 0.2))) #original
      theme(legend.position = c(0.15, 0.8))) #flipped
ggsave(plot = fig2b, filename = here::here("R", "presentations", "Figure2b_recreation.png"), device = "png")




####  Extending Into New Years  ####
cpr_annual <- filter(cpr_wide, period == "annual")
pca_timeseries_full <- cpr_annual %>%  
  select(calanus, centropages, oithona, para_pseudocalanus,
         metridia, calanus1to4, euphausiacea)

mode_1_full <- pca_timeseries_adjust(pca_load = pca_timeseries_full,
                                           pca_rotations = pca_2005$rotation,
                                           mode_num = 1) %>% rowSums() %>% as.data.frame()

mode_2_full <- pca_timeseries_adjust(pca_load = pca_timeseries_full,
                                           pca_rotations = pca_2005$rotation,
                                           mode_num = 2) %>% rowSums() %>% as.data.frame()

# Set up dataframe for plot
colnames(mode_1_full) <- "Principal component value"
colnames(mode_2_full) <- "Principal component value"
mode_1_full$year <- cpr_annual$year
mode_2_full$year <- cpr_annual$year
mode_1_full$PC <- "First Mode"
mode_2_full$PC <- "Second Mode"
all_years <- tibble(year = rep(c(1975, 1976),2),
                    PC = c(rep("First Mode", 2), c(rep("Second Mode", 2)))
)


# figure 2b - extended
(fig2c <- bind_rows(mode_1_full, mode_2_full) %>% 
    full_join(all_years) %>% 
    ggplot() +
      geom_rect(xmin = 1990, xmax = 2000, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
      geom_rect(xmin = 2010, xmax = 2017, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
      geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
      geom_line(aes(year, `Principal component value` * -1, color = PC)) +
      scale_color_gmri(palette = "mixed") +
      scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
      #theme(legend.position = c(0.15, 0.2))) #original
      theme(legend.position = c(0.15, 0.8))) #flipped
ggsave(plot = fig2c, filename = here::here("R", "presentations", "Figure2b_full.png"), device = "png")





####_####
####  Bi-Monthly Periods  ####
bi_monthly_list <- cpr_wide %>% 
  split(.$period) %>%  
  map(function(x) {
    x %>% dplyr::select(year, calanus, centropages, oithona, para_pseudocalanus, 
                          metridia, calanus1to4, euphausiacea) %>% drop_na()
  })

#Map the weights of the pca from the first mode
bi_monthly_mode_1 <- bi_monthly_list %>% 
  map(function(x) 
  {x <- x %>% dplyr::select(-year) %>% 
      pca_timeseries_adjust(pca_load = .,
                            pca_rotations = pca_2005$rotation,
                            mode_num = 1) %>% 
      rowSums() %>% 
      as.data.frame()  %>% 
      mutate(PC = "First Mode")
  colnames(x)[1] <- "Principal component value"
  return(x)}
    ) %>% 
  map2(bi_monthly_list, function(x, y) {
    dplyr::select(y, year) %>% 
      bind_cols(x)  %>% 
      full_join(all_years)
  })

#Map the weights of the pca from the second mode
bi_monthly_mode_2 <- bi_monthly_list %>% 
  map(function(x) 
  {x <- x %>% dplyr::select(-year) %>% 
    pca_timeseries_adjust(pca_load = .,
                          pca_rotations = pca_2005$rotation,
                          mode_num = 2) %>% 
    rowSums() %>% 
    as.data.frame()  %>% 
    mutate(PC = "Second Mode")
  colnames(x)[1] <- "Principal component value"
  return(x)}
  ) %>% 
  map2(bi_monthly_list, function(x, y) {
    dplyr::select(y, year) %>% 
      bind_cols(x) %>% 
      full_join(all_years)
  })

bi_monthly_out <- bind_rows(bi_monthly_mode_1, bi_monthly_mode_2, .id = "period")



#Both
(bi_monthly_plots <- bi_monthly_out %>% 
  ggplot() +
    geom_rect(xmin = 1990, xmax = 2000, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
    geom_rect(xmin = 2010, xmax = 2017, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
    geom_line(aes(year, `Principal component value` * -1, color = PC)) +
    scale_color_gmri(palette = "mixed") +
    scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    #theme(legend.position = c(0.15, 0.2))) #original
    theme(legend.position = c(0.75, 0.075)) +
    facet_wrap( ~ period, ncol = 2) +
    #facet_grid(period ~ PC) +
    labs(x = NULL))
ggsave(plot = bi_monthly_plots, filename = here::here("R", "presentations", "bi_monthly_modes.png"), device = "png")


















