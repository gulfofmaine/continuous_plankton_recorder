#### CPR Dataset - Principal Component Analysis w/ SST
#### Adam A. Kemberling
#### 11/15/2019

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
cpr_long <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "cpr_allspecies_long.csv", sep = "/"),
                     col_types = cols()) %>% 
  mutate(period = case_when(
    period == "p1"     ~ "jf", 
    period == "p2"     ~ "ma",
    period == "p3"     ~ "mj",
    period == "p4"     ~ "ja",
    period == "p5"     ~ "so",
    period == "p6"     ~ "nd",
    period == "annual" ~ "annual"
  ),
  period = factor(period, levels = c("annual", "jf", "ma", "mj", "ja", "so", "nd")))


sst <- read_csv(str_c(cpr_boxpath, "data", "ENV", "GulfofMaineSSTanomalies_2019.11.14.csv", sep = "/"), skip = 1) %>% 
  rename_all(tolower)


#species from 2005 paper
species_05 <- c("calanus", "centropages", "oithona", "para_pseudocalanus",
                "metridia", "calanus1to4", "euphausiacea")



####__####
####  SST Explo  ####
sst_long <- sst %>% 
  gather(key = "period", value = "temp_anomaly", annual, jf, ma, mj, ja, so, nd) %>% 
  mutate(period = factor(period, levels = c("annual", "jf", "ma", "mj", "ja", "so", "nd"))) %>% 
  rename(year = `%year`)

ggplot(sst_long, aes(year, temp_anomaly)) +
  geom_hline(yintercept = 0, color = "darkred", alpha = 0.3, linetype = 2) +
  geom_line(group = 1) +
  geom_smooth(method = "loess") +
  facet_wrap(~period)
  

#Plotting Each species with the temperature

####  Setting up Period Lags  ####
lag_key <- sst_long %>% 
  mutate(
    lag_ref = case_when(
      period == "annual" ~ "annual",
      period == "jf"     ~ "nd",
      period == "nd"     ~ "so",
      period == "so"     ~ "ja",
      period == "ja"     ~ "mj",
      period == "mj"     ~ "ma",
      period == "ma"     ~ "jf"),
    year_ref = if_else(lag_ref == "jf", year - 1, year)) %>% 
  select(period = lag_ref, year = year_ref)

temp_lags <- lag_key %>% left_join(sst_long, by = c("period", "year")) %>% 
  rename(lag_ref = period, year_ref = year, lag_temp = temp_anomaly) %>% 
  mutate(
    period = case_when(
      lag_ref == "annual" ~ "annual",
      lag_ref == "nd"     ~ "jf",
      lag_ref == "so"     ~ "nd",
      lag_ref == "ja"     ~ "so",
      lag_ref == "mj"     ~ "ja",
      lag_ref == "ma"     ~ "mj",
      lag_ref == "jf"     ~ "ma"),
    year = if_else(lag_ref == "nd", year_ref + 1, year_ref)) 

#Add to original 
sst_long_lagged <- left_join(sst_long, temp_lags, by = c("period", "year"))

#Export to processed folder
write_csv(sst_long_lagged, str_c(cpr_boxpath, "data", "processed_data", "SST_with_lags.csv", sep = "/"), col_names = TRUE)


#Add to zooplankton
species_periods_long <- cpr_long %>% 
  filter(species %in% species_05) %>% 
  left_join(sst_long_lagged, by = c("year", "period"))

#Export to processed folder
write_csv(species_periods_long, str_c(cpr_boxpath, "data", "processed_data", "cpr_with_SSTlags.csv", sep = "/"), col_names = TRUE)


#Store combos in a list
species_periods <- cpr_long %>% 
  filter(species %in% species_05,
         between(year, 1961, 2003)) %>% 
  left_join(sst_long_lagged, by = c("year", "period"))  %>% 
  split(.$species) %>% 
  map(function(x){
    x %>% split( .$period)}) 



#Function for plotting
species_period_plot <- function(x) {
  species_name <- as.character(x$species[1])
  period_name  <- as.character(x$period[1])
  x <- filter(x, year >= 1980)
  ggplot(data = x, aes(x = year)) +
    geom_line(aes(y = anomaly, color = "Population Anomaly")) +
    geom_line(aes(y = temp_anomaly, color = "Temperature Anomaly")) +
    geom_line(aes(y = lag_temp, color = "Lagged Temperature")) +
    scale_color_gmri(name = NULL, palette = "main") +
    labs(x = NULL,
         y = "Standard Deviations from Mean",
         title = str_c(species_name, " : ", period_name)) +
    theme(legend.position = "bottom")
}

# #Plot each period for all species alone
# map(species_periods, function(x) {
#   map(x, species_period_plot)
# })

#Or specifics species-periods
species_period_plot(species_periods$calanus$jf)

####  Plotting  Calanus and Calanus 1-4  ####
species_periods_long %>% 
  filter(species %in% c("calanus", "calanus1to4"),
         year >= 1980) %>% 
  ggplot(aes(x = year)) +
  geom_rect(xmin = 1990, xmax = 2000, ymin = -3, ymax = 3, fill = "gray90", alpha = 0.05) +
  geom_hline(yintercept = 0, color = "darkblue", alpha = 0.3) +
  geom_line(aes(y = anomaly, color = "Population Anomaly")) +
  geom_line(aes(y = temp_anomaly, color = "Temperature Anomaly")) +
  geom_line(aes(y = lag_temp, color = "Lagged Temperature")) +
  scale_color_manual(name = NULL, 
                     values = c("gray30", "#EA4F12", "gray60")) +
  facet_grid(period ~ species) +
  labs(x = NULL,
       y = "Standard Deviations from Mean") +
  theme(legend.position = "bottom")

#Doing the Time Lags
ggplot(data = NULL, aes(year, anomaly)) +
  geom_line(data = species_periods$calanus$jf, aes(year, anomaly, color = "Calanus 5+"), group = 1) +
  geom_line(data = species_periods$calanus1to4$nd, aes(year - 1, anomaly, color = "Calanus 1-4"), group = 1) +
  xlim(1980,2005) + 
  labs(title = "Winter 1-4's & January 5+",
       x = NULL, y = NULL)






####__####
####  2005 PCA  ####

####  PCR Data Prep  ####
cpr_2005 <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual",
         species %in% species_05,
         between(year, 1961, 2003)) %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly)

cpr_2005_vals <- cpr_2005 %>% 
  select(one_of(species_05))


pca_2005 <- prcomp(cpr_2005_vals, center = F, scale. = F)
summary(pca_2005)


cpr_decade_2005 <- factor(floor_decade(cpr_2005$year))
ggbiplot(pca_2005, ellipse=TRUE, groups = cpr_decade_2005, obs.scale = T, var.scale = T)



####  Leading Modes  ####
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
    theme(legend.position = c(0.85, 0.9)))


##### Weight Adjusted Timeline  ####
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


#Put into an object for plotting with temperature
pca_modes_2005 <- bind_rows(mode_1_timeseries, mode_2_timeseries) %>% 
  full_join(all_years) %>% 
  left_join(sst_long_lagged, by = "year")


#Figure 2b - Timeline of leading PCA's and their summed impacts on the 6 species
(fig2b <- pca_modes_2005 %>%  #bind_rows(mode_1_timeseries, mode_2_timeseries) %>% full_join(all_years) %>% 
    ggplot(aes(year, `Principal component value` * -1, color = PC)) +
    geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
    geom_line() +
    scale_color_gmri(palette = "mixed") +
    theme(legend.position = c(0.15, 0.8))) #flipped
ggsave(plot = fig2b, filename = here::here("R", "presentations", "Figure2b_recreation.png"), device = "png")




#figure 3a, same timeline but with temperature and faceted out by PCA mode
(fig3a <- pca_modes_2005 %>% 
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
ggsave(plot = fig3a, filename = here::here("R", "presentations", "2005_ts_temps.png"), device = "png")



####__####
#### Projecting Later Years onto 2005 PCA  ####
cpr_recent <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual",
         species %in% species_05,
         between(year, 2004, 2017)) %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly)


cpr_recent_vals <- cpr_recent %>% 
  select(one_of(species_05))


#Row indices for adding grouping labels
cpr_wide <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual",
         species %in% species_05
         ) %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly) %>% 
  mutate(decade_label = floor_decade(year),
         decade_label = factor(decade_label))


#Center and Scale to original PCA if needed
#cpr_recent_vals <- scale(cpr_recent_vals, center = pca_2005$center)
cpr_recent_pred <- as.matrix(cpr_recent_vals) %*% pca_2005$rotation

#Append extra projections to original PCA
pca_2005_extended <- pca_2005
pca_2005_extended$x <- rbind(pca_2005_extended$x, cpr_recent_pred)

####  All year Bi-plot  ####
(biplot_2005_on <- ggbiplot(pca_2005_extended, 
         obs.scale = 1, 
         var.scale = 1, 
         ellipse = TRUE, 
         circle = FALSE, 
         var.axes=TRUE, 
         groups = cpr_wide$decade_label,
         ) +
  labs(subtitle = "2003:2018 Data Extra-Sample Projections") +
  guides(color = guide_legend(title = NULL, nrow = 1)) +
  theme(legend.position = "bottom"))
ggsave(biplot_2005_on, filename = here::here("R", "presentations", "2005pca_biplot.png"), device = "png")






####__####
####  Full Time-series using 2005 Modes  ####

# Figure 2c pca time-series - all years
pca_timeseries_df <- cpr_long %>%
  filter(is.na(anomaly) == FALSE,
         period == "annual",
         species %in% species_05) %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly) 

pca_timeseries <- pca_timeseries_df %>% 
  select(one_of(species_05))


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
mode_1_timeseries$year <- pca_timeseries_df$year
mode_2_timeseries$year <- pca_timeseries_df$year
mode_1_timeseries$PC <- "First Mode"
mode_2_timeseries$PC <- "Second Mode"
all_years <- tibble(year = rep(c(1975, 1976),2),
                    PC = c(rep("First Mode", 2), c(rep("Second Mode", 2)))
)



# # figure 2c. Timeline extending mode trends out to current data
# (fig2c <- bind_rows(mode_1_timeseries, mode_2_timeseries) %>%
#     full_join(all_years) %>%
#     ggplot(aes(year, `Principal component value` * -1, color = PC)) +
#       geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
#       geom_line() +
#       scale_color_gmri(palette = "mixed") +
#       scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010,2020)) +
#       #theme(legend.position = c(0.15, 0.2))) #original
#       theme(legend.position = c(0.15, 0.8))) #flipped
#
# ggsave(plot = fig2c, filename = here::here("R", "presentations", "Figure2c_extended_timeline.png"), device = "png")



#### Full Timeseries Plot  ####
pca_modes_master <- bind_rows(mode_1_timeseries, mode_2_timeseries) %>% 
  left_join(sst_long_lagged, by = "year") %>% 
  full_join(all_years)

(fig3b <- pca_modes_master %>% 
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
ggsave(plot = fig3b, filename = here::here("R", "presentations", "full_ts_temps.png"), device = "png")




####__####
####  Using SST with Population Data  ####

#this has the temperature and population anomalies together
temp_test <- species_periods_long %>% 
  filter(period == "annual") %>% 
         select(year, species, period, pop_anom = anomaly, temp_anomaly) %>% 
  #gather(key = "anomaly_type", value = "anomaly", pop_anom, temp_anomaly) %>% 
  pivot_wider(names_from = c("species"), values_from = "pop_anom") %>% 
  drop_na()
  

tt_vals <- temp_test %>% select(-one_of(c("year", "period")))

  
temp_pca <- prcomp(tt_vals, center = F, scale. = F)
#summary(temp_pca)


temp_test$decade <- factor(floor_decade(temp_test$year))
ggbiplot(temp_pca, ellipse=TRUE, groups = temp_test$decade, obs.scale = T, var.scale = T)
  
  
  

  
  





