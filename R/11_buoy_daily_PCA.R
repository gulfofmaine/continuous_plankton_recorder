####  PCA on daily buoy reads w/ application to interpolated values
####  12/16/2019


####  Packages  ####
#library(ggbiplot)
library(ggpmisc)
library(tidyverse)
library(here)
library(patchwork)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####  Load Data  ####

#Raw - source: 10_buoy_daily_interpolations
buoy_raw <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_raw.csv", sep = "/"),
                     col_types = cols(),
                     guess_max = 1e5)

buoy_pca_mat <- buoy_raw %>% 
  column_to_rownames(var = "Date") %>% 
  as.matrix()

#Interpolated NA's - source: 10_buoy_daily_interpolations
buoy_i <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_interpolated.csv", sep = "/"),
                   col_types = cols())

####__####
####  Daily Buoy PCA   ####
daily_pca <- prcomp(na.omit(buoy_pca_mat), center = FALSE, scale. = FALSE)


#Bi-plots
buoy_pca_dat        <- na.omit(buoy_raw)
buoy_pca_dat$year   <- lubridate::year(buoy_pca_dat$Date)
buoy_pca_dat$decade <- factor(floor_decade(buoy_pca_dat$year))
buoy_pca_dat$year   <- factor(buoy_pca_dat$year)
ggbiplot(na.omit(daily_pca), 
         var.axes = FALSE,
         ellipse=TRUE, 
         #labels = buoy_pca_dat$year,
         groups = buoy_pca_dat$year, 
         obs.scale = T, 
         var.scale = T)


###__ 1. Loadings on Measured Data  ####

#Pull the loadings out
pca_out <- daily_pca$x %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Date") %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, PC1, PC2, PC3) %>% 
  pivot_longer(names_to = "Principal Component", values_to = "Principal Component Loading", cols = c(PC1, PC2, PC3))

#Fill in blank days 
every_day <- data.frame(Date = seq.Date(from = min(buoy_raw$Date), to = max(buoy_raw$Date), by = 1))
every_day <- every_day %>% 
  mutate(PC1 = NA, 
         PC2 = NA,
         PC3 = NA) %>% 
  pivot_longer(names_to = "Principal Component", values_to = "Principal Component Loading", cols = c(PC1, PC2, PC3)) %>% 
  select(-`Principal Component Loading`)

#merge in
pca_out <- full_join(every_day, pca_out, by = c("Date", "Principal Component"))



#### updated plot w/ percent explained
percent_explained <- pull_deviance(daily_pca$sdev)


timeline_raw <- pca_out %>% 
  filter(`Principal Component` != "PC3" & is.na(`Principal Component`) == FALSE) %>% 
  mutate(`Principal Component` = if_else(`Principal Component` == "PC1", 
                                         as.character(percent_explained$PC1), 
                                         as.character(percent_explained$PC2)))  %>% 
  ggplot(aes(x = Date,
             y = `Principal Component Loading`, 
             color = `Principal Component`)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black", linetype = 2, alpha = 0.6) +
    ylim(-11, 11) +
    scale_color_gmri(palette = "mixed") +
    labs(x = NULL, caption = "PCA weights applied to origianl buoy measurements") +
    theme_minimal()
timeline_raw


#Export plot
ggsave(timeline_raw, 
       filename =  here::here("R", "presentations", "buoy_plots", "pca_ts_raw.png"), 
       device = "png")


####__ 2. Loadings on Imputed Data  ####

# Mapping the weights of the pca from the first mode
# Adjust sensor measurements by PC weights
apply_pca_load <- function(pca_load, pca_rotations, mode_num = 1) {
  
  #Pull PCA rotations/loadings
  rotations <- as.data.frame(pca_rotations)
  rotations_t <- t(rotations)
  
  #Principal component whose weights we want to apply
  mode_num <- as.integer(mode_num)
  
  #Copy of the initial values to apply them to
  pca_adjusted <- pca_load[,2:ncol(pca_load)]
  
  #Multiply the columns by their PCA weights
  for (i in 1:ncol(rotations_t)) {
    pca_adjusted[, i] <- pca_adjusted[, i] * rotations_t[mode_num, i]
    
  }
  
  # #Add back in the dates
  # Date <- dplyr::select(pca_load, 1)
  # pca_adjusted <- bind_cols(Date, pca_adjusted)
  return(pca_adjusted)
}

#PC1
pc1_ts_i <- apply_pca_load(pca_load = buoy_i,
                           pca_rotations = daily_pca$rotation,
                           mode_num = 1) %>% rowSums() %>% as.data.frame() 
#PC2
pc2_ts_i <- apply_pca_load(pca_load = buoy_i,
                           pca_rotations = daily_pca$rotation,
                           mode_num = 2) %>% rowSums() %>% as.data.frame()

# Set up dataframe for plot
colnames(pc1_ts_i) <- "Principal Component Loading"
colnames(pc2_ts_i) <- "Principal Component Loading"
pc1_ts_i$Date <- buoy_i$Date
pc2_ts_i$Date <- buoy_i$Date
pc1_ts_i$"Principal Component" <- "PC1"
pc2_ts_i$"Principal Component" <- "PC2"


interp_timeline <- bind_rows(pc1_ts_i, pc2_ts_i) %>% 
  mutate(`Principal Component` = if_else(`Principal Component` == "PC1", 
                                         as.character(percent_explained$PC1), 
                                         as.character(percent_explained$PC2)))

#Plot on interpolated timeline
(timeline_interp <- interp_timeline %>% 
  ggplot(aes(Date, `Principal Component Loading`, color = `Principal Component`)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "black", linetype = 2, alpha = 0.6) +
  scale_color_gmri(palette = "mixed") +
  ylim(-11, 11) +
  labs(x = NULL, caption = "PCA weights applied to interpolated buoy measurements") +
  theme_minimal())

#Export plot
ggsave(timeline_interp, 
       filename =  here::here("R", "presentations", "buoy_plots", "pca_ts_interp.png"), 
       device = "png")


#Side-by-side
stacked_plot <- timeline_raw / timeline_interp & theme(legend.position = "none")
ggsave(plot = stacked_plot, filename = here::here("R", "presentations", "buoy_plots","buoy_pca_timelines_stacked.png"), device = "png")

#Loadings and percent explained
daily_pca_loadings <- t(daily_pca$rotation[,1:5]) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Principal Component") %>% 
  pivot_longer(names_to = "sensor", values_to = "loading", 2:ncol(.))

pull_deviance <- function(pca_sdev) {
  
  eigs <- pca_sdev ^ 2
  
  deviance_df <- rbind(
    SD = sqrt(eigs),
    Proportion = eigs/sum(eigs),
    Cumulative = cumsum(eigs)/sum(eigs))
  
  pca_dev_out <- data.frame(
    "PC1" = str_c(as.character(round(deviance_df[2,1] * 100, 2)), "% of Variance"),
    "PC2" = str_c(as.character(round(deviance_df[2,2] * 100, 2)), "% of Variance"),
    "PC3" = str_c(as.character(round(deviance_df[2,3] * 100, 2)), "% of Variance"),
    "PC4" = str_c(as.character(round(deviance_df[2,4] * 100, 2)), "% of Variance"),
    "PC5" = str_c(as.character(round(deviance_df[2,5] * 100, 2)), "% of Variance"))
  
  return(pca_dev_out)
  
}

pull_deviance(daily_pca$sdev)






####________________________________####
####  Applying weights to Quarterly CPR Data  ####

# Full Buoy Timeseries A. w/o  interpolated values
actual_means <- buoy_pca_dat %>% 
  drop_na() %>% 
  mutate(
    Year = lubridate::year(Date),
    julian = lubridate::yday(Date), 
    period = case_when(
      julian <= 91                       ~ "Q1",
      between(julian, left = 92, 182)    ~ "Q2",
      between(julian, left = 183, 273)   ~ "Q3",
      julian > 273                       ~ "Q4"
    )) %>% 
  group_by(Year, period) %>% 
  summarise_if(is.double, mean) %>% 
  select(-julian, -Date)

# Full Buoy Timeseries B. w/  interpolated values
interpolated_means <- buoy_i %>% 
  mutate(
    Year = lubridate::year(Date),
    julian = lubridate::yday(Date), 
    period = case_when(
      julian <= 91                       ~ "Q1",
      between(julian, left = 92, 182)    ~ "Q2",
      between(julian, left = 183, 273)   ~ "Q3",
      julian > 273                       ~ "Q4"
    ))%>% 
  group_by(Year, period) %>% 
  summarise_if(is.double, mean) %>% 
  select(-julian, -Date)

# Quarterly Timeseries
cpr_quarters <- read_csv(str_c(cpr_boxpath,"data", "processed_data", "cpr_allspecies_long_quarters.csv", sep = "/"),
                         col_types = cols()) %>% mutate(
                           period = case_when(
                             period == "annual" ~"Annual",
                             period == "q1" ~"Q1",
                             period == "q2" ~"Q2",
                             period == "q3" ~"Q3",
                             period == "q4" ~"Q4",
                             TRUE ~ "Missed One")) %>% 
  filter(period != "Annual") %>% 
  pivot_wider(names_from = species, values_from = anomaly) %>% 
  rename(Year = year)

# Basically we want to apply the PCA weights to the buoy values at their quarterly means
# This gies us the loadings that correspond with the quarterly CPR concentrations

# PCA object

#Loadings for each sensor for 1 & 2
loadings <- as.data.frame(daily_pca$rotation[, 1:2])
loadings <- rownames_to_column(loadings, var = "sensor")


####__ Apply PC's to non-interpolated data  ####
actual_means$PC1 <- c(as.matrix(actual_means[, c(3:48)]) %*% loadings[,"PC1"]) #pc1
actual_means$PC2 <- c(as.matrix(actual_means[, c(3:48)]) %*% loadings[,"PC2"]) #pc1


ggplot(actual_means, aes(x = Year)) +
  geom_line(aes(y = PC1, color = "PC1")) +
  geom_line(aes(y = PC2, color = "PC2")) +
  facet_wrap(~period) +
  labs(y = "Principal Component Loading")



#####__ Apply PC's to interpolated data  ####
interpolated_means$PC1 <- c(as.matrix(interpolated_means[, c(3:48)]) %*% loadings[,"PC1"]) #pc1
interpolated_means$PC2 <- c(as.matrix(interpolated_means[, c(3:48)]) %*% loadings[,"PC2"]) #pc1


ggplot(interpolated_means, aes(x = Year)) +
  geom_line(aes(y = PC1, color = "PC1")) +
  geom_line(aes(y = PC2, color = "PC2")) +
  facet_wrap(~period) +
  labs(y = "Principal Component Loading")



###__ Combine with CPR Data  ####
interpolated_pc <- interpolated_means %>% select(Year, period, PC1_interpolated = PC1, PC2_interpolated = PC2)
gappy_pc <- select(actual_means, Year, period, PC1_actual = PC1, PC2_actual = PC2)
buoy_pca_quarters <- full_join(interpolated_pc, gappy_pc, by = c("Year", "period"))
buoy_w_cpr <- left_join(buoy_pca_quarters, cpr_quarters)

####__ Plot Quarters Within Years   ####
buoy_w_cpr <- buoy_w_cpr %>% 
  mutate(
    jday = case_when(
      period == "Q1" ~ 45,
      period == "Q2" ~ 137,
      period == "Q3" ~ 228,
      period == "Q4" ~ 320
    ),
    Q_Date = as.Date(paste(Year, jday, sep = "-"),"%Y-%j") 
  ) %>% 
  select(Year, period, jday, Q_Date, everything())

# Interpolated Timeline - Ordered by date of occurrence
ggplot(buoy_w_cpr, aes(x = Q_Date)) +
  geom_line(aes(y = PC1_interpolated, color = "Buoy PC1 Interpolated")) +
  geom_line(aes(y = PC2_interpolated, color = "Buoy PC2 Interpolated")) +
  labs(x = "Date", y = "Principal Component Loading")

ggplot(buoy_w_cpr, aes(x = Q_Date)) +
  geom_line(aes(y = PC1_actual, color = "Buoy PC1 Measured")) +
  geom_line(aes(y = PC2_actual, color = "Buoy PC2 Measured")) +
  labs(x = "Date", y = "Principal Component Loading")


####________________________________####
####  Correlations between buoy PCA loadings and Quarterly Concentrations  ####

# Data going in: Quarterly Averaged PCA loadings - Quarterly Averaged CPR Data
buoy_w_cpr

# prep for corrplot functions
buoy_cpr_mat <- buoy_w_cpr %>% select(-jday, -Q_Date) %>% rename(year = Year)
actual_prepped <- buoy_cpr_mat %>% select(-PC1_interpolated, -PC2_interpolated) %>% drop_na()
interp_prepped <- buoy_cpr_mat %>% select(-PC1_actual, -PC2_actual) %>% drop_na()

# correlations and significance
observed_val_corrs <- actual_prepped %>% 
  split(.$period) %>% 
  map(function(x){ungroup(x) %>% select(-period) %>% corr_plot_setup()})
interp_val_corrs <- interp_prepped %>% 
  split(.$period) %>% 
  map(function(x){ungroup(x) %>% select(-period) %>% corr_plot_setup()})


# Actual data first
obs_plots <- observed_val_corrs %>% imap(function(x, y) {
  if(y == "Q1") {
    cpr_corr_plot(x, period = y, plot_style = "wide")
  } else {
    cpr_corr_plot(x, period = y, plot_style = "wide") + theme(axis.text.y = element_blank())
  }
})

# And again for the interpolated data
interp_plots <- interp_val_corrs %>% imap(function(x, y) {
  if(y == "Q1") {
    cpr_corr_plot(x, period = y, plot_style = "wide")
  } else {cpr_corr_plot(x, period = y, plot_style = "wide") + theme(axis.text.y = element_blank())}
})


#Patch them together
obs_quarterly_corrplot <- obs_plots[[1]] | obs_plots[[2]] | obs_plots[[3]] | obs_plots[[4]]
obs_quarterly_corrplot <- obs_quarterly_corrplot & theme(legend.position = "none")
obs_quarterly_corrplot <- obs_quarterly_corrplot + labs(caption = "PCA Loadings Applied to all Non-NA records")
obs_quarterly_corrplot
ggsave(plot = obs_quarterly_corrplot, filename = here::here("R", "presentations", "buoy_plots", "quarterly_buoy_pca_correlations_actual.png"), device = "png")

#Patch them together
interp_quarterly_corrplot <- interp_plots[[1]] | interp_plots[[2]] | interp_plots[[3]] | interp_plots[[4]]
interp_quarterly_corrplot <- interp_quarterly_corrplot & theme(legend.position = "none")
interp_quarterly_corrplot <-interp_quarterly_corrplot + labs(caption = "PCA Loadings Applied to all Imputed Measurements")
interp_quarterly_corrplot
ggsave(plot = interp_quarterly_corrplot, filename = here::here("R", "presentations", "buoy_plots","quarterly_buoy_pca_correlations_interpolated.png"), device = "png")



