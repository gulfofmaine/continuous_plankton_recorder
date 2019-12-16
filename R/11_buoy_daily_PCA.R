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
####  PCA on physical measurements  ####
daily_pca <- prcomp(na.omit(buoy_pca_mat), center = FALSE, scale. = FALSE)
summary(daily_pca)


#Bi-plots



####__####
####  Loadings on Real Data  ####

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

#Plot first two modes - display gaps from NA values
(timeline_raw <- ggplot(filter(pca_out, `Principal Component` != "PC3" & is.na(`Principal Component`) == FALSE), 
       aes(x = Date,
           y = `Principal Component Loading`, 
           color = `Principal Component`)) +
  geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
  geom_line() +
  scale_color_gmri(palette = "mixed") +
  labs(x = NULL, caption = "PCA weights applied to origianl buoy measurements") +
  theme_minimal())





####__####
####  Loadings on Interpolated Data  ####

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


#Plot on interpolated timeline
(timeline_interp <- bind_rows(pc1_ts_i, pc2_ts_i) %>% 
  ggplot(aes(Date, `Principal Component Loading`, color = `Principal Component`)) +
  geom_hline(yintercept = 0, color = "royalblue", linetype = 2, alpha = 0.2) +
  geom_line() +
  scale_color_gmri(palette = "mixed") +
  labs(x = NULL, caption = "PCA weights applied to interpolated buoy measurements") +
  theme_minimal())


#Side-by-side
stacked_plot <- timeline_raw / timeline_interp
ggsave(plot = stacked_plot, filename = here::here("R", "presentations", "buoy_pca_timelines_stacked.png"), device = "png")

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
