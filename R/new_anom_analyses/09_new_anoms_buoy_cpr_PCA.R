####  PCA on daily buoy reads w/ application to interpolated values
####  PCA loads then compared to quarterly zooplankton concentrations
####  New anom analysis version of `11_buoy_daily_pca.R`
####  12/16/2019

####  Packages  ####
#library(ggbiplot)
library(ggpmisc)
library(tidyverse)
library(here)
library(patchwork)
library(gmRi)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####  Load Data  ####

# CPR Dataset with quarterly anomalies and SST with a one-period lag
# souce: 03_new_anoms_quarterly_sst.R
cpr_sst <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "anomalies_w_quarterlysst.csv", sep = "/"),
                    col_types = cols(),
                    guess_max = 1e5)


# #Reference Taxa
# species_05 <- c("calanus_finmarchicus_v_vi", "centropages_typicus", "oithona_spp","para_pseudocalanus_spp", 
#                 "metridia_lucens", "calanus_i_iv", "euphausiacea_spp")
# 
# cpr_sst <- cpr_sst %>% filter(taxa %in% species_05)

#New levels for taxa plots, ordered by size with calanus together
species_05 <- c("Calanus I-IV", "Calanus finmarchicus V-VI", "Centropages typicus",
                "Oithona spp.","Para-Pseudocalanus spp.",
                "Metridia lucens",  "Euphausiacea spp.")
species_05 <- factor(species_05, levels = species_05)


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




# Gappy Buoy data - source: 10_buoy_daily_interpolations
buoy_raw <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_raw.csv", sep = "/"),
                     col_types = cols(),
                     guess_max = 1e5)

# number of days with complete records
buoy_raw %>% column_to_rownames(var = "Date") %>% drop_na() %>% nrow()

# Interputed NA Buoy data - source: 10_buoy_daily_interpolations.R
buoy_i <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_interpolated.csv", sep = "/"),
                   col_types = cols())

# Matrix used for daily PCA
buoy_pca_mat <- buoy_raw %>% 
  column_to_rownames(var = "Date") %>% 
  as.matrix()

####__####
####  Daily Buoy PCA   ####
daily_pca <- prcomp(na.omit(buoy_pca_mat), center = FALSE, scale. = FALSE)




####__ 1. Loadings by Sensor  ####
#PCA Leading Modes
leading_modes <- rownames_to_column(as.data.frame(daily_pca$rotation)) %>%
  dplyr::select(sensor_id = rowname, PC1, PC2)

#Percent Deviance Explained (To slide into plots)
deviance_explained <- pull_deviance(daily_pca$sdev)

#Plotting Buoy Weights
buoy_weights <- leading_modes %>%
  gather(key = "PC", value =  "Principal Component Weight", PC1, PC2) %>%
  mutate(PC = if_else(PC == "PC1", 
                      as.character(deviance_explained$PC1), 
                      as.character(deviance_explained$PC2)),
         buoy_id = str_sub(sensor_id, -1, -1),
         buoy_type = if_else(buoy_id %in% c("M", "N"), "Offshore", "Nearshore"),
         reading_depth = str_sub(sensor_id, -6, -4),
         reading_depth = str_c(reading_depth, " m"),
         reading_depth = str_replace_all(reading_depth, "001", "1"),
         reading_depth = str_replace_all(reading_depth, "010", "10"),
         reading_depth = str_replace_all(reading_depth, "020", "20"),
         reading_depth = str_replace_all(reading_depth, "050", "50"),
         reading_depth = if_else(buoy_id == "M" & reading_depth == "150 m", 
                                 "180 m", reading_depth),
         reading_depth = factor(reading_depth, 
                                levels = c("1 m", "10 m", "20 m", 
                                           "50 m", "100 m", "150 m", "180 m")),
         var_id = str_sub(sensor_id, 1, 1),
         var_id = if_else(var_id == "s", "Salinity", "Temperature")
  )




# Plot the buoy sensor loadings
sensor_p <- buoy_weights %>% 
  mutate(PC = fct_rev(PC)) %>% 
  ggplot(aes(reading_depth, `Principal Component Weight`, fill = PC)) +
  geom_hline(yintercept = 0, color = "black", linetype = 2, alpha = 0.6) +
  geom_col(position  = "dodge") +
  scale_fill_gmri(palette = "mixed") +
  labs(x = "") +
  facet_grid(var_id ~ buoy_id) +
  #facet_grid(buoy_id ~ var_id) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_text(angle = 0))

# plot it
sensor_p <- sensor_p + guides("fill" = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme(legend.position = "bottom")
sensor_p


#Export plot
# ggsave(sensor_p,
#        filename =  here::here("R", "new_anom_analyses", "figures", "buoy_pca_weights.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")





####__ 2.  Loadings on Actual Data  ####
#### updated plot w/ percent explained


####
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
####


# Pull the percent explaned
percent_explained <- pull_deviance(daily_pca$sdev)

# Data for timeline
t1_data <- pca_out %>% 
  filter(`Principal Component` != "PC3" & is.na(`Principal Component`) == FALSE) %>% 
  mutate(PC_num = `Principal Component`,
         `Principal Component` = if_else(`Principal Component` == "PC1", 
                                         as.character(percent_explained$PC1), 
                                         as.character(percent_explained$PC2)),
         `Principal Component` = fct_rev(`Principal Component`))  %>% 
  select(Date, PC_num, everything())


# Timeline plot
timeline_raw <- t1_data %>% 
  ggplot(aes(x = Date,
             y = `Principal Component Loading`, 
             color = `Principal Component`)) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black", linetype = 2, alpha = 0.6) +
    ylim(-11, 11) +
    scale_color_gmri(palette = "mixed") +
    labs(x = NULL) +
    theme_minimal()
timeline_raw


# #Export plot
# ggsave(timeline_raw,
#        filename =  here::here("R", "new_anom_analyses", "figures", "pca_ts_raw.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")


####__ 3. Loadings on Imputed Data  ####

# Mapping the weights of the pca from the first mode
# Adjust sensor measurements by PC weights


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

# Data for plot
interp_timeline <- bind_rows(pc1_ts_i, pc2_ts_i) %>% 
  mutate(PC_num = `Principal Component`,
         `Principal Component` = if_else(`Principal Component` == "PC1", 
                                         as.character(percent_explained$PC1), 
                                         as.character(percent_explained$PC2)),
         `Principal Component` = fct_rev(`Principal Component`)) %>% 
  select(Date, PC_num, everything())



#Plot on interpolated timeline
(timeline_interp <- interp_timeline %>% 
    ggplot(aes(Date, `Principal Component Loading`, 
               color = `Principal Component`
               #alpha = `Principal Component`
               )) +
    geom_line() +
    geom_hline(yintercept = 0, color = "black", linetype = 2, alpha = 0.6) +
    scale_color_gmri(palette = "mixed") +
    ylim(-11, 11) +
    labs(x = NULL) +
    theme_minimal())



# # #Export plot
# ggsave(timeline_interp,
#        filename =  here::here("R", "new_anom_analyses", "figures", "pca_ts_interp.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")


# Save for future use
# write_csv(t1_data, here("R/new_anom_analyses/derived_data/buoy_pca_raw.csv"))
# write_csv(interp_timeline, here("R/new_anom_analyses/derived_data/buoy_pca_interp.csv"))


####__ 4. Patchwork Figure  ####

p1 <- sensor_p + theme(legend.position = "none")
p2 <- timeline_raw + theme(legend.position = c("none"))
p3 <- timeline_interp + 
  guides("color" = guide_legend(title.position = "top")) +
  theme(legend.position = "bottom", 
        legend.title.align = 0.5)



# Just the timelines
timelines_stacked <- p2 / p3
timelines_stacked

# #Export plot
# ggsave(timelines_stacked,
#        filename =  here::here("R", "new_anom_analyses", "figures", "buoy_pca_ts_stacked.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")





####__ 5. Offline Sensor Visual  ####
#Daily Reads
buoys <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoys_daily.csv", sep = "/"), col_types = cols())
buoys <- buoys %>% 
  mutate(
    #Create julian days for quarter assignment
    julian = lubridate::yday(Date), 
    period = case_when(
      julian <= 91                       ~ "Q1",
      between(julian, left = 92, 182)    ~ "Q2",
      between(julian, left = 183, 273)   ~ "Q3",
      julian > 273                       ~ "Q4"),
    reading_depth = factor(
      reading_depth, 
      levels = c("1 meter", "20 meters", "50 meters", 
                 "100 meters", "150 meters", "180 meters"))) 


####  Daily Anomalies Estimations  
daily_anoms <- buoys %>% 
  group_by(buoy_id, reading_depth, julian) %>% 
  summarise(avg_temp = mean(temp, na.rm = T),
            temp_sd  = sd(temp, na.rm = T),
            avg_sal = mean(sal, na.rm = T),
            sal_sd = sd(sal, na.rm = T)) %>% 
  right_join(buoys, by = c("buoy_id", "reading_depth", "julian"))  %>% 
  select(buoy_id, reading_depth, Date, julian, period, everything()) %>% 
  mutate(temp_anom = (temp - avg_temp) / temp_sd,
         sal_anom  = (sal - avg_sal) / sal_sd) %>% 
  ungroup()

# Maybe a heatmap, column for each day, color of cell corresponds to sensor status
daily_long <- daily_anoms %>% 
  split(.$buoy_id) %>% 
  map_dfr(function(buoy){
    buoy %>% select(buoy_id, reading_depth, Date, temp_anom, sal_anom) %>% 
      pivot_longer(names_to = "measure", values_to = "val", cols = c(temp_anom, sal_anom))})


# Reshape as a dataframe to use geom_tile, easier to format
matrix_df <- daily_long %>% 
  mutate(buoy_id  = str_sub(buoy_id, 6,6),
         reading_depth = str_remove_all(reading_depth, pattern = " meters| meter"),
         reading_depth = str_pad(reading_depth, width = 3, side = "left", pad = "0"),
         measure = str_remove_all(measure, "emp_anom|al_anom"),
         measure = str_to_upper(measure),
         ID = str_c(buoy_id, reading_depth, measure)) %>% 
  select(Date, ID, val) 

# Expand out the dates and ID's to get NA gaps for missing records
bin_df <- matrix_df %>% 
  expand(Date, ID) %>% 
  left_join(matrix_df) %>% 
  mutate(`Buoy Status` = ifelse(is.na(val), "Offline", "Online"),
         buoy = str_sub(ID, 1, 1),
         depth = str_sub(ID, 2, 4),
         sensor = str_sub(ID, -1, -1))

# Flag dates with no NA values
bin_df <- bin_df %>% 
  split(.$Date) %>% 
  map_dfr(function(buoy){
    any_na <- any(is.na(buoy$val))
    if(any_na == TRUE) {
      buoy_out <- mutate(buoy, `Array Status` = "Incomplete")
    } else { buoy_out <- mutate(buoy, `Array Status` = "Complete") }
    return(buoy_out)})

#with facets
sensor_status <- ggplot(bin_df, aes(Date, ID, fill = `Buoy Status`)) +
  geom_tile(aes(alpha = `Array Status`)) +
  facet_grid(buoy ~ ., scales = "free", switch = "y") +
  #gmRi::scale_fill_gmri(palette = "mixed") +
  scale_fill_manual(values = c("white", gmri_cols("gmri blue"))) +
  scale_alpha_discrete(range = c(range = c(1, 0.4))) +
  labs(x = "", y = "Buoy Sensor Status") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        panel.spacing = unit(0, "lines")) 


# plot with the timelines
trip_stack <- (sensor_status + theme(legend.position = "none")) / p2 / p3
trip_stack #+ plot_annotation(tag_levels = 'A') 

# ggsave(plot = trip_stack,
#        filename =  here::here("R", "new_anom_analyses", "figures", "buoy_pca_triple.png"),
#        device = "png",
#        height = 10, width = 8, units = "in")










####________________________________####
####  Applying weights to Quarterly CPR Data  ####

# Basically we want to apply the PCA weights to the buoy values at their quarterly means
# This gies us the loadings that correspond with the quarterly CPR concentrations

# Full Buoy Timeseries A. w/o  interpolated values
actual_means <- buoy_raw %>% 
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
cpr_quarters <- cpr_sst %>% 
  filter(period != "annual") %>% 
  pivot_wider(names_from = taxa, values_from = anomaly) %>% 
  rename(Year = year) %>% 
  select(-c("datebounds", "period_anom_n", "temp_anomaly", "lag_ref", "year_ref", "lag_temp"))



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
  labs(y = "Principal Component Loading", x = "")



#####__ Apply PC's to interpolated data  ####
interpolated_means$PC1 <- c(as.matrix(interpolated_means[, c(3:48)]) %*% loadings[,"PC1"]) #pc1
interpolated_means$PC2 <- c(as.matrix(interpolated_means[, c(3:48)]) %*% loadings[,"PC2"]) #pc1


ggplot(interpolated_means, aes(x = Year)) +
  geom_line(aes(y = PC1, color = "PC1")) +
  geom_line(aes(y = PC2, color = "PC2")) +
  facet_wrap(~period) +
  labs(y = "Principal Component Loading", x = "")



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
      period == "Q4" ~ 320),
    Q_Date = as.Date(paste(Year, jday, sep = "-"),"%Y-%j")) %>% 
  select(Year, period, jday, Q_Date, everything())

# Interpolated Timeline - Ordered by date of occurrence
ggplot(buoy_w_cpr, aes(x = Q_Date)) +
  geom_line(aes(y = PC1_interpolated, color = "Buoy PC1 Interpolated")) +
  geom_line(aes(y = PC2_interpolated, color = "Buoy PC2 Interpolated")) +
  labs(x = "Date", y = "Principal Component Loading") +
  scale_color_gmri(name = "", palette = "mixed")
# Non-interpolated, date of occurrence
ggplot(buoy_w_cpr, aes(x = Q_Date)) +
  geom_line(aes(y = PC1_actual, color = "Buoy PC1 Measured")) +
  geom_line(aes(y = PC2_actual, color = "Buoy PC2 Measured")) +
  labs(x = "Date", y = "Principal Component Loading") +
  scale_color_gmri(name = "", palette = "mixed")





####________________________________####
####  Correlations between buoy PCA loadings and Quarterly Concentrations  ####

# Data going in: Quarterly Averaged PCA loadings - Quarterly Averaged CPR Data
buoy_w_cpr

# prep for corrplot functions
buoy_cpr_mat <- buoy_w_cpr %>% select(-jday, -Q_Date) %>% rename(year = Year) %>% ungroup()
actual_prepped <- buoy_cpr_mat %>% 
  rename(PC1 = PC1_actual, PC2 = PC2_actual) %>% 
  select(-PC1_interpolated, -PC2_interpolated) %>% drop_na()
interp_prepped <- buoy_cpr_mat %>% 
  rename(PC1 = PC1_interpolated, PC2 = PC2_interpolated) %>% 
  select(-PC1_actual, -PC2_actual) %>% drop_na()

# correlations and significance
observed_val_corrs <- actual_prepped %>% 
  split(.$period) %>% 
  map(function(x){ungroup(x) %>% select(-period) %>% corr_plot_setup()})
interp_val_corrs <- interp_prepped %>% 
  split(.$period) %>% 
  map(function(x){ungroup(x) %>% select(-period) %>% corr_plot_setup()})


# cpr corr plot needs to be changed
my_taxa <- species_05


# Actual data first
obs_plots <- observed_val_corrs %>% imap(function(x, y) {
  if(y == "Q1") {
    cpr_corr_plot(x, period = y, taxa = my_taxa, plot_style = "wide")
  } else {
    cpr_corr_plot(x, period = y, taxa = my_taxa, plot_style = "wide") + theme(axis.text.y = element_blank())
  }
})

# And again for the interpolated data
interp_plots <- interp_val_corrs %>% imap(function(x, y) {
  if(y == "Q1") {
    cpr_corr_plot(x, period = y, taxa = my_taxa, plot_style = "wide")
  } else {cpr_corr_plot(x, period = y, taxa = my_taxa, plot_style = "wide") + theme(axis.text.y = element_blank())}
})


#Patch them together
obs_quarterly_corrplot <- obs_plots[[1]] | obs_plots[[2]] | obs_plots[[3]] | obs_plots[[4]]
obs_quarterly_corrplot <- obs_quarterly_corrplot & theme(legend.position = "none")
obs_quarterly_corrplot <- obs_quarterly_corrplot + labs(caption = "PCA Loadings Applied to all Non-NA records")
obs_quarterly_corrplot


#Patch them together
interp_quarterly_corrplot <- interp_plots[[1]] | interp_plots[[2]] | interp_plots[[3]] | interp_plots[[4]]
interp_quarterly_corrplot <- interp_quarterly_corrplot & theme(legend.position = "none")
interp_quarterly_corrplot <- interp_quarterly_corrplot + labs(caption = "PCA Loadings Applied to all Imputed Measurements")
interp_quarterly_corrplot



# Save them
# ggsave(plot = obs_quarterly_corrplot,
#        filename = here::here("R", "new_anom_analyses", "figures", "quarterly_buoy_pca_correlations_actual.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")
# ggsave(plot = interp_quarterly_corrplot,
#        filename = here::here("R", "new_anom_analyses", "figures", "quarterly_buoy_pca_correlations_interpolated.png"),
#        device = "png",
#        height = 6, width = 8, units = "in")

# Possible better way to impute gappy matrix
# http://menugget.blogspot.com/2012/10/dineof-data-interpolating-empirical.html




# Side-by-side?
(obs_quarterly_corrplot + labs(caption = "")) | (interp_quarterly_corrplot + labs(caption = ""))















#### PCA + CPR + SST  ####

# Goal: Add the SST anomaly for the matching quarter to the bottom
