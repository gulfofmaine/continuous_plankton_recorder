#### CPR Dataset - Principal Component Analysis w/ SST
#### Adam A. Kemberling
#### 12/17/2019

####  Packages  ####
library(ggbiplot)
library(patchwork)
library(tidyverse)
library(corrplot)
library(here)


####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####  Load Data  ####
# CPR Dataset with quarterly anomalies and SST with a one-period lag
# souce: 02b_quarterly_cpr_sst_regressions
cpr_sst <- read_csv(str_c(cpr_boxpath, "data/processed_data/quarterly_cpr_sst.csv", sep = "/"), 
                    col_names = TRUE,
                    col_types = cols())

#CPR Dataset with annual anomalies
species_periods_long <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "cpr_with_SSTlags.csv", sep = "/"),
                                 col_types = cols()) %>%  
  mutate(period = case_when(
    period == "annual" ~ "Annual",
    period == "jf" ~ "January - February",
    period == "ma" ~ "March - April",
    period == "mj" ~ "May - June",
    period == "ja" ~ "July - August",
    period == "so" ~ "September - October",
    period == "nd" ~ "November - December"),
    period = factor(period, levels = c("Annual", "January - February",
                                       "March - April", "May - June", 
                                       "July - August", "September - October", 
                                       "November - December")))


#Target species
species_05 <- c("calanus", "centropages", "oithona", "para_pseudocalanus",
                "metridia", "calanus1to4", "euphausiacea")

#Correlation vars
corr_vars <- c("calanus", "centropages", "oithona", "para_pseudocalanus",
               "metridia", "calanus1to4", "euphausiacea", "temp_anomaly")

#Color Scale
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

########



####  1. SST Quarterly Correlations  ####



#This will be tricky because we want annual plankton anomalies, but seasonal temperatures
pull_period <- function(time_period = NULL) {
  
  plankton_ts <- species_periods_long %>% 
    filter(period == "Annual") %>% 
    pivot_wider(names_from = species, values_from = anomaly) %>% 
    select(year, one_of(species_05))
  
  temp_ts <- cpr_sst %>% 
    distinct(year, period, .keep_all = T) %>% 
    pivot_wider(names_from = period, values_from = temp_anomaly) %>% 
    #filter(period == time_period) %>% 
    select(year, one_of(time_period))
  
  df_out <- inner_join(plankton_ts, temp_ts, by = "year") %>% drop_na()
  return(df_out)
  
}

#Make list of the data groups
period_df_list <- list(
  "Q1"  = pull_period(time_period = "Q1"),
  "Q2"  = pull_period(time_period = "Q2"),
  "Q3"  = pull_period(time_period = "Q3"),
  "Q4"  = pull_period(time_period = "Q4")
)


#Plotting Function
plot_corr <- function(df, id) {
  
  #prep dataframe by removing the year column
  df <- df %>% select(-year) %>% cor()
  
  #save name
  save_name <- str_c(here::here("R/presentations/corrplots/quarterly"), "/", id, ".png")
  
  png(save_name, width = 800, height = 600, res = 100)
  corrplot::corrplot(df, method="color", col=col(200),  
                     # hide correlation coefficient on the principal diagonal
                     #diag=FALSE, 
                     type = "lower", 
                     #title = id, 
                     addCoef.col = "black", # Add coefficient of correlation
                     #Adjust Margin
                     mar=c(0,0,2,2) 
  )
  dev.off()
}



#Plot them all
imap(period_df_list, plot_corr)


#Could also put them together and plot in one...
pull_temp_corr <- function(df, id) {
  
  #prep dataframe by removing the year column
  df <- df %>% select(-year) %>% cor()
  
  df[id,]
}

#Pull out the temperature correlation from each and stack them in a matrix
temp_period_corrs <- imap(period_df_list, pull_temp_corr) %>% 
  bind_rows() %>% 
  as.matrix()
rownames(temp_period_corrs) <- c(species_05, "temp_anomaly")
temp_period_corrs <- temp_period_corrs[1:7,] #Drop temperature row

png(str_c(here::here("R/presentations/corrplots/quarterly/all_seasons.png")), width = 800, height = 600, res = 100)
corrplot::corrplot(temp_period_corrs, method="color", col=col(200),  
                   # hide correlation coefficient on the principal diagonal
                   #title = "Seasonal Temperature Anomalies - Species Correlations \n 1982-2018", 
                   addCoef.col = "black", # Add coefficient of correlation
                   #Adjust Margin
                   mar=c(0,0,2,2) 
)
dev.off()


####  2. Lagged SST Quarterly Correlations


#This will be tricky because we want annual plankton anomalies, but seasonal temperatures
pull_lag_period <- function(time_period = NULL) {
  
  plankton_ts <- species_periods_long %>% 
    filter(period == "Annual") %>% 
    pivot_wider(names_from = species, values_from = anomaly) %>% 
    select(year, one_of(species_05))
  
  temp_ts <- cpr_sst %>% 
    distinct(year, period, .keep_all = T) %>% 
    pivot_wider(names_from = period, values_from = lag_temp) %>% 
    #filter(period == time_period) %>% 
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

lag_period_corrs <- imap(lagged_df_list, pull_temp_corr) %>% 
  bind_rows() %>% 
  as.matrix()
rownames(lag_period_corrs) <- c(species_05, "temp_anomaly")
lag_period_corrs <- lag_period_corrs[1:7,] #Drop temperature row

#Rename columns to show lag structure
colnames(lag_period_corrs) <- c("Q4 SST & Q1 CPR", "Q1 SST & Q2 CPR", "Q2 SST & Q3 CPR", "Q3 SST & Q4 CPR")

png(str_c(here::here("R/presentations/corrplots/quarterly/all_seasons_lagged.png")), width = 800, height = 700, res = 100)
corrplot::corrplot(lag_period_corrs, method="color", col=col(200),  
                   # hide correlation coefficient on the principal diagonal
                   #title = "Seasonal Temperature Anomalies - Species Correlations \n 1982-2018", 
                   addCoef.col = "black", # Add coefficient of correlation
                   #Adjust Margin
                   mar=c(0,0,2,2) 
)
dev.off()
