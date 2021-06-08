####  Change Point Analysis Revisit - 2020
####  Input data: 2020 New Anomalies
####


####  Packages  ####
library(strucchange)
library(changepoint)
library(cpm)
library(bcp)
library(zoo)
library(tidyverse)
library(patchwork)
library(gmRi)
library(here)


####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))


####  Data  ####

####__CPR####
# CPR Dataset with quarterly anomalies and SST with a one-period lag
# souce: 03_new_anoms_quarterly_sst.R
cpr_sst <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "anomalies_w_quarterlysst.csv", sep = "/"),
                    col_types = cols(),
                    guess_max = 1e5)


#Reference Taxa
species_05 <- c("calanus_finmarchicus_v_vi", "centropages_typicus", "oithona_spp","para_pseudocalanus_spp", 
                "metridia_lucens", "calanus_i_iv", "euphausiacea_spp")

# Filter species, add date at midpoint for each period
cpr_sst <- cpr_sst %>% filter(taxa %in% species_05) %>% 
  mutate(yday = case_when(
    period == "Q1"       ~ 45,
    period == "Q2"       ~ 136,
    period == "Q3"       ~ 227,
    period == "Q4"       ~ 318,
    period == "annual"   ~ 182
  )) %>% 
  mutate(p_date = as.Date(str_c(year, "-01-01")),
         p_date = p_date + yday)


####__BuoyData####
# Gappy Buoy data - source: 10_buoy_daily_interpolations
buoy_raw <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_raw.csv", sep = "/"),
                     col_types = cols(),
                     guess_max = 1e5)

# Interputed NA Buoy data - source: 10_buoy_daily_interpolations.R
buoy_i <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_interpolated.csv", sep = "/"),
                   col_types = cols())


####__BuoyPCA####
# Buoy PCA timelines
bpca_1 <- read_csv(here("R/new_anom_analyses/derived_data/buoy_pca_raw.csv"), guess_max = 1e6, col_types = cols())
bpca_2 <- read_csv(here("R/new_anom_analyses/derived_data/buoy_pca_interp.csv"), guess_max = 1e6, col_types = cols()) %>% 
  mutate(PC = case_when(`Principal Component` == "48.44% of Variance" ~ "PC1",
                        TRUE ~ "PC2"))



####________________________#########
####  Data Exploration  ####

# Temporal changes in Buoy Principal components
####  1. Temporal Changes - RUNSUM  ####
bpca_2 %>% 
  ggplot(aes(Date, `Principal Component Loading`)) +
  geom_line(color = "gray80") +
  geom_smooth(formula = y ~ s(x, bs = "cs"),
              method = "gam", 
              color = gmri_cols("gmri blue"), 
              se = FALSE) +
  facet_wrap(~PC) +
  labs(x = NULL, y = "Population Anomaly", subtitle = "Buoy PCA Modes - Imputed Timeline")




####  2. Running Sum Plots  ####

# Cumulative sum plot of Principal component loadings
runsum_tl <- bpca_2 %>% 
  split(.$PC) %>% 
  map(~.x %>% mutate(`Running Sum` = cumsum(`Principal Component Loading`))) %>% 
  bind_rows() %>% 
  ggplot(aes(Date, `Running Sum`)) +
  geom_hline(yintercept = 0, linetype = 2, color = gmri_cols("orange"), alpha = 0.3) +
  geom_line(color = gmri_cols("gmri blue"), group = 1) +
  labs(x = NULL, caption = "Running sum of Principal Component Loading, i.e. mode switches.") +
  facet_wrap(~PC, ncol = 1)


# Plot Calanus against Time
calanus <- c("calanus_i_iv", "calanus_finmarchicus_v_vi")
cal_tl <- cpr_sst %>% filter(#year >= 2000, 
                   taxa %in% calanus,
                   period != "annual")  %>% 
  mutate(taxa = fct_rev(taxa)) %>% 
  ggplot(aes(p_date, anomaly)) +
    geom_point(color = "gray80") +
    geom_line(color = "gray80") +
    geom_smooth(formula = y ~ s(x, bs = "cs"),
                method = "gam",
                se = TRUE,
                aes(color = taxa), 
                show.legend = FALSE) +
    scale_color_gmri(palette = "mixed") +
    facet_wrap(~taxa, ncol = 1) +
    labs(x = "", y = "Standardized Abundance Anomaly")
    

runsum_tl / cal_tl




# CPR taxa running summs
cpr_sst %>% 
  filter(
    #year >= 2000, 
    #taxa %in% calanus,
    period == "annual")  %>%  
    arrange(p_date) %>% 
  split(.$taxa) %>% 
  map(~.x %>% mutate(`Running Sum` = cumsum(anomaly))) %>% 
  bind_rows() %>% 
  ggplot(aes(p_date, `Running Sum`)) +
    geom_hline(yintercept = 0, linetype = 2, color = gmri_cols("orange"), alpha = 0.3) +
    geom_line(color = gmri_cols("gmri blue"), group = 1) +
    labs(x = NULL, caption = "Running sum of population anomalies, i.e. deviance from predicted mean from spline.") +
    facet_wrap(~taxa)





cpr_sst %>% 
  filter(
    #year >= 2000, 
    #taxa %in% calanus,
    period == "annual")  %>%  
  split(.$taxa) %>% 
  map(function(x){
    data.frame(avg_anom = mean(x$anomaly))
    }) %>% bind_rows(.id = "taxa")





#Plotting running sum of lagged differences
runsum_plots <- bpca_2 %>% 
  split(.$PC) %>% 
  map(~diff(.x$`Principal Component Loading`)) %>% 
  imap(function(.x, .y){
    
    #Cumsulative Sum Plot of lagged differences
    cs_df <- tibble(x = c(1:length(.x)),
                    `Running Sum` = cumsum(.x))
    
    ggplot(cs_df, aes(x, `Running Sum`)) +
      geom_area(fill = gmri_cols("gmri blue"), alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = 2, color = gmri_cols("orange"), alpha = 0.3) +
      geom_line(color = gmri_cols("gmri blue"), group = 1) +
      #geom_point(color = gmri_cols("gmri blue"),) +
      labs(title = .y,
           x = NULL,
           y = "Running Sum - Lagged Principal Component Loadings")
    
    
  })

runsum_plots$PC1 /runsum_plots$PC2


####  3.  Locate Breakpoints  ####
#Using the strucchange package to locate breakpoints:

#Empirical Fluctuation Process CUSUM process
efp_list <- bpca_2 %>% 
  split(.$PC) %>% 
  imap(function(.x, .y) {
    
    efp_df <- tibble(
      PC           = .y,
      date         = .x$Date[2:nrow(.x)],
      daily_diffs  = diff(.x$`Principal Component Loading`),
      rolling_sum  = cumsum(daily_diffs) #not sure if we need to do this or if efp does it
    )
    
    #Perform ordinary least-squared CUSUM estimation
    efp <- list(
      daily_diffs = strucchange::efp(daily_diffs ~ date, type = "OLS-CUSUM", data = efp_df),
      rolling_sum = strucchange::efp(rolling_sum ~ date, type = "OLS-CUSUM", data = efp_df)
    )

    return(efp)
    
  })

summary(efp_list$PC1$daily_diffs)


#Plotting the CUSUM plots
imap(efp_list$PC1, ~plot(.x, main = str_c("OLS-based CUSUM test \n PC1 - ", .y)))
imap(efp_list$PC2, ~plot(.x, main = str_c("OLS-based CUSUM test \n PC2 - ", .y)))






#Getting breakpoints - double check formula
bp_list <-bpca_2 %>% 
  split(.$PC) %>% 
  imap(function(.x, .y) {
    
    .x$lag1 <- lag(.x$`Principal Component Loading`, n = 1)
    
    ###
    #Attempt 2: Match efp formula
    bp_df <- tibble(
      PC           = .y,
      date         = .x$Date[2:nrow(.x)],
      daily_diffs  = diff(.x$`Principal Component Loading`),
      rolling_sum  = cumsum(daily_diffs) #not sure if we need to do this or if efp does it
    )
    breakpoints(daily_diffs ~ date, data = bp_df, h = 0.1)
    ###
    
  })

#plot(bp_list$PC1)
