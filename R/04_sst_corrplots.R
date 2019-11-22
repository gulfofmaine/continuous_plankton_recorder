#### CPR Dataset - Principal Component Analysis w/ SST
#### Adam A. Kemberling
#### 11/22/2019

####  Packages  ####
library(ggbiplot)
library(tidyverse)
library(corrplot)
library(here)


####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))



####  Load Data  ####
# SST long-format with the lag periods
sst_long_lagged <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "SST_with_lags.csv", sep = "/")) %>% 
  mutate(period = case_when(
    period == "annual" ~ "Annual",
    period == "jf" ~ "January - February",
    period == "ma" ~ "March - April",
    period == "mj" ~ "May - June",
    period == "ja" ~ "July - August",
    period == "so" ~ "September - October",
    period == "nd" ~ "November - December"),
    period = factor(period, 
                    levels = c("Annual", "January - February", "March - April", "May - June", "July - August", "September - October", "November - December")))

# CPR species Data with the corrresponding SST
species_periods_long <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "cpr_with_SSTlags.csv", sep = "/"))  %>% 
  mutate(period = case_when(
    period == "annual" ~ "Annual",
    period == "jf" ~ "January - February",
    period == "ma" ~ "March - April",
    period == "mj" ~ "May - June",
    period == "ja" ~ "July - August",
    period == "so" ~ "September - October",
    period == "nd" ~ "November - December"),
    period = factor(period, 
                    levels = c("Annual", "January - February", "March - April", "May - June", "July - August", "September - October", "November - December")))



####  SST Plankton Correlations  ####
#Target species
species_05 <- c("calanus", "centropages", "oithona", "para_pseudocalanus",
                "metridia", "calanus1to4", "euphausiacea")

#Correlation vars
corr_vars <- c("calanus", "centropages", "oithona", "para_pseudocalanus",
               "metridia", "calanus1to4", "euphausiacea", "temp_anomaly")

#Correlation Matrix needed to plot
cor_data <- species_periods_long %>% 
  filter(period == "Annual") %>% 
  pivot_wider(names_from = species, values_from = anomaly) %>% 
  select(year, one_of(corr_vars))

#### 1. Whole Period  ####
full_ts <- cor_data %>% 
  filter(year >= 1982) %>% 
  select(-year) %>% 
  cor()

#Color Scale
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

png(here::here("R/presentations/corrplots/corrplot_full.png"), width = 800, height = 600, res = 100)
corrplot::corrplot(full_ts, method="color", col=col(200),  
                   # hide correlation coefficient on the principal diagonal
                   diag=FALSE, 
                   type = "lower", 
                   #title = "1982-2018", 
                   addCoef.col = "black", # Add coefficient of correlation
                   #Adjust Margin
                   mar=c(0,0,2,2) # http://stackoverflow.com/a/14754408/54964
)
dev.off()


#### 2. Pre-trend  ####
pre_trend <- cor_data %>% 
  filter(between(year, 1982, 2003)) %>% 
  select(-year) %>% 
  cor()

png(here::here("R/presentations/corrplots/corrplot_pre.png"), width = 800, height = 600, res = 100)
corrplot::corrplot(pre_trend, method="color", col=col(200),  
                   # hide correlation coefficient on the principal diagonal
                   diag=FALSE, 
                   type = "lower", 
                   #title = "1982-2000", 
                   addCoef.col = "black", # Add coefficient of correlation
                   #Adjust Margin
                   mar=c(0,0,2,2) 
)
dev.off()

#### 3. Post-trend  ####
post_trend <- cor_data %>% 
  filter(year > 2003) %>% 
  select(-year) %>% 
  cor()

#Significance test
p.mat <- cor.mtest(post_trend)


png(here::here("R/presentations/corrplots/corrplot_post.png"), width = 800, height = 600, res = 100)
corrplot::corrplot(post_trend, method="color", col=col(200),  
         # hide correlation coefficient on the principal diagonal
         #diag=FALSE, 
         type = "lower", 
         #title = "2000-2018", 
         addCoef.col = "black", # Add coefficient of correlation
         #Adjust Margin
         mar=c(0,0,2,2) 
)
dev.off()

####  4. SST Periods  ####

#This will be tricky because we want annual plankton anomalies, but seasonal temperatures
pull_period <- function(time_period = NULL) {
  
  plankton_ts <- species_periods_long %>% 
    filter(period == "Annual") %>% 
    pivot_wider(names_from = species, values_from = anomaly) %>% 
    select(year, one_of(species_05))
  
  temp_ts <- species_periods_long %>% 
    distinct(year, period, .keep_all = T) %>% 
    pivot_wider(names_from = period, values_from = temp_anomaly) %>% 
    #filter(period == time_period) %>% 
    select(year, one_of(time_period))
  
  df_out <- inner_join(plankton_ts, temp_ts, by = "year") %>% drop_na()
  return(df_out)
  
}

#Make list of the data groups
period_df_list <- list(
  "Annual"              = pull_period(time_period = "Annual"),
  "January - February"  = pull_period(time_period = "January - February"),
  "March - April"       = pull_period(time_period = "March - April"),
  "May - June"          = pull_period(time_period = "May - June"),
  "July - August"       = pull_period(time_period = "July - August"),
  "September - October" = pull_period(time_period = "September - October"),
  "November - December" = pull_period(time_period = "November - December")
)


#Plotting Function
plot_corr <- function(df, id) {
  
  #prep dataframe by removing the year column
  df <- df %>% select(-year) %>% cor()
  
  #save name
  save_name <- str_c(here::here("R/presentations/corrplots"), "/", id, ".png")
   
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

png(str_c(here::here("R/presentations/corrplots/all_seasons.png")), width = 800, height = 600, res = 100)
corrplot::corrplot(temp_period_corrs, method="color", col=col(200),  
                   # hide correlation coefficient on the principal diagonal
                   #title = "Seasonal Temperature Anomalies - Species Correlations \n 1982-2018", 
                   addCoef.col = "black", # Add coefficient of correlation
                   #Adjust Margin
                   mar=c(0,0,2,2) 
)
dev.off()






####__####
####  PCA weights for entire time series  ####
pca_full_data <- species_periods_long %>% 
  filter(period == "Annual") %>% 
  pivot_wider(names_from = species, values_from = anomaly) %>% 
  select(year, one_of(species_05))

pca_full_vals <- pca_full_data %>% select(-year) %>% drop_na()

#PCA - full timeseries
pca_full <- prcomp(pca_full_vals, center = F, scale. = F)
summary(pca_full)


#leading_modes <- rownames_to_column(as.data.frame(pca_full$rotation)) %>% dplyr::select(species = rowname, PC1, PC2)


# PCA Weights Figure
(weights_fig <- rownames_to_column(as.data.frame(pca_full$rotation)) %>% 
    dplyr::select(species = rowname, PC1, PC2)%>% 
    dplyr::rename("First Mode (60.2%)" = PC1,
                  "Second Mode (17.2%)" = PC2) %>% 
    gather(key = "PC", value =  "Principal Component Weight", 
           `First Mode (60.2%)`, `Second Mode (17.2%)`) %>% 
    mutate(species = factor(species, 
                            levels = c("calanus", "centropages", "oithona","para_pseudocalanus", 
                                       "metridia", "calanus1to4", "euphausiacea"))) %>% 
    ggplot(aes(species, `Principal Component Weight` * -1, fill = PC)) +
    geom_col(position  = "dodge") +
    scale_fill_gmri(palette = "mixed") +
    labs(x = "", title = "PCA leading Mode Weights", subtitle = "1982-2018") +
    theme(legend.position = c(0.85, 0.2)))
ggsave(weights_fig, filename =  here::here("R", "presentations", "full_ts_pca.png"), device = "png")



####  PCA post - 2003  ####
pca_post_data <- species_periods_long %>% 
  filter(period == "Annual", 
         year >= 2003) %>% 
  pivot_wider(names_from = species, values_from = anomaly) %>% 
  select(year, one_of(species_05))

pca_post_vals <- pca_post_data %>% select(-year) %>% drop_na()

#PCA - full timeseries
pca_post <- prcomp(pca_post_vals, center = F, scale. = F)
summary(pca_post)


#leading_modes <- rownames_to_column(as.data.frame(pca_full$rotation)) %>% dplyr::select(species = rowname, PC1, PC2)


# PCA Weights Figure
(weights_fig_post <- rownames_to_column(as.data.frame(pca_post$rotation)) %>% 
    dplyr::select(species = rowname, PC1, PC2)%>% 
    dplyr::rename("First Mode (46.3%)" = PC1,
                  "Second Mode (27.8%)" = PC2) %>% 
    gather(key = "PC", value =  "Principal Component Weight", 
           `First Mode (46.3%)`, `Second Mode (27.8%)`) %>% 
    mutate(species = factor(species, 
                            levels = c("calanus", "centropages", "oithona","para_pseudocalanus", 
                                       "metridia", "calanus1to4", "euphausiacea"))) %>% 
    ggplot(aes(species, `Principal Component Weight` * -1, fill = PC)) +
    geom_col(position  = "dodge") +
    scale_fill_gmri(palette = "mixed") +
    labs(x = "", title = "PCA leading Mode Weights", subtitle = "2003-2018") +
    theme(legend.position = c(0.25, 0.8)))
ggsave(weights_fig_post, filename =  here::here("R", "presentations", "post_shift_pca.png"), device = "png")




####  Buoy Data  ####
load(file = str_c(cpr_boxpath, "/data/processed_data/Buoy.RData"))

#Buoys are lists with dataframes by depth
Buoys$Buoy_B$depth_1m
