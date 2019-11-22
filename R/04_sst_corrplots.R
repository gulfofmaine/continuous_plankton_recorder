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
corrplot::corrplot(full_ts, method="color", col=col(200),  
                   # hide correlation coefficient on the principal diagonal
                   diag=FALSE, 
                   type = "lower", 
                   title = "1982-2018", 
                   addCoef.col = "black", # Add coefficient of correlation
                   #Adjust Margin
                   mar=c(0,0,2,2) # http://stackoverflow.com/a/14754408/54964
)

#### 2. Pre-trend  ####
pre_trend <- cor_data %>% 
  filter(between(year, 1982, 2000)) %>% 
  select(-year) %>% 
  cor()

corrplot::corrplot(pre_trend, method="color", col=col(200),  
                   # hide correlation coefficient on the principal diagonal
                   diag=FALSE, 
                   type = "lower", 
                   title = "1982-2000", 
                   addCoef.col = "black", # Add coefficient of correlation
                   #Adjust Margin
                   mar=c(0,0,2,2) 
)

#### 3. Post-trend  ####
post_trend <- cor_data %>% 
  filter(year > 2000) %>% 
  select(-year) %>% 
  cor()

#Significance test
p.mat <- cor.mtest(post_trend)


corrplot::corrplot(post_trend, method="color", col=col(200),  
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, 
         type = "lower", 
         title = "2000-2018", 
         addCoef.col = "black", # Add coefficient of correlation
         #Adjust Margin
         mar=c(0,0,2,2) 
)

####  PCA weights for entire time series  ####
species_05 <- c("calanus", "centropages", "oithona", "para_pseudocalanus",
                "metridia", "calanus1to4", "euphausiacea")

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
    labs(x = "", title = "PCA leading Mode Weights", subtitle = "1982-2018") +
    theme(legend.position = c(0.85, 0.2)))


