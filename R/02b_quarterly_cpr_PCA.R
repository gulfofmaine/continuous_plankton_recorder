#### CPR Dataset - Principal Component Analysis - QUARTERLY Anomalies
#### Adam A. Kemberling
#### 12/02/2019

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
cpr_long <- read_csv(str_c(cpr_boxpath,"data", "processed_data", "cpr_allspecies_long_quarters.csv", sep = "/")) %>% 
  mutate(
    period = case_when(
      period == "annual" ~"Annual",
      period == "q1" ~"Q1",
      period == "q2" ~"Q2",
      period == "q3" ~"Q3",
      period == "q4" ~"Q4",
      TRUE ~ "Missed One"
    )
  )



####  Quarterly Species Abundance Timelines  ####
species_list <- list(
  "calanus" = filter(cpr_long, species == "calanus"),
  "calanus1to4" = filter(cpr_long, species == "calanus1to4"),
  "centropages" = filter(cpr_long, species == "centropages"),
  "euphausiacea" = filter(cpr_long, species == "euphausiacea"),
  "metridia" = filter(cpr_long, species == "metridia"),
  "oithona" = filter(cpr_long, species == "oithona"),
  "para_pseudocalanus" = filter(cpr_long, species == "para_pseudocalanus")
)

# #Export Timelines
# imap(species_list, function(x, y) {
#   species_plot <- ggplot(x, aes(year, anomaly)) +
#     geom_hline(yintercept = 0, alpha = 0.3, color = "darkred", linetype = 2) +
#     geom_point(color = gmri_cols("gmri blue")) +
#     geom_line(color = gmri_cols("gmri blue"), group = 1) +
#     labs(x = NULL, y = NULL, caption = y) +
#     facet_wrap(~period, ncol = 2)
#   
#   ggsave(plot = species_plot, filename = here::here("R", "presentations", "quarterly_timelines", str_c(y, ".png")), device = "png")
#   
# })





####  Quarterly SST Measures  ####
sst <- read.table(str_c(cpr_boxpath, "data", "ENV", "GoMSST_quartlery.txt", sep = "/")) %>% 
  dplyr::rename(year = V1,
                Q1   = V2,
                Q2   = V3,
                Q3   = V4,
                Q4   = V5)

sst_long <- sst %>% pivot_longer(names_to = "period", cols = Q1:Q4, values_to = "temp_anomaly")




####  Setting up Period Lags  ####
lag_key <- sst_long %>% 
  mutate(
    lag_ref = case_when(
      period == "Q1" ~ "Q4",
      period == "Q2" ~ "Q1",
      period == "Q3" ~ "Q2",
      period == "Q4" ~ "Q3"),
    year_ref = if_else(lag_ref == "Q1", year - 1, year)) %>% 
  select(period = lag_ref, year = year_ref)

temp_lags <- lag_key %>% left_join(sst_long, by = c("period", "year")) %>% 
  dplyr::rename(lag_ref = period, year_ref = year, lag_temp = temp_anomaly) %>% 
  mutate(
    period = case_when(
      lag_ref == "Q4" ~ "Q1",
      lag_ref == "Q1" ~ "Q2",
      lag_ref == "Q2" ~ "Q3",
      lag_ref == "Q3" ~ "Q4"),
    year = if_else(lag_ref == "Q4", year_ref + 1, year_ref)) 

#Add to original 
sst_long_lagged <- left_join(sst_long, temp_lags, by = c("period", "year"))

#Plot to test
ggplot(sst_long_lagged, aes(year)) +
  geom_line(data = filter(sst_long_lagged, period == "Q1"), aes(year, temp_anomaly, color = "Q1 Present")) + #Present Q1 Anomalies
  geom_line(data = filter(sst_long_lagged, period == "Q2"), aes(year, lag_temp, color = "Q1 lagged")) #Should match Q2's lag


####  Pair cpr data with quarterly anomalies  ####
cpr_sst <- cpr_long %>%
  filter(period != "Annual") %>% 
  left_join(sst_long_lagged, by = c("year", "period"))


#Timelines
cpr_sst %>% 
  filter(species == "calanus") %>% 
  ggplot(aes(year, anomaly, fill = temp_anomaly)) + 
    geom_hline(yintercept = 0, linetype = 2, color = "gray", alpha = 0.3) +
    geom_col(color = "gray50", size = 0.1) +
    scale_fill_gradientn(colours = c("steelblue", "white", "darkred")) +
    scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
    facet_grid(period~ species) +
    labs(x = NULL, y = NULL) +
    theme(panel.grid.major.x = element_line(color = "gray80", size = 0.2),
          panel.grid.minor.x = element_line(color = "gray80", size = 0.2))

#Using previous quarter's temperatures
cpr_sst %>% 
  filter(species == "calanus") %>% 
  ggplot(aes(year, anomaly, fill = lag_temp)) + 
  geom_hline(yintercept = 0, linetype = 2, color = "gray", alpha = 0.3) +
  geom_col(color = "gray50", size = 0.1) +
  scale_fill_gradientn(colours = c("steelblue", "white", "darkred")) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  facet_grid(period~ species) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.major.x = element_line(color = "gray80", size = 0.2),
        panel.grid.minor.x = element_line(color = "gray80", size = 0.2))

#Correlations

#Temperature CPR in sync
library(ggpmisc)

temp_anom_plots <- cpr_sst %>% 
  split(.$species) %>% 
  map(function(.x) {
    
    #Add Equation and r-squared values?  
    my.formula <- y ~ x
    
    plot_out <- .x %>% 
      ggplot(aes(anomaly, temp_anomaly, fill = temp_anomaly)) + 
      geom_smooth(method = "lm", se=FALSE, 
                  color = "gray", 
                  formula = my.formula) +
      geom_point(shape = 21, color = "gray50") +
      scale_fill_gradientn(colours = c("steelblue", "white", "darkred")) +
      facet_grid(period ~ species) +
      stat_poly_eq(data = .x,
                   formula = my.formula, 
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +  
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "Population Anomaly", y = "Temperature Anomaly")
  }
)


#Previous quarter's Temperature with  CPR
lag_temp_plots <- cpr_sst %>% 
  split(.$species) %>% 
  map(function(.x) {
      
    #Add Equation and r-squared values?    
    my.formula <- y ~ x
    
    .x %>% 
      ggplot(aes(anomaly, lag_temp, fill = lag_temp)) + 
      geom_smooth(method = "lm", se=FALSE, 
                  color = "gray", 
                  formula = my.formula) +
      geom_point(shape = 21, color = "gray50") +
      scale_fill_gradientn(colours = c("steelblue", "white", "darkred")) +
      facet_grid(period ~ species) +
      stat_poly_eq(data = .x,
                   formula = my.formula, 
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +  
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "Population Anomaly", y = "Previous Quarter's Temperature Anomaly")
  }
)


#Put them together to export them
library(patchwork)

twin_plots <- map2(temp_anom_plots, lag_temp_plots, function(x,y) {
  new_plot <- x + y
})

#Check them out
twin_plots

twin_plots %>% imap(function(x,y) {
  ggsave(x, filename = here::here("R", "presentations", "temp_regressions", str_c(y, "_temp_corr.png")), device = "png")
})
