#### New Anomalies Quarterly SST Regressions with NOAA/SAHFOS CPR Taxa  ####
#### Adam A. Kemberling
#### 3/11/2020

####  Packages  ####
library(tidyverse)
library(here)
library(gmRi)
library(patchwork)
library(ggpmisc)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))


####  NOAA + SAHFOS Anomaly Data  ####
cpr_wide <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "detrended_anomalies_noaa_sahfos.csv", sep = "/"), 
                     guess_max = 1e6, 
                     col_types = cols())

cpr_long <- cpr_wide %>% pivot_longer(names_to = "taxa", values_to = "anomaly", cols = 5:ncol(.))

#Reference Taxa
species_05 <- c("calanus_finmarchicus_v_vi", "centropages_typicus", "oithona_spp","para_pseudocalanus_spp", 
                "metridia_lucens", "calanus_i_iv", "euphausiacea_spp")



####  Match CPR withQuarterly SST  ####

#Change period labels in cpr data
cpr_long <- cpr_long %>% 
  mutate(period = case_when(
    period == "1" ~ "Q1",
    period == "2" ~ "Q2",
    period == "3" ~ "Q3",
    period == "4" ~ "Q4",
    period == "annual" ~ "annual"
  ))


####__Setting up Period Lags  ####
sst <- read.table(str_c(cpr_boxpath, "data", "ENV", "GoMSST_quartlery.txt", sep = "/")) %>% 
  dplyr::rename(year = V1,
                Q1   = V2,
                Q2   = V3,
                Q3   = V4,
                Q4   = V5)

sst_long <- sst %>% pivot_longer(names_to = "period", cols = Q1:Q4, values_to = "temp_anomaly")
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

###__Add Annual SST + lags  ####
# Bring in SST
sst_bimonthly <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "SST_with_lags.csv", sep = "/"))
sst_bimonthly <- filter(sst_bimonthly, period == "annual")

sst_combined <- bind_rows(sst_long_lagged, sst_bimonthly)

###__Pair cpr data with quarterly anomalies  ####
cpr_sst <- cpr_long %>%
  left_join(sst_combined, by = c("year", "period"))


# #Save it
# write_csv(cpr_sst,
#           str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "anomalies_w_quarterlysst.csv", sep = "/"),
#           col_names = TRUE)


####___________________________####
####  Regression Plots  ####
cpr_sst <- mutate(cpr_sst, period = str_to_title(period))

####  1. Full TS  ####

####__Temperature CPR Same Q  ####
temp_anom_plots <- cpr_sst %>% 
  split(.$taxa) %>% 
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
      facet_grid(period ~ taxa) +
      stat_poly_eq(data = .x,
                   formula = my.formula, 
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +  
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "Population Anomaly", 
           y = "Temperature Anomaly")
  }
  )


###__Cpr lagged one quarter  ####
lag_temp_plots <- cpr_sst %>% 
  split(.$taxa) %>% 
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
      facet_grid(period ~ taxa) +
      stat_poly_eq(data = .x,
                   formula = my.formula, 
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +  
      theme_bw() +
      labs(x = "Population Anomaly", 
           y = "Previous Quarter's Temperature Anomaly",
           caption = "Years: 1981 - 2017") +
      theme(legend.position = "none",
            plot.caption    = element_text(color = "gray60", size = 8) )
    
  }
  )


#Put them together to export them
twin_plots <- map2(temp_anom_plots, lag_temp_plots, function(x,y) {
  new_plot <- x + y
})

#Check them out
twin_plots$calanus_finmarchicus_v_vi

####__Save Plots ####
# twin_plots %>% imap(function(x,y) {
#   ggsave(x, filename = here::here()), device = "png")
# })




####  2. Recent Period  ####

####__Temperature CPR Same Q  ####
t_plots_2 <- cpr_sst %>% 
  filter(year >= 2004) %>% 
  split(.$taxa) %>% 
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
      facet_grid(period ~ taxa) +
      stat_poly_eq(data = .x,
                   formula = my.formula, 
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +  
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "Population Anomaly", 
           y = "Temperature Anomaly")
  }
  )


###__Cpr lagged one quarter  ####
lag_plots_2 <- cpr_sst %>% 
  filter(year >= 2004) %>% 
  split(.$taxa) %>% 
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
      facet_grid(period ~ taxa) +
      stat_poly_eq(data = .x,
                   formula = my.formula, 
                   eq.with.lhs = "italic(hat(y))~`=`~",
                   aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                   parse = TRUE) +  
      theme_bw() +
      labs(x = "Population Anomaly", 
           y = "Previous Quarter's Temperature Anomaly",
           caption = "Years 2004 - 2017") +
      theme(legend.position = "none",
            plot.caption    = element_text(color = "gray60", size = 8) )
  }
  )


#Put them together to export them
recent_plots <- map2(t_plots_2, lag_plots_2, function(x,y) {
  new_plot <- x + y
})

#Check them out
recent_plots$calanus_finmarchicus_v_vi

####__Save Plots ####
# recent_plots %>% imap(function(x,y) {
#   ggsave(x, filename = here::here(), device = "png")
# })