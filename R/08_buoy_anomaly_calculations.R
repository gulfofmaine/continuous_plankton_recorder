# Buoy Temp and Salinity Anomaly Calculations
# 12/10/2019

####  Packages  ####
library(ggpmisc)
library(tidyverse)
library(here)
library(patchwork)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####  Data  ####
#Period aggregations
buoys_aggregated <- read.csv(str_c(cpr_boxpath, "data/processed_data/buoys_aggregated.csv", sep = "/")) %>% 
  mutate(reading_depth = factor(
    reading_depth, levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters")
    ))

#Daily Reads
buoys <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoys_daily.csv", sep = "/"),
                  col_types = cols())

buoys <- buoys %>% 
  mutate(
    #Create julian days for quarter assignment
    julian = lubridate::yday(Date), 
    period = case_when(
      julian <= 91                       ~ "Q1",
      between(julian, left = 92, 182)    ~ "Q2",
      between(julian, left = 183, 273)   ~ "Q3",
      julian > 273                       ~ "Q4"
    ),
    reading_depth = factor(
    reading_depth, levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters")
))

#GOALS:
# For each qaurter, at each buoy, for 1m and 50m sensors
# Get a handle on daily/monthly readings of temperature and salinity

head(buoys)


####____________________________####
####  Monthly Data Exploration  ####
theme_set(theme_bw())

#Pull Select Depths
our_depths <- c("1 meter", "50 meters", "150 meters")


# linear regression of each quarter over time
buoys %>% 
  filter(reading_depth %in% our_depths,
         buoy_id == "Buoy_M") %>% 
  ggplot(aes(Date, temp, color = period)) +
    geom_line(color = "gray50") +
    geom_smooth(formula = y ~ x,
                method = "lm",
                se = FALSE) +
    facet_wrap(~ reading_depth, ncol = 1) +
    labs(x = NULL, y = "Temp (C)", title = "Tester")



####  Temerature Splines  ####
temp_plots <- buoys %>% 
  filter(reading_depth %in% our_depths) %>% 
  split(.$buoy_id) %>% 
  imap(~
      .x %>% ggplot(aes(Date, temp)) +
         geom_line(color = "gray50") +
        # #Quarterly averages
        #  geom_smooth(formula = y ~ x,
        #              method = "lm",
        #              se = FALSE) +
        # #GAM splines
        geom_smooth(formula = y ~ s(x, bs = "cs", k = 80),
                    method = "gam",
                    size = 0.5) +
        # # #lm Splines
        # geom_smooth(formula = y ~ splines::bs(x, 40), 
        #             method = "lm",
        #             size = 0.5) +
        facet_wrap(~ reading_depth, ncol = 1) +
        labs(x = NULL, y = "Temp (C)", title = .y)
  )

#single plot
temp_plots$Buoy_M + temp_plots$Buoy_N

####  Salinity Splines  ####
sal_plots <- buoys %>% 
  filter(reading_depth %in% our_depths) %>% 
  split(.$buoy_id) %>% 
  imap(~
         
      .x %>% ggplot(aes(Date, sal)) +
         geom_line(color = "gray50") +
         #GAM splines
         geom_smooth(formula = y ~ s(x, bs = "cs", k = 80),
                     method = "gam",
                     size = 0.5) +
         # # #lm Splines
         # geom_smooth(formula = y ~ splines::bs(x, 40), 
         #             method = "lm",
         #             size = 0.5) +
         facet_wrap(~ reading_depth, ncol = 1) +
         labs(x = NULL, y = "Salinity", title = .y)
  )

#Single plot
sal_plots$Buoy_M

####____________________________####

####  Calculate Daily Anomalies  ####
daily_anoms <- buoys %>% 
  group_by(buoy_id, reading_depth, julian) %>% 
  summarise(avg_temp = mean(temp, na.rm = T),
            temp_sd  = sd(temp, na.rm = T),
            avg_sal = mean(sal, na.rm = T),
            sal_sd = sd(sal, na.rm = T)) %>% 
  right_join(buoys, by = c("buoy_id", "reading_depth", "julian"))  %>% 
  select(buoy_id, reading_depth, Date, julian, period, everything()) %>% 
  mutate(temp_anom = (temp - avg_temp) / temp_sd,
         sal_anom  = (sal - avg_sal) / sal_sd)



####  Plotting Daily Anomaly Values  ####
temp_anom_plots <- daily_anoms %>% 
  filter(reading_depth %in% our_depths) %>% 
  split(.$buoy_id) %>% 
  imap(~
      .x %>% ggplot(aes(Date, temp_anom)) +
         geom_hline(yintercept = 0,linetype = 2, color = "darkred", alpha = 0.8) +
         geom_line(color = "gray50") +
         # #GAM splines
         geom_smooth(formula = y ~ s(x, bs = "cs", k = 80),
                     method = "gam",
                     size = 0.5) +
         # # #lm Splines
         # geom_smooth(formula = y ~ splines::bs(x, 40), 
         #             method = "lm",
         #             size = 0.5) +
         facet_wrap(~ reading_depth, ncol = 1) +
         labs(x = NULL, y = "Daily Temperature Anomalies (sd)", title = .y)
  )

#single plot
temp_anom_plots$Buoy_M + temp_anom_plots$Buoy_N

####____________________________####

#####  Calculate  Aggregate Values  ####
yearly_means <- daily_anoms %>% 
  mutate(year = lubridate::year(Date),
         year = factor(year)) %>% 
  group_by(buoy_id, year, reading_depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_sal = mean(sal, na.rm = T),
            temp_anom = mean(temp_anom, na.rm = T),
            sal_anom = mean(sal_anom, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(period = "Annual")

#Quarters are 91 julian day increments
quarterly_means <- daily_anoms %>% 
  mutate(
    year = lubridate::year(Date),
    year = factor(year),
    julian = lubridate::yday(Date), 
    period = case_when(
      julian <= 91                       ~ "Q1",
      between(julian, left = 92, 182)    ~ "Q2",
      between(julian, left = 183, 273)   ~ "Q3",
      julian > 273                       ~ "Q4"
    )) %>% 
  group_by(buoy_id, year, period, reading_depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_sal = mean(sal, na.rm = T),
            temp_anom = mean(temp_anom, na.rm = T),
            sal_anom = mean(sal_anom, na.rm = T)) %>% 
  ungroup()

bi_monthly_means <- daily_anoms %>% 
  mutate(year = lubridate::year(Date),
         year = factor(year),
         month_col = lubridate::month(Date),
         month_col = factor(month_col),
         period = case_when(
           month_col %in% c(1:2)     ~ "P1",
           month_col %in% c(3:4)     ~ "P2",
           month_col %in% c(5:6)     ~ "P3",
           month_col %in% c(7:8)     ~ "P4",
           month_col %in% c(9:10)    ~ "P5",
           month_col %in% c(11:12)   ~ "P6",
         )) %>% 
  group_by(buoy_id, year, period, reading_depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = T),
            mean_sal = mean(sal, na.rm = T),
            temp_anom = mean(temp_anom, na.rm = T),
            sal_anom = mean(sal_anom, na.rm = T)) %>% 
  ungroup()

#Put them back together
anoms_dataset <- full_join(yearly_means, quarterly_means) %>% 
  full_join(bi_monthly_means) %>% 
  mutate(reading_depth = as.character(reading_depth),
         reading_depth = if_else(buoy_id == "Buoy_M" & reading_depth == "150 meters", "180 meters", reading_depth),
         reading_depth = factor(reading_depth, levels = c("1 meter", "20 meters", "50 meters", "100 meters",
                                                          "150 meters", "180 meters")))



####  Export Out  ####
write_csv(anoms_dataset, 
          path = str_c(cpr_boxpath, "data/processed_data/buoy_anomalies.csv", sep = "/"), 
          col_names = TRUE)



#Test Plot
anoms_dataset %>% 
  filter(period == "Q1", 
         reading_depth == "1 meter") %>% 
  ggplot(aes(as.numeric(year), temp_anom)) +
    geom_line() +
    facet_wrap(~buoy_id)
