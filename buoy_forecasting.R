# Research Idea 4/4/2022
# Use near-term NERACOOS buoy data at multiple depths to predict abundance/presence
# of calanus and other species from CPR data
# Can use the buoy data as-is or as a PCA
# I think data engineering around temporal lags will be informative




####  Packages  ####
library(tidyverse)
library(gmRi)
library(modeltime)
library(tsibble)




####  Data  ####

# Path to CPR data on Box
cpr_boxpath <- str_c(cs_path(box_group = "root"),
                     "Adam Kemberling/Box_Projects/continuous_plankton_recorder/")

# 1.
# Temperature anomaly data from NERACOOS Buoys (Annual, quarterly)
# Source: 07_buoy_regressions.R
buoy <- read.csv(str_c(cpr_boxpath, "data/processed_data/buoy_anomalies.csv", sep = "/")) %>% 
  mutate(reading_depth = factor(
    reading_depth, levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters")
  ))


# 2.
# Daily Buoy Readings source: 06_buoy_data_mgmt.R
buoys_daily <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoys_daily.csv", sep = "/"))



# 3.
# CPR Data w/ periods for working with buoy periods:
# Source: 09_buoy_quarterly_pca.R
read_csv(str_c(cpr_boxpath,"data", "processed_data", "cpr_allspecies_long_quarters.csv", sep = "/"),
         col_types = cols()) %>% 
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


####  Feature Engineering (timeseries)  ####

# Missing Data Imputations:
# Fill gaps in the timeseries in some robust way to enable more complete timeseries of predictors


# PCA 
# Concept: PCA's decompose the correlative relationships between many predictors into
# a handful of principal components that may ultimately better predict outcomes than their components

# Stratification State
# The presence/strength/duration of stratification throughout to GOM informs where/when mixing
# may occur and the consequential immigration/migration opportunities for plankton

# Lags
# Concept: Plankton may not respond immediately to the environment, but at some sort of lag, or 
# under some periodic oscillation. Engineering these may help better predict outcomes


# Month/season
# Categorical classifications to accompany physical environmental status
# stratification in "summer" may have different relationship than stratification in "winter"


# Climate State
# In addition to the measurements directly taken by there are larger external forces like
# NAO, GSI, etd.

# Anomaly strength/persistence 
# The idea here is to flag with categorical variables when certain events have persisted for long
# durations: ex. is_hw_5days as a binary predictor when heatwaves have lasted 5 or more days



####  Modeling  ####


