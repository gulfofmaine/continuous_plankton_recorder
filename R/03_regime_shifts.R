####  Regime Shifts  
####  11/12/2019
#### Adam A. Kemberling
#source: http://oceantippingpoints.org/portal/analytical-tools-and-resources


####  Load Packages  ####
library(strucchange)
library(changepoint)
library(cpm)
library(bcp)
library(zoo)
library(tidyverse)
library(here)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

#Set ggplot theme
theme_set(theme_classic())

####  Data  ####
cpr_long <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "cpr_allspecies_long.csv", sep = "/"))
cpr_wide <- cpr_long %>%
  filter(is.na(anomaly) == FALSE) %>% 
  pivot_wider(names_from = species, 
              values_from = anomaly) 


#Pull only annual measurements
annual_anoms <- cpr_long %>% 
  filter(period == "annual") 


####  Approach  ####
#The following steps follow the approaches laid out in:
#Bestelmeyer et al. 2011

####__#####################################
####  1. Temporal Changes In Response  ####
anniual_anoms %>% 
  ggplot(aes(year, anomaly)) +
    geom_point(color = gmri_cols("dark gray")) +
    geom_smooth(method = "loess", color = gmri_cols("gmri blue")) +
    facet_wrap(~species) +
    labs(x = NULL, y = "Population Anomaly")


####__#####################################
####  2. Locate & Test Break Points  ####

#NA values were filled in by sampling from a normal distribution with the mean and variance of points around the gaps
#Ex. N(mean[z1974, z1977], SD[z1974, z1977])

# Function to fill in 1975-1976 using random draws from a normal distribution
# Distribution mean and sd is from the values adjacent to the gap
set.seed(123)
fill_gaps <- function(df) {
  
  low_end  <- as.double(df[df$year == 1974, "anomaly"])
  high_end <- as.double(df[df$year == 1977, "anomaly"])
  gap_ends <- c(low_end, high_end)
  
  year_fills <- rnorm(
    n    = 2, 
    mean = mean(gap_ends), 
    sd   = sd(gap_ends)
  )
  
  #Fill gaps with random
  df[df$year == 1975, "anomaly"] <- year_fills[1]
  df[df$year == 1976, "anomaly"] <- year_fills[2]
  
  return(df)
}

#Adding in randomly generated data
annual_filled <- annual_anoms %>% 
  split(.$species) %>% 
  map(~fill_gaps(.x))


#Cumulative sum plot of raw values
annual_filled %>% 
  map(~.x %>% mutate(`Running Sum` = cumsum(anomaly))) %>% 
  bind_rows() %>% 
  ggplot(aes(year, `Running Sum`)) +
  geom_hline(yintercept = 0, linetype = 2, color = gmri_cols("orange"), alpha = 0.3) +
  geom_line(color = gmri_cols("gmri blue"), group = 1) +
  labs(x = NULL, caption = "Running sum of population anomalies, i.e. deviance from predicted mean from spline.") +
  facet_wrap(~species)


#Plotting cumulative sum of lagged differences
runsum_plots <- annual_filled %>% 
  map(~diff(.x$anomaly)) %>% 
  imap(function(.x, .y){
    
    ## This just plots the points to make sure they are detrended properly
    #plot(.x, main = str_c("Differences of ", .y, " anomalies")) 
    
    #Cumsulative Sum Plot of lagged differences
    cs_df <- tibble(x = c(1:length(.x)),
                    `Running Sum` = cumsum(.x))
    
    ggplot(cs_df, aes(x, `Running Sum`)) +
      geom_area(fill = gmri_cols("gmri blue"), alpha = 0.3) +
      geom_hline(yintercept = 0, linetype = 2, color = gmri_cols("orange"), alpha = 0.3) +
      geom_line(color = gmri_cols("gmri blue"), group = 1) +
      geom_point(color = gmri_cols("gmri blue"),) +
      labs(title = str_to_title(.y),
           x = NULL,
           caption = "Running sum of one-year lag differences in population anomalies")
    
    
  })

# Single species can be pulled out this way
runsum_plots$metridia



#Using the strucchange package to locate breakpoints:

#CUSUM process
efp_list <- annual_filled %>% 
  imap(function(.x, .y) {
    
    efp_df <- tibble(
      species     = .y,
      year        = .x$year[2:nrow(.x)],
      year_diffs  = diff(.x$anomaly),
      rolling_sum = cumsum(year_diffs) #not sure if we need to do this or if efp does it
    )
    
    #Perform ordinary least-squared CUSUM estimation
    efp_obj <- strucchange::efp(year_diffs ~ year, type = "OLS-CUSUM", data = efp_df)
    #efp_obj <- strucchange::efp(rolling_sum ~ year, type = "OLS-CUSUM", data = efp_df)
    })

summary(efp_list[["calanus"]])


#Plotting the CUSUM plots
imap(efp_list, ~plot(.x, main = str_c("OLS-based CUSUM test \n", .y)))

#Getting breakpoints - double check formula
bp_list <- annual_filled %>% 
  imap(function(.x, .y) {
    
    .x$lag1 <- lag(.x$anomaly, n = 1)
    
    # ###
    # #Attempt 1: 1 year lag ~ year before
    # bp_df <- tibble(
    #   species = .y,
    #   year    = .x$year[2:nrow(.x)],
    #   anomaly = .x$anomaly[2:nrow(.x)],
    #   lag1    = .x$lag1[2:nrow(.x)]
    # )
    # 
    # breakpoints(lag1 ~ anomaly, data = bp_df, h = 0.1)
    # ###
    
    ###
    #Attempt 2: Match efp formula
    bp_df <- tibble(
      species     = .y,
      year        = .x$year[2:nrow(.x)],
      year_diffs  = diff(.x$anomaly),
      rolling_sum = cumsum(year_diffs) #not sure if we need to do this or if efp does it
    )
    breakpoints(year_diffs ~ year, data = bp_df, h = 0.1)
    ###
    
  })

plot(bp_list[["calanus"]])
summary(bp_list[["calanus"]])

?strucchange::breakpoints()

# F test for number of breakpoints - 2011 paper uses Chow test






####__#####################################
####  3. Testing Unimodality of Response  ####
annual_anoms %>% 
  ggplot() +
  geom_histogram(aes(anomaly, y = ..density..), fill = "gray50") +
  stat_density(aes(anomaly), 
               geom = "line",
               color = gmri_cols("gmri blue")) +
  facet_wrap(~species) +
  labs(x = "Population Anomaly", y = "Density")


####__#####################################
####  4. Calculate Temporal Variance  ####


#rollapply function from zoo package, done on th difference values from diff()

####__#####################################
####  5. Assess Driver/Response Relationship around Breakpoints  ####
