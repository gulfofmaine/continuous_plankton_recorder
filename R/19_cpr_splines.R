####  CPR Zooplankton Abundance Anomaly Splines  ####
# 1/31/2020

####  Packages  ####
library(gmRi)
library(broom)
library(splines)
library(mgcv)
library(tidyverse)

ccel_boxpath <- shared.path(os.use = "unix", group = "Climate Change Ecology Lab", folder = NULL)


####  Data  ####
cpr <- read_csv(str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "zooplankton_combined.csv", sep = "/"), 
                guess_max = 1e6, col_types = cols())


#Calanus Test Set
cal_test <- cpr %>% 
  mutate(cal_date = as.POSIXct(str_c(year, month, day, sep = "/"), format = "%Y/%m/%d"),
         jday = lubridate::yday(cal_date),
         abundance = `calanus finmarchicus i-iv`,
         log_abundance = log(abundance)) %>% 
  select(cal_date, year, month, jday, `latitude (degrees)`, `longitude (degrees)`, 
         abundance, log_abundance)

####  EDA  ####

#Annual Spline - 10 knots
ggplot(cal_test, aes(jday, abundance)) +
  geom_point(alpha = 0.2) +
  geom_smooth(formula = y ~ s(x, bs = "cc", k = 10), 
              method = "gam")

#Within year cyclic splines - 4 seasons, 5 knots
ggplot(cal_test, aes(jday,abundance)) +
  geom_point(alpha = 0.2) +
  geom_smooth(formula = y ~ s(x, bs = "cc", k = 4), 
              method = "gam")#Working with the data


####  Step 1  ####

#Set seasonal bin number
season_bins <- 4
bin_splits <- c(seq(0,365, by = ceiling(365 / (season_bins))), 365)
#names(bin_splits) <- c(1:length(bin_splits))

#Set period number in data based on desired number of splits
period <- data.frame(period = rep(0, nrow(cal_test)))
for (n in 1:nrow(cal_test)) {
  for (i in 1:season_bins) {
    if( cal_test$jday[n] > bin_splits[i] & cal_test$jday[n] <=  bin_splits[i+1]) {
      period[n,1] <- i
    }
      
  }
}

cal_test <- bind_cols(cal_test, period)

#Fit a cubic spline is fit to the seasonal cycle using desired number of seasonal bins
seasonal_spline <- gam(abundance ~  s(jday, bs = "cc",  k = length(bin_splits)),
                           data = cal_test)



#Set the knot values explicitly using the equal breaks in year-day
seasonal_spline2 <- gam(abundance ~  s(jday, bs = "cc", k = length(bin_splits)),
                       knots = list(jday = bin_splits),
                       data = cal_test)



# # seasonal spline predictions are added
# cal_test$seasonal_preds  <- predict(seasonal_spline, cal_test)
# cal_test$seasonal_preds2  <- predict(seasonal_spline2, cal_test)
# 
# plot(cal_test$seasonal_preds, cal_test$seasonal_preds2,
#      xlab = "k = 5, knots = seasonal bin splits",
#      ylab = "k = number of seasonal bins")
# 
# #Plotting it
# ggplot(cal_test, aes(jday, abundance)) +
#   geom_point(alpha = 0.1, shape = 1) +
#   geom_line(aes(jday, seasonal_preds2,  color = "with setting knots"), size = 1) +
#   geom_line(aes(jday, seasonal_preds,  color = "without setting knots"), size = 1)


#going with this one
cal_test$seasonal_preds  <- predict(seasonal_spline2, cal_test)

#The seasonal cycle is removed from the data and mean anomalies are computed for each year.
cal_test$season_removed  <- cal_test$abundance - cal_test$seasonal_preds


(period_anoms <- cal_test %>% 
  group_by(period) %>% 
  summarise(mean_anom = mean(season_removed, na.rm = T)))
(yearly_anoms <- cal_test %>% 
  group_by(year) %>% 
  summarise(mean_anom = mean(season_removed, na.rm = T)))

#Plotting it
ggplot(cal_test, aes(jday, abundance)) +
  geom_point(alpha = 0.1, shape = 1) +
  geom_line(aes(jday, seasonal_preds), color = "royalblue", size = 1)

ggplot(cal_test, aes(cal_date, abundance)) +
  geom_point(alpha = 0.1, shape = 1) +
  geom_line(aes(cal_date, seasonal_preds), color = "royalblue", size = 1)


#Two vectors define the resolution of the spline and the division of each year. 
#The resolution of the spline is determined by a 1-by-p vector splinebins (in days).  

#The spline is produced by computing the mean value of the data between splinebins(j) 
#and splinebins(j+1). If splinebins is an integer p, then the year is divided into p equal periods. 

#This value is assumed to occur on a day within this interval (the mean of thesample dates).  

#This procedure creates a set of p-1 points. The spline is fit to these points using csape. 
#The spline is subtracted from each observation to create a series of anomalies.  
#If seasonbins is not provided, then the anomalies are returned in a matrix [CPRdat(:,1)+CPRdat(:,2)/365, anomalies]
#If season bins is an integer q, then the year is divided into q equal periods and the mean of these periods within each year is produced.  

#The result will be a y-by-(q+1) matrix [year, mean in period 1, mean in period 2, ..., 
#mean in period q] Seasonbins can also be a 1-by-(q+1) vector defining the boundaries of the
#bins.

# 10/3/02--splinebins can be matlab polynomial (like output of csape).  If this
#          is the case, then that polynomial is used, rather than creating one.

#The optional outputs SD will contain the standard deviation of the estimates in
#TS and N will contain the number of samples.



#### Adapting Andy's Code  ####

# CPRdat should be an m-by-5 matrix of CPR observations: [year, jday, lat, lon, #]
cpr_dat <- cal_test %>% select(year, jday, `latitude (degrees)`, `longitude (degrees)`, abundance)


####  Define Seasonal Spline Function  ####
#' Continuous Plankton Recorder Seasonal Spline Tool
#'
#' @param cpr_dat n_obs x 5 matrix containing year, year-day, lat, lon, and abundance from cpr data
#' @param season_bins Integer value indicating the number of periods you wish there to be in a recurring 365 day cycle
#'
#' @return  ts_out
#' @export
#'
#' @examples
cpr_spline_fun <- function(cpr_dat = cpr_dat, season_bins = 4) {
  
  #Check Input dimensions are correct
  if(ncol(cpr_dat) != 5) {return(print('dat format requires 5 columns: [year, jday, lat, lon, #]'))}

  #Get Period Split Points from number of season_bins
  bin_splits <- c(seq(0,365, by = ceiling(365 / (season_bins))), 365)
  
  #Set period number in data based on desired number of splits
  period <- data.frame(period = rep(0, nrow(cal_test)))
  for (n in 1:nrow(cpr_dat)) {
    for (i in 1:season_bins) {
      if( cpr_dat$jday[n] > bin_splits[i] & cpr_dat$jday[n] <=  bin_splits[i+1]) {
        period[n,1] <- i
      }
    }
  }
  
  #Bind period column
  cpr_dat <- bind_cols(cpr_dat, period)
  
  #Build spling model using mgsv::gam using cyclic penalized cubic regression spline smooth
  #Set the knot values explicitly using the equal breaks in year-day based on season bin number
  cc_spline_mod <- gam(abundance ~  s(jday, bs = "cc", k = length(bin_splits)),
                       knots = list(jday = bin_splits),
                       data = cpr_dat)
  
  
  #Add Predictions back to the data
  cpr_dat$spline_pred <- predict(cc_spline_mod, cpr_dat)
  
  
  #Predictions out
  return(cpr_dat)
  

} ####  End Spline Function  #### 




####__####
####  Pulling Taxa   ####
taxa_cols <- names(cpr)[12:ncol(cpr)]
names(taxa_cols) <- taxa_cols

taxa_list <- map(taxa_cols, function(x){
  taxa_name <- sym(x)
  taxa_subset <- cpr %>% 
    mutate(
      cal_date = as.POSIXct(str_c(year, month, day, sep = "/"), format = "%Y/%m/%d"),
      jday = lubridate::yday(cal_date)
      ) %>% 
    select(year, jday, `latitude (degrees)`, `longitude (degrees)`, abundance = !!taxa_name)
    
})


#Find those pesky NA taxa
map(taxa_list, function(x){
  sum(is.na(x$abundance))
})


#### Abundance Anomalies for All Taxa  ####
anomaly_list <- map(taxa_list, cpr_spline_fun, 4)
