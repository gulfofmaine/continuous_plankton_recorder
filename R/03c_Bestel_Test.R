#### Mirroring Bestelmeyer Code
####  11/14/2019


#### Objective:
#Mimic methods used in bestelmeyer et. al 2011 then transfer his methods to 03_regime_shifts.R file

##_############################################################
## Required libraries  ####
##_############################################################

library(plotrix)
library(strucchange)
library(tidyverse)
library(here)


##_############################################################
## Load Data  ####
##_############################################################

#All species, subset to only annual data
mydata <- read_csv(here::here("data", "processed_data", "cpr_allspecies_long.csv")) %>% 
  filter(period == "annual")

#breakout into list of df's for each species
species_l <- mydata %>% 
	split(.$species) 

##_############################################################
## Standardize data (sd units), and interpolate over NAs if necessary  ####
## Fit a loess curve to visualize trend if any
## Create time-series object for later use
##_############################################################

#Standardization done during pre-processing

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
annual_filled <- species_l %>% 
  map(~fill_gaps(.x))


#Make each a ts object
myfrequency <- 1 #frequency of time (1 = annual, etc.)

#Code for one species
#interp.data <- ts(interp.data, start=mystart, frequency = myfrequency)

interp_l <- annual_filled %>%
 map(function(x) {
 	mystart     <- min(x$year)
 	myfrequency <- myfrequency

 	ts_out <- ts(x, start = mystart, frequency = myfrequency)
 	return(ts_out[, c("year", "anomaly")])
 })


##_############################################################
## Change-point analysis (strucchange)  ####
##_############################################################


####  timeseries detrending with diff()
#Detrend with diff - then use efp to fit time-series model
diff_detrend_l <- interp_l %>%
	map(function(x){
		y_diff <- diff(x)
		return(y_diff)
	})


# efp
diff_detrend_l %>%
	imap(function(x, y)	{
		y_diff <- x
        #fit basic model that fits constant to the ts data
		ocus_y_detrend <- efp(y_diff ~ 1, type = "OLS-CUSUM") 
		plot(ocus_y_detrend, main = str_c(y, ": diff() approach", "\n OLS-based CUSUM test"),
		     xlim = c(1961, 2017))
		return(ocus_y_detrend)
	})

####  Unfinished Breakpoint Testing  ####
# Fstats gives a sequence of F statistics
# compute and plot

diff_detrend_l %>% 
  map(function(x) {
      fstats <- Fstats(x ~ 1)
      plot(fstats#, main = str_c(y, ": diff() approach", "\n OLS-based CUSUM test")
           )
    })

fs.y.detrend <- Fstats(y.detrend~1)
plot(fs.y.detrend)

# breakpoint estimate for the F-statistic
# this will be slow for very long (thousands of points) time series
breakpoints(fs.y.detrend)

# this computes all possible breakpoints
# this will be slow for very long (thousands of points) time series

bp.y.detrend <- breakpoints(y.detrend~1)

# model selection (n breakpoints)using minimum of BIC or RSS
plot(bp.y.detrend)

# this identifies one breakpoint
bp1.y.detrend <- breakpoints(bp.y.detrend, breaks=1) 

# fit a null model with no breakpoint
fm0.y.detrend <- lm(y.detrend ~ 1)

# use the breakpoint for coefficients on either side 
y.detrend.fac <- breakfactor(bp1.y.detrend, breaks=1) 

# fits alternative model with breakpoints in y.detrend.fac
fm1.y.detrend <- lm(y.detrend ~ y.detrend.fac -1, )

## this process could be repeated for multiple breakpoints...

# return coefficents
coef(fm0.y.detrend)
coef(fm1.y.detrend)

# plot the detrended series then add the predicted values from strucchange models
plot(y.detrend) 
lines(ts(predict(fm0.y.detrend), start=mystart, frequency=myfrequency), col="red", lty=3)
lines(ts(predict(fm1.y.detrend), start=mystart, frequency=myfrequency), col="blue", lty=1)



###_#####################################
#### Timeseries detrend with loess  ####
ls_detrend_l <- annual_filled %>%
	map2(interp_l, function(original_df, ts_object){

		myresponse_loess <- loess(anomaly ~ year, data = original_df)
		y_detrend <- ts_object - predict(myresponse_loess)
		return(y_detrend)
	})

# efp
ls_detrend_l %>%
	imap(function(x, y){
		y_detrend <- x

		# fit basic model that fits constant to the ts data
		ocus_y_detrend <- efp(y_detrend ~ 1, type = "OLS-CUSUM") 
		plot(ocus_y_detrend, main = str_c(y, ": loess detrend approach", "\n OLS-based CUSUM test"),
		     xlim = c(1961,2020))
		return(ocus_y_detrend)

	})




























