####  Helper Functions  ####

#Set ggplot theme
ggplot2::theme_set(ggplot2::theme_classic())

#' @title Locate Data Path on Box
#' @description Safely navigate to box path within GMRI in lieu of here function
#'
#' @param box_project_name 
#'
#' @return box_path project directory string for box project data
#' @export
#'
#' @examples
find_box_data <- function(box_project_name) {
  box_project <- as.character(box_project_name)
  box_path <- str_c("/Users/akemberling/Box/Adam Kemberling/Box_Projects/", paste(box_project))
  return(box_path)
}

cpr_boxpath <- find_box_data("continuous_plankton_recorder")
ccel_boxpath <- "/Users/akemberling/Box/Climate Change Ecology Lab"




#' Floor Decade
#'
#' @param year_vector Vector of integer years
#' @param return_class String indicating output type for the vector, factor or numeric
#'
#' @return decade_vector returned vector of years rounded down to their decade
#' @export
#'
#' @examples
floor_decade <- function(year_vector, return_class = "factor"){ 
  
  if(class(year_vector) == "numeric") {
    decade_vector <- year_vector - year_vector %% 10
  }
  
  if(class(year_vector) %in% c("factor", "character")) {
    year_vector <- as.numeric(as.character(year_vector))
    decade_vector <- year_vector - year_vector %% 10
  }
  
  if(return_class == "factor") {
    decade_vector <- factor(decade_vector)
    
  }
  
  return(decade_vector)
}

#' @title Apply Principal Component Loadings to Data Matrix
#'
#' @param pca_load The data we wish do apply loadings to. Must have same column dimensions as the PCA dataset.
#' @param pca_rotations Roatations obtained from the PCA object (results from porcomp())
#' @param mode_num The Principal component loading to apply as an integer
#'
#' @return pca_adjusted dataframe containing original values of pca_load adjusted by the selected PCA loading's weights
#' @export
#'
#' @examples
apply_pca_load <- function(pca_load, pca_rotations, mode_num = 1) {
  
  #Pull PCA rotations/loadings
  rotations <- as.data.frame(pca_rotations)
  rotations_t <- t(rotations)
  
  #Principal component whose weights we want to apply
  mode_num <- as.integer(mode_num)
  
  #Copy of the initial values to apply them to
  pca_adjusted <- pca_load[, 2:ncol(pca_load)]
  
  #Multiply the columns by their PCA weights
  for (i in 1:ncol(rotations_t)) {
    pca_adjusted[, i] <- pca_adjusted[, i] * rotations_t[mode_num, i]
    
  }
  
  return(pca_adjusted)
}


 
#' @title Extract Percent Deviance Explained
#' @description Extract % variance explained from PCA object. Useful when
#' plotting PCA modes in situations where you want to show deviance explained
#' and have it update with new pca.
#'
#' @param pca_sdev 
#'
#' @return
#' @export
#'
#' @examples
pull_deviance <- function(pca_sdev) {
  
  eigs <- pca_sdev ^ 2
  
  deviance_df <- rbind(
    SD = sqrt(eigs),
    Proportion = eigs/sum(eigs),
    Cumulative = cumsum(eigs)/sum(eigs))
  
  pca_dev_out <- data.frame(
    "PC1" = str_c(as.character(round(deviance_df[2,1] * 100, 2)), "% of Variance"),
    "PC2" = str_c(as.character(round(deviance_df[2,2] * 100, 2)), "% of Variance"))
  
  return(pca_dev_out)
  
}


####  Corrplot Functions  ####

#' @title Pull Time Periods for Corrplots
#'
#' @param time_period Time period of interest identifying the "period" value to extract
#'
#' @return 
#' @export
#'
#' @examples
pull_period <- function(cpr_long_df = cpr_sst, time_period = annual) {
  
  plankton_ts <- cpr_long_df %>% 
    filter(period %in% c("Annual", "annual")) %>% 
    pivot_wider(names_from = taxa, values_from = anomaly) %>% 
    select(year, one_of(species_05))
  
  temp_ts <- cpr_long_df %>% 
    distinct(year, period, .keep_all = T) %>% 
    pivot_wider(names_from = period, values_from = temp_anomaly) %>% 
    select(year, one_of(time_period)) #%>%  setNames(c("year", "period"))
  
  df_out <- inner_join(plankton_ts, temp_ts, by = "year") %>% drop_na()
  return(df_out)
  
}




#' Extract upper triangle of the correlation matrix
#'
#' @param correlation_matrix Correlation matrix object created by cor()
#'
#' @return correlation_matrix Correlation matrix with NA values substituted for lower-triangle correlations
#' @export
#'
#' @examples
get_upper_tri <- function(correlation_matrix){
  correlation_matrix[lower.tri(correlation_matrix)] <- NA
  return(correlation_matrix)
}





 
#' Pull correlations and p-values for Correlogram
#'
#' @param wide_df 
#'
#' @return
#' @export
#'
#' @examples
corr_plot_setup <- function(wide_df) {
  
  # 1. Pull data used for corellation matrix
  corr_data <- wide_df %>% 
    select(-year#, -period
           )
  
  # 2. Pull the correlation matrix and melt to a dataframe
  corr_mat <- corr_data %>% cor() 
  
  # 2b. Correlation Matrix as a dataframe
  corr_out <- corr_mat %>% reshape2::melt(na.rm = TRUE)
  
  # 2c. Upper Triangle of correlation matrix
  upper_tri <- corr_mat %>% 
    get_upper_tri() %>%
    reshape2::melt() %>% 
    drop_na()
  
  # 3. do it again but pull the p-values
  p_data <- corrplot::cor.mtest(corr_mat)$p 
  
  #Assign the same names as the corr matrix
  dimnames(p_data) <- dimnames(corr_mat)
  
  #reshape to match correlation df
  p_data <- reshape2::melt(p_data, na.rm = T) %>% dplyr::rename(pval = value)
  
  
  #Put the two together
  corr_full <- inner_join(corr_out, p_data, by = c("Var1", "Var2")) %>% 
    #Format levels and labels
    mutate(Var1 = fct_relevel(Var1, sort),
           Var2 = fct_relevel(Var2, sort),
           sig_symbol = if_else(pval <= 0.05 & value > 0, "+", " "),
           sig_symbol = if_else(pval <= 0.05 & value < 0, "-", sig_symbol))
  
  return(corr_full)
}

#Not in Function
`%notin%` <- purrr::negate(`%in%`)


#' CPR Correlogram
#'
#' @param corr_dataframe 
#' @param period 
#' @param plot_style 
#' @param taxa 
#'
#' @return
#' @export
#'
#' @examples
cpr_corr_plot <- function(corr_dataframe, period = "Q1", plot_style = "tall", taxa = NULL){
  
  #Filter Var1 and Var2 to reshape plot
  
  #Taxa
  if(is.null(taxa)) {
    my_taxa <- c("calanus", "calanus1to4", "centropages", "chaetognatha",
                 "euphausiacea", "metridia", "oithona", "para_pseudocalanus",
                 "paraeucheata", "temora")
  } else{my_taxa = taxa}
  
  long_plot <- corr_dataframe %>% 
    filter(Var1 %notin% my_taxa,
           Var2 %in% my_taxa) %>% 
    mutate(Var2 = factor(Var2, levels = my_taxa))
  
  tall_plot <- corr_dataframe %>% 
    filter(Var1 %in% my_taxa,
           Var2 %notin% my_taxa) %>% 
    mutate(Var1 = factor(Var1, levels = my_taxa))
  
  if(plot_style == "tall") {
    plot_option  <- tall_plot
    leg_position <- "right"
  } else {
    plot_option  <- long_plot
    leg_position <- "bottom"
  }
  
  
  ggplot(plot_option, aes(x = Var1, y = fct_rev(Var2), fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sig_symbol), 
              color = "black", 
              size = 3) +
    scale_fill_gradient2(low = "blue", 
                         high = "red", 
                         mid = "white", 
                         midpoint = 0, 
                         limit = c(-1,1), 
                         space = "Lab", 
                         name = "Pearson\nCorrelation") +
    labs(x = NULL, 
         y = NULL, 
         title = period) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.position = leg_position,
          axis.text = element_text(color = "black")) +
    coord_fixed() 
  
  
}


####__####
####  Spline Tools  ####
####__####
#' Continuous Plankton Recorder Seasonal Spline Tool
#'
#' @param cpr_dat n_obs x 5 matrix containing year, year-day, lat, lon, and abundance from cpr data
#' @param spline_bins Integer value indicating the desired number of basis functions for the seasonal spline fit, default = 10
#' @param season_bins Integer value indicating the number of periods you wish there to be in a recurring 365 day cycle
#'
#' @return  List containing 1. the original dataframe with log transformed abundances, labeled periods, and log transformed anomalies
#' and 2. the mean, standard deviation, and sample size for each period for each year.
#' @export
#'
#' @examples
cpr_spline_fun <- function(cpr_dat = cpr_data, spline_bins = 10, season_bins = 4, study_area = "GOM") {
  
  #Check Input dimensions are correct
  if(ncol(cpr_dat) != 5) {return(print('dat format requires 5 columns: [year, jday, lat, lon, #]'))}
  
  
  
  #### Trim Data to Study Area BBox  ####
  
  area_bboxes <- tribble( ##### Area BBbox Open  ####
    ~"area",  ~"lon",  ~"lat",
    #Gulf of Maine - Historic
    "GOM",   -70.000000,	42.200000,
    "GOM",   -68.600000,	42.200000,
    "GOM",   -68.600000,	42.400000,
    "GOM",   -66.600000,	42.400000,
    "GOM",   -66.600000,	43.400000,
    "GOM",   -68.600000,	43.400000,
    "GOM",   -68.600000,	43.200000,
    "GOM",   -70.000000,	43.200000,
    "GOM",   -70.000000,	42.200000,
    #Gulf of Maine - Extended North to capture cpr route change
    "GOM_new",   -70.000000,	42.200000, 
    "GOM_new",   -66.600000,	42.200000, 
    "GOM_new",   -66.600000,	43.800000, 
    "GOM_new",   -70.000000,	43.800000, 
    "GOM_new",   -70.000000,	42.200000,
    
    "CCB",   -70.800000,	42.200000,
    "CCB",   -70.000000,	42.200000,
    "CCB",   -70.000000,	42.800000,
    "CCB",   -70.800000,	42.800000,
    "CCB",   -70.800000,	42.200000,
    #Western Gulf of Maine
    "WGOM",  -70.000000, 	42.200000,
    "WGOM",  -68.600000, 	42.200000,
    "WGOM",  -68.600000, 	43.200000,
    "WGOM",  -70.000000, 	43.200000,
    "WGOM",  -70.000000, 	42.200000,
    #Eastern Gulf of Maine
    "EGOM",  -68.600000,	42.400000,
    "EGOM",  -66.600000,	42.400000,
    "EGOM",  -66.600000,	43.400000,
    "EGOM",  -68.600000,	43.400000,
    "EGOM",  -68.600000,	42.400000,
    
    "SS",    -66.600000,	42.600000,
    "SS",    -65.400000,	42.600000,
    "SS",    -65.400000,	43.400000,
    "SS",    -66.600000,	43.400000,
    "SS",    -66.600000,	42.600000,
  ) %>% arrange(area) ##### Area BBbox Close  ####
  
  

  #Filter to the correct area
  study_area_bbox <- filter(area_bboxes, area == study_area)
  
  
  #subset data that fits
  cpr_dat <- cpr_dat %>% mutate(lon = ifelse(lon > 0, lon * -1, lon))
  
  cpr_dat <- cpr_dat %>% 
    filter(
      between(lon, min(study_area_bbox$lon), max(study_area_bbox$lon)),
      between(lat, min(study_area_bbox$lat), max(study_area_bbox$lat)))

  
  
  ####  Fit Seasonal Trend using all data  ####
  
  #Transform abundance to log abundance
  cpr_dat <- cpr_dat %>% 
    mutate(
      abundance = as.numeric(abundance),
      log_abund = log(abundance),
      #log_abund = log10(abundance),
      log_abund = ifelse(is.infinite(log_abund), 0, log_abund)
    )
  
  
  #Build spline model using mgsv::gam using cyclic penalized cubic regression spline smooth
  cc_spline_mod <- gam(log_abund ~  s(jday, bs = "cc", k = spline_bins),
                       data = cpr_dat)
  
  
  #Add Predictions back to the data
  cpr_dat$spline_pred <- predict(cc_spline_mod, cpr_dat, type = "response")
  
  
  #Calculate anomalies
  cpr_dat$anomaly <- cpr_dat$log_abund - cpr_dat$spline_pred
  
  #Calculate Anomalies relative to standard deviation measure of:
  #overall_sd <- sd(cpr_dat$spline_pred, na.rm = T)
  overall_sd <- sd(cpr_dat$log_abund, na.rm = T)
  cpr_dat$rel_anomaly <- cpr_dat$anomaly / overall_sd
  
  
  ####  Calculate Seasonal Averages  ####
  #Get Period Split Points from number of season_bins
  bin_splits <- c(seq(0,365, by = ceiling(365 / (season_bins))), 365)
  
  #Set period number in data based on desired number of splits
  period <- data.frame(
    period = rep(0, nrow(cpr_dat)),
    min_date = rep(NA, nrow(cpr_dat)),
    max_date = rep(NA, nrow(cpr_dat)))
  
  #Add period label and datebound
  for (n in 1:nrow(cpr_dat)) {
    for (i in 1:season_bins) {
      if( cpr_dat$jday[n] > bin_splits[i] & cpr_dat$jday[n] <=  bin_splits[i+1]) {
        period[n,"period"]   <- i
        period[n,"min_date"] <- bin_splits[i]
        period[n,"max_date"] <- bin_splits[i+1]
      }
    }
  }
  
  #Bind period column
  period <- period %>% 
    mutate(datebounds = str_c(min_date, "-", max_date)) %>% 
    select(-c(min_date, max_date))
  
  cpr_dat <- bind_cols(cpr_dat, period)
  
  #Get Period Means
  seasonal_summary <- cpr_dat %>% 
    group_by(year, period, datebounds) %>% 
    summarise(
      period_anom_mu = mean(anomaly, na.rm = T),
      period_anom_sd = mean(anomaly, na.rm = T),
      period_anom_std = mean(rel_anomaly, na.rm = T),
      period_anom_n = n() ) %>% 
    ungroup() %>% 
    mutate(period = as.character(period))
  
  #Get Annual Means
  annual_summary <- cpr_dat %>% 
    group_by(year) %>% 
    summarise(
      period_anom_mu = mean(anomaly, na.rm = T),
      period_anom_sd = mean(anomaly, na.rm = T),
      period_anom_std = mean(rel_anomaly, na.rm = T),
      period_anom_n = n() ) %>% 
    mutate(period = "annual",
           datebounds = "1-365")
  
  #Put the annual and period means together
  period_summaries <- bind_rows(seasonal_summary, annual_summary) %>% arrange(year, period)
  
  #Predictions out
  ts_out <- list(
    "cprdat_predicted" = cpr_dat,
    "period_summs" = period_summaries,
    "spline_model" = cc_spline_mod
  )
  return(ts_out)
  
  
} ####  End Spline Function  #### 
