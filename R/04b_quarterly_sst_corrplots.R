#### CPR Dataset - Principal Component Analysis w/ SST
#### Adam A. Kemberling
#### 12/17/2019

####  Packages  ####
library(ggbiplot)
library(patchwork)
library(tidyverse)
library(corrplot)
library(here)


####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####  Load Data  ####
# CPR Dataset with quarterly anomalies and SST with a one-period lag
# souce: 02b_quarterly_cpr_sst_regressions
cpr_sst <- read_csv(str_c(cpr_boxpath, "data/processed_data/quarterly_cpr_sst.csv", sep = "/"), 
                    col_names = TRUE,
                    col_types = cols())

#CPR Dataset with annual anomalies
species_periods_long <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "cpr_with_SSTlags.csv", sep = "/"),
                                 col_types = cols()) %>%  
  mutate(period = case_when(
    period == "annual" ~ "Annual",
    period == "jf" ~ "January - February",
    period == "ma" ~ "March - April",
    period == "mj" ~ "May - June",
    period == "ja" ~ "July - August",
    period == "so" ~ "September - October",
    period == "nd" ~ "November - December"),
    period = factor(period, levels = c("Annual", "January - February",
                                       "March - April", "May - June", 
                                       "July - August", "September - October", 
                                       "November - December")))


#Target species
species_05 <- c("calanus", "centropages", "oithona", "para_pseudocalanus",
                "metridia", "calanus1to4", "euphausiacea")

#Correlation vars
corr_vars <- c("calanus", "centropages", "oithona", "para_pseudocalanus",
               "metridia", "calanus1to4", "euphausiacea", "temp_anomaly")

#Color Scale
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

########



####  1. SST Quarterly Correlations  ####



#This will be tricky because we want annual plankton anomalies, but seasonal temperatures
pull_period <- function(time_period = NULL) {
  
  plankton_ts <- species_periods_long %>% 
    filter(period == "Annual") %>% 
    pivot_wider(names_from = species, values_from = anomaly) %>% 
    select(year, one_of(species_05))
  
  temp_ts <- cpr_sst %>% 
    distinct(year, period, .keep_all = T) %>% 
    pivot_wider(names_from = period, values_from = temp_anomaly) %>% 
    #filter(period == time_period) %>% 
    select(year, one_of(time_period))
  
  df_out <- inner_join(plankton_ts, temp_ts, by = "year") %>% drop_na()
  return(df_out)
  
}

#Make list of the data groups
period_df_list <- list(
  "Q1"  = pull_period(time_period = "Q1"),
  "Q2"  = pull_period(time_period = "Q2"),
  "Q3"  = pull_period(time_period = "Q3"),
  "Q4"  = pull_period(time_period = "Q4")
)


#Plotting Function
plot_corr <- function(df, id) {
  
  #prep dataframe by removing the year column
  df <- df %>% select(-year) %>% cor()
  
  #save name
  save_name <- str_c(here::here("R/presentations/corrplots/quarterly"), "/", id, ".png")
  
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

png(str_c(here::here("R/presentations/corrplots/quarterly/all_seasons.png")), width = 800, height = 600, res = 100)
corrplot::corrplot(temp_period_corrs, method="color", col=col(200),  
                   # hide correlation coefficient on the principal diagonal
                   #title = "Seasonal Temperature Anomalies - Species Correlations \n 1982-2018", 
                   addCoef.col = "black", # Add coefficient of correlation
                   #Adjust Margin
                   mar=c(0,0,2,2) 
)
dev.off()

####__####

####  Corrplot with ggplot  ####

#Pull individual periods and prep for correlation matrix
Q1 <- period_df_list[["Q1"]] %>% drop_na()
Q2 <- period_df_list[["Q2"]] %>% drop_na()
Q3 <- period_df_list[["Q3"]] %>% drop_na()
Q4 <- period_df_list[["Q4"]] %>% drop_na()

####  Corrplot Functions  

#Pull correlations with p-values
corr_plot_setup <- function(wide_df) {
  
  # 1. Pull data used for corellation matrix
  corr_data <- wide_df %>% 
    select(-year) 
  
  colnames(corr_data)[8] <- "temp_anomaly"
  
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
  p_data <- reshape2::melt(p_data, na.rm = T) %>% rename(pval = value)
  
  
  #Put the two together
  corr_full <- inner_join(corr_out, p_data, by = c("Var1", "Var2")) %>% 
    #Format levels and labels
    mutate(Var1 = fct_relevel(Var1, sort),
           Var2 = fct_relevel(Var2, sort),
           sig_symbol = if_else(pval <= 0.05 & value > 0, "+", " "),
           sig_symbol = if_else(pval <= 0.05 & value < 0, "-", sig_symbol))
  
  return(corr_full)
}

# data for corrplots
Q1_corrs <- corr_plot_setup(Q1)
Q2_corrs <- corr_plot_setup(Q2)
Q3_corrs <- corr_plot_setup(Q3)
Q4_corrs <- corr_plot_setup(Q4)


# Function for custom corr plot
cpr_corr_plot <- function(corr_dataframe, period = "Q1", plot_style = "tall"){
  
  #Filter Var1 and Var2 to reshape plot
  
  #Taxa
  my_taxa <- c("calanus", "calanus1to4", "centropages", "chaetognatha",
               "euphausiacea", "metridia", "oithona", "para_pseudocalanus",
               "paraeucheata", "temora")
  
  long_plot <- corr_dataframe %>% 
    filter(Var1 %notin% my_taxa,
           Var2 %in% my_taxa)
  
  tall_plot <- corr_dataframe %>% 
    filter(Var1 %in% my_taxa,
           Var2 %notin% my_taxa)
  
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
                         name="Pearson\nCorrelation") +
    labs(x = NULL, 
         y = NULL, 
         title = period) +
    theme(#axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      plot.caption = element_text(size = 6, color = "gray50"),
      legend.position = leg_position) +
    coord_fixed()
  
  
}


####__####

####  Corrplots  ####
q1_t <- cpr_corr_plot(Q1_corrs, period = "Q1", plot_style = "wide")
q2_t <- cpr_corr_plot(Q2_corrs, period = "Q2", plot_style = "wide") + theme(axis.text.y = element_blank())
q3_t <- cpr_corr_plot(Q3_corrs, period = "Q3", plot_style = "wide") + theme(axis.text.y = element_blank())
q4_t <- cpr_corr_plot(Q4_corrs, period = "Q4", plot_style = "wide") + theme(axis.text.y = element_blank())

#Patch them together
quarterly_corrplot <- q1_t | q2_t | q3_t | q4_t
quarterly_corrplot <- quarterly_corrplot & theme(legend.position = "none")
quarterly_corrplot <- quarterly_corrplot + labs(caption = "Correlations between quarterly mean SST and simulataneous abundance anomalies for focal taxa from CPR data.")
quarterly_corrplot

#Export ggplot version
ggsave(quarterly_corrplot, 
       filename =  here::here("R", "presentations", "sst_plots", "sst_quarterly_corrplot.png"), 
       device = "png")



####__####

####  2. Lagged SST Quarterly Correlations  ####


#This will be tricky because we want annual plankton anomalies, but seasonal temperatures
pull_lag_period <- function(time_period = NULL) {
  
  plankton_ts <- species_periods_long %>% 
    filter(period == "Annual") %>% 
    pivot_wider(names_from = species, values_from = anomaly) %>% 
    select(year, one_of(species_05))
  
  temp_ts <- cpr_sst %>% 
    distinct(year, period, .keep_all = T) %>% 
    pivot_wider(names_from = period, values_from = lag_temp) %>% 
    select(year, one_of(time_period))
  
  df_out <- inner_join(plankton_ts, temp_ts, by = "year") %>% drop_na()
  return(df_out)
  
}

#Make list of the data groups
lagged_df_list <- list(
  "Q1"  = pull_lag_period(time_period = "Q1"),
  "Q2"  = pull_lag_period(time_period = "Q2"),
  "Q3"  = pull_lag_period(time_period = "Q3"),
  "Q4"  = pull_lag_period(time_period = "Q4")
)

lag_period_corrs <- imap(lagged_df_list, pull_temp_corr) %>% 
  bind_rows() %>% 
  as.matrix()
rownames(lag_period_corrs) <- c(species_05, "temp_anomaly")
lag_period_corrs <- lag_period_corrs[1:7,] #Drop temperature row

#Rename columns to show lag structure
colnames(lag_period_corrs) <- c("Q4 SST & Q1 CPR", "Q1 SST & Q2 CPR", "Q2 SST & Q3 CPR", "Q3 SST & Q4 CPR")

png(str_c(here::here("R/presentations/corrplots/quarterly/all_seasons_lagged.png")), width = 800, height = 700, res = 100)
corrplot::corrplot(lag_period_corrs, method="color", col=col(200),  
                   # hide correlation coefficient on the principal diagonal
                   #title = "Seasonal Temperature Anomalies - Species Correlations \n 1982-2018", 
                   addCoef.col = "black", # Add coefficient of correlation
                   #Adjust Margin
                   mar=c(0,0,2,2) 
)
dev.off()





####__####

####  Corrplot with ggplot  ####

#Pull individual periods and prep for correlation matrix
Q1_l <- lagged_df_list[["Q1"]] %>% drop_na()
Q2_l <- lagged_df_list[["Q2"]] %>% drop_na()
Q3_l <- lagged_df_list[["Q3"]] %>% drop_na()
Q4_l <- lagged_df_list[["Q4"]] %>% drop_na()

# Data for lagged corrplots
Q1_corrs_l <- corr_plot_setup(Q1_l) %>% 
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2),
         Var1 = ifelse(Var1 == "temp_anomaly", "Q4-SST", Var1),
         Var2 = ifelse(Var2 == "temp_anomaly", "Q4-SST", Var2))
Q2_corrs_l <- corr_plot_setup(Q2_l) %>% 
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2),
         Var1 = ifelse(Var1 == "temp_anomaly", "Q1-SST", Var1),
         Var2 = ifelse(Var2 == "temp_anomaly", "Q1-SST", Var2))
Q3_corrs_l <- corr_plot_setup(Q3_l) %>% 
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2),
         Var1 = ifelse(Var1 == "temp_anomaly", "Q2-SST", Var1),
         Var2 = ifelse(Var2 == "temp_anomaly", "Q2-SST", Var2))
Q4_corrs_l <- corr_plot_setup(Q4_l) %>% 
  mutate(Var1 = as.character(Var1),
         Var2 = as.character(Var2),
         Var1 = ifelse(Var1 == "temp_anomaly", "Q3-SST", Var1),
         Var2 = ifelse(Var2 == "temp_anomaly", "Q3-SST", Var2))


# Function for custom lagged temp corr plot
#Adds labels for claarity on what SST goes with CPR Data
lagged_corr_plot <- function(corr_dataframe, cpr_period = "Q1", plot_style = "tall"){
  
  #Filter Var1 and Var2 to reshape plot
  
  #Taxa
  my_taxa <- c("calanus", "calanus1to4", "centropages", "chaetognatha",
               "euphausiacea", "metridia", "oithona", "para_pseudocalanus",
               "paraeucheata", "temora")
  
  long_plot <- corr_dataframe %>% 
    filter(Var1 %notin% my_taxa,
           Var2 %in% my_taxa)
  
  tall_plot <- corr_dataframe %>% 
    filter(Var1 %in% my_taxa,
           Var2 %notin% my_taxa)
  
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
                         name="Pearson\nCorrelation") +
    labs(x = NULL, 
         y = NULL, 
         title = cpr_period) +
    theme(
      #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
      #axis.ticks.x = element_blank(),
      #axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      plot.caption = element_text(size = 6, color = "gray50"),
      legend.position = leg_position) +
    coord_fixed()
  
  
}

####  Lagged Corrplots  ####
q1_l <- lagged_corr_plot(Q1_corrs_l, cpr_period = "Q1", plot_style = "wide")
q2_l <- lagged_corr_plot(Q2_corrs_l, cpr_period = "Q2", plot_style = "wide") + theme(axis.text.y = element_blank())
q3_l <- lagged_corr_plot(Q3_corrs_l, cpr_period = "Q3", plot_style = "wide") + theme(axis.text.y = element_blank())
q4_l <- lagged_corr_plot(Q4_corrs_l, cpr_period = "Q4", plot_style = "wide") + theme(axis.text.y = element_blank())

#Patch them together
lagged_corrplot <- q1_l | q2_l | q3_l | q4_l
lagged_corrplot <- lagged_corrplot & theme(legend.position = "none")
lagged_corrplot <- lagged_corrplot + labs(caption = "Correlations between mean SST from previous quarter and subsequent abundance anomalies.")
lagged_corrplot

#Export ggplot version
ggsave(lagged_corrplot, 
       filename =  here::here("R", "presentations", "sst_plots", "sst_qlagged_corrplot.png"), 
       device = "png")
