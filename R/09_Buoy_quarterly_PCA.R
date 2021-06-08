####  Buoy Temperature and Salinity Anomaly Correlations with CPR Data
####  12/11/2019


####  Packages  ####
library(ggbiplot)
library(ggpmisc)
library(tidyverse)
library(here)
library(patchwork)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

#Set ggplot theme
theme_set(theme_classic())

####  Load Data  ####
cpr_long <- read_csv(str_c(cpr_boxpath,"data", "processed_data", "cpr_allspecies_long_quarters.csv", sep = "/"),
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

#Temperature anomaly data from NERACOOS Buoys
buoy <- read.csv(str_c(cpr_boxpath, "data/processed_data/buoy_anomalies.csv", sep = "/")) %>% 
  mutate(reading_depth = factor(
    reading_depth, levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters")
  ))


####__####
####  Data Reshaping  ####

# Pair cpr data with quarterly Buoy Measurements
cpr_buoys <- cpr_long %>%
  filter(period != "Annual") %>% 
  left_join(buoy, by = c("year", "period")) %>% 
  filter(is.na(buoy_id) == FALSE)

# clean environment so there's no name conflicts
rm("buoy", "cpr_long")

####__ Reshape####
# Reshape data so we have a column for each buoy-depth-measurement
corr_df <- cpr_buoys %>% 
  mutate(buoy_id = str_replace_all(buoy_id, "Buoy_", ""),
         reading_depth = str_replace_all(reading_depth, " meter", ""),
         reading_depth = str_replace_all(reading_depth, "s", ""),
         reading_depth = str_pad(reading_depth, 3, side = "left", pad = "0")) %>% 
  select(taxa = species, year, period, pop_anom = anomaly, buoy = buoy_id, depth = reading_depth, t = temp_anom,s = sal_anom)


# Final datasets for PCA, and for correlation tables by quarter
corr_setup_full <- corr_df %>% 
  pivot_wider(names_from = taxa, values_from = pop_anom) %>% 
  pivot_wider(names_from = c(buoy, depth), values_from = c(t, s)) 

####__ Set Focal Taxa  ####

# Rename columns we care about for plot
corr_setup_full <- corr_setup_full %>% 
  rename(
    "calanus 1-4" = "calanus1to4",
    "para-pseudocalanus" = "para_pseudocalanus"
  )

#Taxa we care about - need to match name changes
my_taxa <- c("calanus", "calanus 1-4", "centropages", "chaetognatha",
             "euphausiacea", "metridia", "oithona", "para-pseudocalanus",
             "paraeucheata", "temora")

#Taxa levels - for plotting
taxa_levels <- c(
  "calanus 1-4", "calanus",      # calanus
  "oithona", "temora", "metridia", "para-pseudocalanus","paraeucheata", "centropages",  #smaller copepods
  "euphausiacea", "chaetognatha") # big zooplankton)



###__ Prep Quarters  ####
Q1 <- corr_setup_full %>% filter(period == "Q1") %>% drop_na()
Q2 <- corr_setup_full %>% filter(period == "Q2") %>% drop_na()
Q3 <- corr_setup_full %>% filter(period == "Q3") %>% drop_na()
Q4 <- corr_setup_full %>% filter(period == "Q4") %>% drop_na()



####__####

####  Corrplot Functions  ####


#Pull correlations with p-values
corr_plot_setup <- function(wide_df) {
  
  # 1. Pull data used for corellation matrix
  corr_data <- wide_df %>% 
    select(-year, -period)
  
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


# custom corr plot
cpr_corr_plot <- function(corr_dataframe, period = "Q1", plot_style = "tall"){
  
  #Filter Var1 and Var2 to reshape plot
  long_plot <- corr_dataframe %>% 
    filter(Var1 %notin% my_taxa,
           Var2 %in% my_taxa) %>% 
    mutate(Var2 = factor(Var2, levels = taxa_levels))
  
  tall_plot <- corr_dataframe %>% 
    filter(Var1 %in% my_taxa,
           Var2 %notin% my_taxa) %>% 
    mutate(Var1 = factor(Var1, levels = taxa_levels))
  
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
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
          axis.text.y = element_text(size = 6),
          legend.position = leg_position) +
    coord_fixed()
    
  
}


# data for corrplots
Q1_corrs <- corr_plot_setup(Q1)
Q2_corrs <- corr_plot_setup(Q2)
Q3_corrs <- corr_plot_setup(Q3)
Q4_corrs <- corr_plot_setup(Q4)


####__####

####  Quarterly CPR-Buoy Corrplots  ####
q1_t <- cpr_corr_plot(Q1_corrs, period = "Q1", plot_style = "tall")
q2_t <- cpr_corr_plot(Q2_corrs, period = "Q2", plot_style = "tall") + theme(axis.text.y = element_blank())
q3_t <- cpr_corr_plot(Q3_corrs, period = "Q3", plot_style = "tall") + theme(axis.text.y = element_blank())
q4_t <- cpr_corr_plot(Q4_corrs, period = "Q4", plot_style = "tall") + theme(axis.text.y = element_blank())

#Patch them together
quarterly_corrplot <- q1_t | q2_t | q3_t | q4_t
quarterly_corrplot <- quarterly_corrplot & theme(legend.position = "none")
quarterly_corrplot

#Export
ggsave(quarterly_corrplot, 
       filename =  here::here("R", "presentations", "buoy_plots", "buoy_quarterly_corrplot.png"), 
       device = "png")

####__####

####  Buoy Sensor & CPR PCA  ####

# All the CPR Data and the Buoy Data, no missing records, no interpolated buoy measures
buoy_pca_dat <- corr_setup_full %>% drop_na()

#Perform PCA
pca_buoys <- prcomp(x = select(buoy_pca_dat, -year, -period), 
                    center = F, 
                    scale. = F)
summary(pca_buoys)

#Bi-Plot
buoy_pca_dat$decade <- factor(floor_decade(buoy_pca_dat$year))
ggbiplot(pca_buoys, 
         ellipse=TRUE, 
         labels = buoy_pca_dat$year,
         groups = buoy_pca_dat$decade, 
         obs.scale = T, 
         var.scale = T)





