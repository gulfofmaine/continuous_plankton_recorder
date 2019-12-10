# Buoy x CPR data Relationship
# 12/3/2019

#### CPR Dataset - Principal Component Analysis - QUARTERLY Anomalies
#### Adam A. Kemberling
#### 12/02/2019

####  Packages  ####
library(ggpmisc)
library(tidyverse)
library(here)
library(patchwork)

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

buoy <- read.csv(str_c(cpr_boxpath, "data/processed_data/buoys_aggregated.csv", sep = "/"))



####  Pair cpr data with quarterly measurements  ####
cpr_buoys <- cpr_long %>%
  filter(period != "Annual") %>% 
  left_join(buoy, by = c("year", "period")) %>% 
  mutate(reading_depth = factor(reading_depth, 
         levels = c("1 meter", "20 meters", "50 meters", "100 meters", "150 meters", "180 meters"))
  )


# ####  Export cpr_buoys  ####
# write_csv(cpr_buoys, path = str_c(cpr_boxpath,"data", "processed_data", "cpr_quarters_buoys.csv", sep = "/"), col_names = TRUE)
# write_csv(cpr_buoys, path = here::here("R", "cpr_buoy_DE", "Data", "cpr_quarters_buoys.csv"), col_names = TRUE)


####  Data Exploration  ####

#What information do we want to highlight?
cpr_buoys %>% 
  filter(is.na(buoy_id) == FALSE,
         #period  == "Q1",
         species == "calanus") %>% 
  ggplot(aes(mean_strat_index, anomaly)) +
    geom_smooth(method = "lm", se = FALSE, color = "gray50") +
    geom_point() +
    stat_poly_eq(formula = y ~ x, 
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +  
    facet_grid(period ~ buoy_id, scales = "free") +
    labs(x = "Stratification Index ()",
         y = "Population Anomaly (sd)") +
    theme_bw()
    


#Buoy N is the furthest offshore
#Buoy M isn't loading correctly - but would be the next best
# B, E, & I are a little offshore
# F is in penobscot bay

#Offshore Stratification - Buoy N
species_l <- list(
  species_01 =  c("calanus", "calanus1to4", "centropages", "euphausiacea"),
  species_02 = c("metridia", "oithona", "para_pseudocalanus", "paraeucheata"))

cpr_buoys %>% 
  #Choose a Buoy
  filter(buoy_id == "Buoy_N") %>% 
  #Choose a species group
  filter(species %in% species_l$species_01) %>% 
  ggplot(aes(mean_strat_index, anomaly)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray50") +
  geom_point() +
  stat_poly_eq(formula = y ~ x, 
               eq.with.lhs = "italic(hat(y))~`=`~",
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +  
  facet_grid(period ~ species) +
  labs(x = "Stratification Index",
       y = "Population Anomaly (sd)") +
  theme_bw()






#####  Correlation Tables  ####
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

#Make a list containing correlation coefficients for each grouping
confusing_matrix_2 <- cpr_buoys %>%
  filter(is.na(buoy_id) == FALSE) %>%
  split(.$period) %>%
    map(~ .x %>% split(.$buoy_id) %>%
        map(~ .x %>%  split(.$reading_depth) %>%
          map(~ .x %>%
              pivot_wider(names_from = species, values_from = anomaly) %>%
                select(-year, -period, -buoy_id, -reading_depth) %>%
                drop_na() %>%
                
                #Get correlation matrix
                cor() %>%
                #get_upper_tri() %>%
                reshape2::melt() %>% 
                drop_na()

            )
          )
        )


#What did we just do...
confusing_matrix_2$Q3$Buoy_B$`1 meter` %>%
  ggplot(aes(x = fct_rev(Var1), y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() + labs(x = NULL, y = NULL, title = "Q3-Buoy B-1 meter") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  coord_fixed()


####__####

####  Full Correlation Table Workup  ####
corr_list  <- cpr_buoys %>% ####  Period Buoy and Reading Depth  ####
  filter(is.na(buoy_id) == FALSE) %>% 
  split(.$period) %>% 
  map(~ .x %>% split(.$buoy_id) %>% 
        map(~ .x %>%  split(.$reading_depth) %>% 
              map(function(x){
                
                # 1. Pull data used for corellation matrix
                corr_data <- x %>% 
                    pivot_wider(names_from = species, values_from = anomaly) %>% 
                    select(-year, -period, -buoy_id, -reading_depth) %>%
                    drop_na()
                
                # 2. Pull the correlation matrix and melt to a dataframe
                corr_out <- corr_data %>%
                  #Get correlation matrix
                  cor() %>% 
                  get_upper_tri() %>%
                  reshape2::melt() %>% 
                  drop_na()
                
                # 3. do it again but pull the p-values
                if(is.null(corr_data) == FALSE & nrow(corr_data) >= 3) {
                  
                  #Get correlation matrix
                  corr_mat <- corr_data %>% cor()
                  #Pull p-values
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
                  
                } else {
                  p_data <- data.frame("insufficient data" = NULL)
                  corr_full <- data.frame("insufficient data" = NULL)
                  }
                
                #Just return the corr_out data...
                return(corr_full)
              })
        )
  )

#Check a reading
corr_list$Q1$Buoy_B$`1 meter`

####  Plotting A Single Buoy + Depths  ####
bind_rows(corr_list$Q1$Buoy_B, .id = "reading_depth")  %>% 
  ggplot(aes(x = Var1, y = fct_rev(Var2), fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sig_symbol), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = "Q1-Buoy B-1 meter") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        legend.position = "bottom")+
  coord_fixed() +
  facet_wrap(~reading_depth)









######__####


####  By Quarter and Buoy  ####
quarter_list  <- cpr_buoys %>% ####  Quarterly Correlations  ####
  filter(is.na(buoy_id) == FALSE) %>% 
  split(.$buoy_id) %>% 
    map(~ .x %>% split(.$period) %>% 
      map(function(x){
      
        # 1. Pull data used for corellation matrix
        corr_data <- x %>% 
          pivot_wider(names_from = species, values_from = anomaly) %>% 
          select(-year, -period, -buoy_id, -reading_depth) %>%
          drop_na()
        
        # 2. Pull the correlation matrix and melt to a dataframe
        corr_out <- corr_data %>%
          #Get correlation matrix
          cor() %>%
          #get_upper_tri() %>%
          reshape2::melt(na.rm = T)
        
        # 3. do it again but pull the p-values
        if(is.null(corr_data) == FALSE & nrow(corr_data) >= 3) {
          
          #Get correlation matrix
          corr_mat <- corr_data %>% cor()
          #Pull p-values
          p_data <- corrplot::cor.mtest(corr_mat)$p 
          #Assign the same names as the corr matrix
          dimnames(p_data) <- dimnames(corr_mat)
          #reshape to match correlation df
          p_data <- reshape2::melt(p_data, na.rm = T) %>% rename(pval = value)
          
          
          #Put the two together
          corr_full <- full_join(corr_out, p_data, by = c("Var1", "Var2")) %>% 
            #Format levels and labels
            mutate(Var1 = fct_relevel(Var1, sort),
                   Var2 = fct_relevel(Var2, sort),
                   sig_symbol = if_else(pval <= 0.05 & value > 0, "+", " "),
                   sig_symbol = if_else(pval <= 0.05 & value < 0, "-", sig_symbol))
          
        } else {
          p_data <- data.frame("insufficient data" = NULL)
          corr_full <- data.frame("insufficient data" = NULL)
        }
        
        
        #Just return the corr_out data...
        return(corr_full)
      })
  )



####  Plotting Quarters by Buoy  ####

#Buoy P
buoy_b <- bind_rows(quarter_list$Buoy_B, .id = "Period") 

buoy_b_p <- buoy_b %>% 
  ggplot(aes(x = Var1, y = fct_rev(Var2), fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sig_symbol), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = "Buoy B") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  coord_fixed() +
  facet_wrap(~Period)

#Buoy M
buoy_m <- bind_rows(quarter_list$Buoy_M, .id = "Period")
buoy_m_p <-  buoy_m %>% 
  ggplot(aes(x = Var1, y = fct_rev(Var2), fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sig_symbol), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  labs(x = NULL, y = NULL, title = "Buoy M") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  coord_fixed() +
  facet_wrap(~Period)


#Side by Side
buoy_b_p + buoy_m_p


####  All Buoys DF  ####

#Function to gather them all
buoy_bind <- function(buoy_corr_list) {
  buoy_b <- bind_rows(buoy_corr_list$Buoy_B, .id = "Period")
  buoy_e <- bind_rows(buoy_corr_list$Buoy_E, .id = "Period")
  buoy_f <- bind_rows(buoy_corr_list$Buoy_F, .id = "Period")
  buoy_i <- bind_rows(buoy_corr_list$Buoy_I, .id = "Period")
  buoy_m <- bind_rows(buoy_corr_list$Buoy_M, .id = "Period")
  buoy_n <- bind_rows(buoy_corr_list$Buoy_N, .id = "Period")
  
  all_buoys <- bind_rows(list(
    "Buoy B" = buoy_b,
    "Buoy E" = buoy_e,
    "Buoy F" = buoy_f,
    "Buoy I" = buoy_i,
    "Buoy M" = buoy_m,
    "Buoy N" = buoy_n
    ), .id = "buoy_id")
  
  return(all_buoys)
}

# Full Dataframe of Each Byou, and it's quarterly aggregation
all_buoys_df <- buoy_bind(quarter_list)




#Can plot specific ones with filter
buoy_plot <- function(all_buoys_df, which_buoy, which_period) {
  
  
  all_buoys_df %>% 
    filter(buoy_id == which_buoy,
           Period == which_period) %>% 
    ggplot(aes(x = Var1, y = fct_rev(Var2), fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sig_symbol), color = "black", size = 3) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal() + 
    labs(x = NULL, y = NULL, 
         title = str_c(which_buoy, " - ", which_period)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    coord_fixed()
} # Buoy / Period Plots 

#Targeted plots
buoy_plot(all_buoys_df, "Buoy M", "Q3")
buoy_plot(all_buoys_df, "Buoy N", "Q3")


####__####

#Pulling unique combinations
test_df <- tribble(
  ~"col1", ~"col2",
  "A", "B",
  "B", "A",
  "A", "C",
  "C", "A",
  "C", "C"
)

test_df[!duplicated(t(apply(test_df,1, sort))),]

#All buoys unique combinations
all_buoys_df <- all_buoys_df 
all_buoys_df[!duplicated(t(apply(all_buoys_df, 1, sort))),] %>% 
  buoy_plot(which_buoy = "Buoy M", "Q2")

#### Annual Aggregates - All Buoys  ####

# NOTES:
# 1m 50m, then if buoys have it do 150m or a deep read
# Temperature and salinity for all, not stratification
# Get a handle on daily mean temperature and salinity at each buoy and sensor


