####  Missing value PCA testing
####  12/12/2019


####  Packages  ####
#library(ggbiplot)
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
buoys_aggregated <- read.csv(str_c(cpr_boxpath, "data/processed_data/buoy_anomalies.csv", sep = "/")) %>% 
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


####  Daily Anomalies Estimations  ####
daily_anoms <- buoys %>% 
  group_by(buoy_id, reading_depth, julian) %>% 
  summarise(avg_temp = mean(temp, na.rm = T),
            temp_sd  = sd(temp, na.rm = T),
            avg_sal = mean(sal, na.rm = T),
            sal_sd = sd(sal, na.rm = T)) %>% 
  right_join(buoys, by = c("buoy_id", "reading_depth", "julian"))  %>% 
  select(buoy_id, reading_depth, Date, julian, period, everything()) %>% 
  mutate(temp_anom = (temp - avg_temp) / temp_sd,
         sal_anom  = (sal - avg_sal) / sal_sd) %>% 
  ungroup()

####__####

####  Manual PCA  ####

####  Need a Matrix with and without NA values
buoys_aggregated %>% filter(is.na(temp_anom == TRUE))

#Buoy N has lots of NA values
buoys_aggregated %>% 
  filter(is.na(temp_anom) == TRUE, buoy_id == "Buoy_N") %>% 
  arrange(year, reading_depth)

#Do some reshaping - save matrix w/ NA value
with_na <- buoys_aggregated %>% 
  filter(year %in% c(2005:2010), 
         buoy_id == "Buoy_N",
         period == "Q1") %>% 
  arrange(year, reading_depth) %>% 
  select(-mean_temp, -mean_sal, -sal_anom) %>% 
  mutate(#reading_depth = as.character(reading_depth),
         reading_depth = str_replace_all(reading_depth, " meter", ""),
         reading_depth = str_replace_all(reading_depth, "s", ""),
         reading_depth = str_pad(reading_depth, 3, side = "left", pad = "0"),
         reading_depth = str_c(reading_depth, "m")) %>% 
  pivot_wider(names_from = reading_depth, values_from = temp_anom) %>% 
  mutate(buoy_id = str_replace_all(buoy_id, "Buoy_", ""),
         key = str_c(buoy_id, year, period, sep = "-")) %>% 
  select(key, -buoy_id, -year, -period,  `001m`:`180m`) %>% 
  column_to_rownames(var = "key") %>% 
  as.matrix()


####  Matrix Algebra Testing  ####

#Here  is our tester
with_na


#Here is our tester without NA's, just filling the hole
without_na <- with_na
without_na[is.na(without_na)] <- 2

#Variance covariance [S]
var(with_na)

#The variance for that hole now comes from all values not absent
var(with_na, na.rm = T)  

#What a full matrix looks like
var(without_na)

#Correlation matrices [R]
cor(with_na)     
cor(without_na)

####  Computation of eigenvalues  ####

#The eigen() function computes the eigenvalues and eigenvectors simultaneously. 
#Therefore it’s typically best to save the results in a variable and access the appropriate vector.

# PCA analysis calculates the eigenvectors of the variance covariance matrix [S]

#Dealing with NA values
e_na <- eigen(var(with_na))
e_na <- eigen(var(with_na, na.rm = T))

e_full <- eigen(var(without_na))
e_full$values
e_full$vectors

#The squared eigenvecctors all sum to 1
colSums(e_full$vectors ^ 2) 

####__####

####  PCA from covariance matrix  ####
x_anoms <- without_na
S <- var(x_anoms)
E <- eigen(S)

#Somehow we go from there to the PCA components by multiplying eigenvectors to the standardized values...

#Should be able to get the principal components this way... 
U <- t(E$vectors) %*% x_anoms
U

#but prcomp() eigenvaules/vectors do not match eigen()
pca_full <- prcomp(without_na, scale = FALSE, center = FALSE)
t(pca_full$rotation)
pca_full$x


#But you can correctly go back to the raw values...
E$vectors %*% U
x_anoms

#Is prcomp() scaling?
x_scaled <- scale(x_anoms)
U_scaled <- t(eigen(S)$vectors) %*% x_scaled


#No but it uses singular vector decomposition
prcomp_pc <- svd(x_anoms)$v
dimnames(prcomp_pc) <- list(colnames(x_anoms), paste0("PC", seq_len(ncol(prcomp_pc))))
prcomp_pc
pca_full$rotation


####  Compare manual eigenvalue estimation to prcomp()

#Conversely you should be able to get the raw data from the eigenvalues and the principal components
pca_test <- prcomp(x_anoms, scale. = FALSE, center = FALSE)
pca_test$sdev  #sd of principal components (sqrt of the eigenvalues of the covariance/variance matrix)
sqrt(E$values) #doesn't match
pca_test$rotation #matrix of variable loadings, columns are the eigenvectors
E$vectors

pca_test$x #Input data multiplied by the rotation matrix
x_anoms

#We could also just omit the NA values
prcomp(na.omit(with_na))

#Compare to princomp()
pca_test_2 <- princomp(cor = FALSE, covmat = S)

#Do these loadings match?
pca_test_2$loadings
pca_test_2$sdev


t(E$vectors) %*% x_anoms


####__#### 

####  Daily Scale PCA  ####
buoys_pca_dat <- daily_anoms %>% 
  select(-avg_temp, -avg_sal, -temp_sd, -sal_sd, -density, - temp, -sal) %>% 
  rename(temp = temp_anom, sal = sal_anom) %>% 
  mutate(year = lubridate::year(Date),
         reading_depth = str_replace_all(reading_depth, " meter", ""),
         reading_depth = str_replace_all(reading_depth, "s", ""),
         reading_depth = str_pad(reading_depth, 3, side = "left", pad = "0"),
         reading_depth = str_c(reading_depth, "m"),
         buoy_id = str_replace_all(buoy_id, "Buoy_", "")) %>% 
  pivot_wider(names_from = c(reading_depth, buoy_id), values_from = c(temp, sal)) %>% 
  select(Date, everything())  %>% 
  select(-julian, -year, -period) 

buoys_pca_mat <- buoys_pca_dat %>% 
  column_to_rownames(var = "Date") %>% 
  as.matrix()

(days_dropped <- nrow(buoys_pca_dat) - nrow(drop_na(buoys_pca_dat)))

#Every Day has a record, the measurements are the mean value for that sensor that day
head(buoys_pca_dat)
image(t(buoys_pca_mat))


#PCA using prcomp
daily_pca <- prcomp(na.omit(buoys_pca_mat), center = FALSE, scale. = FALSE)
summary(daily_pca)

#Pull the loadings out
pca_out <- daily_pca$x %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Date") %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(Date, PC1, PC2, PC3) %>% 
  pivot_longer(names_to = "Principal Component", values_to = "Principal Component Loading", cols = c(PC1, PC2, PC3))

#Fill in blank days 
every_day <- data.frame(Date = seq.Date(from = min(buoys_pca_dat$Date), to = max(buoys_pca_dat$Date), by = 1))
pca_out <- full_join(every_day, pca_out, by = "Date")

#Pllot first three modes
ggplot(filter(pca_out, `Principal Component` != "PC3" & is.na(`Principal Component`) == FALSE), 
       aes(x = Date,
           y = `Principal Component Loading`, 
           color = `Principal Component`)) +
  geom_line()
