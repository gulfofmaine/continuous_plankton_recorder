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
buoys <- read_csv(str_c(cpr_boxpath, "data/processed_data/buoys_daily.csv", sep = "/"), col_types = cols())
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

####________________________________####

####  Dealing with Gappy Matrix  ####

###1.  Manual PCA  ####

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


###__ Matrix Algebra Testing  ####

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

###__ Computation of eigenvalues  ####

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


#### 2.  PCA from covariance matrix  ####
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

##### 3. Singular Vector Decomposition  ####
# #Is svd done on the covariance matrix or the raw values?
# prcomp_pc #Match this
# 
# #SVD can be performed step-by-step with R by calculating ATA and AAT then finding the eigenvalues and eigenvectors of the matrices.
# A <- x_anoms
# ATA <- t(A) %*% A
# 
# #The V component of the singular value decomposition is then found by calculating the eigenvectors of the resultant ATA matrix.
# ATA.e <- eigen(ATA)
# v.mat <- ATA.e$vectors
# v.mat
# 
# # Here we see the V matrix is the same as the output of the svd() but with some sign changes. These sign changes can happen, 
# #as mentioned earlier, as the eigenvector scaled by −1 
# v.mat[,1:2] <- v.mat[,1:2] * -1
# v.mat
# 
# #The same routine is done for the AAT matrix.
# AAT <- A %*% t(A)
# AAT
# 
# #The eigenvectors are again found for the computed AAT matrix.
# AAT.e <- eigen(AAT)
# u.mat <- AAT.e$vectors
# u.mat
# 
# #There are four eigenvectors in the resulting matrix; however, 
# #we are only interested in the non-zero eigenvalues and their 
# #respective eigenvectors. Therefore, we can remove the last 
# #eigenvector from the matrix which gives us the U matrix. 
# #Note the eigenvalues of AAT and ATA are the same except the 
# #0 eigenvalue in the AAT matrix.
# u.mat <- u.mat
# 
# #As mentioned earlier, the singular values r are 
# #the square roots of the non-zero eigenvalues of the AAT and ATA matrices.
# r <- sqrt(ATA.e$values)
# r <- r * diag(length(r))
# r 
# 
# #Our answers align with the output of the svd() function. 
# #We can also show that the matrix A is indeed equal to the components resulting from singular value decomposition.
# svd.matrix <- u.mat %*% r %*% t(v.mat)
# svd.matrix
# 
# A == round(svd.matrix, 0)

####________________________________####

####  Daily Scale Buoy PCA  ####
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

#Export the non-interpolated Buoy Data
write_csv(buoys_pca_dat, 
          path = str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_raw.csv", sep = "/"), 
          col_names = TRUE)




####__ PCA using prcomp  ####

#PCA
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
every_day <- every_day %>% 
  mutate(PC1 = NA, 
         PC2 = NA,
         PC3 = NA) %>% 
  pivot_longer(names_to = "Principal Component", values_to = "Principal Component Loading", cols = c(PC1, PC2, PC3)) %>% 
  select(-`Principal Component Loading`)

#merge in
pca_out <- full_join(every_day, pca_out, by = c("Date", "Principal Component"))

#Plot first three modes
ggplot(filter(pca_out, `Principal Component` != "PC3" & is.na(`Principal Component`) == FALSE), 
       aes(x = Date,
           y = `Principal Component Loading`, 
           color = `Principal Component`)) +
  geom_line() +
  theme_minimal()


####________________________________####

####  Buoy-Gap Interpolations  ####

#First of all where are our gaps
pca_gaps <- pca_out %>% filter(is.na(`Principal Component Loading`))

#Plot them over raw data
daily_anoms %>% 
  ggplot(aes(Date, temp_anom)) +
  geom_line(alpha = 0.3, aes(color = reading_depth)) +
  facet_grid(buoy_id~reading_depth) +
  geom_point(data = pca_gaps, aes(x = Date, y = 0), shape = 3, size = .05, alpha = 0.5)


#Data Matrix 
buoys_pca_mat[1:6,1:6]
image(t(buoys_pca_mat[nrow(buoys_pca_mat):1,]) , axes=FALSE,)
#n column index
#k row index

#variance covariance matrix
S_daily <- var(na.omit(buoys_pca_mat))
image(t(S_daily[nrow(S_daily):1,]) , axes=FALSE,)

#eigen values/vectors
E_daily <- eigen(S_daily)

#Should be able to get the principal components vectors this way... 
U_daily <- t(E_daily$vectors) * buoys_pca_mat[1,] #Day 1 PC loadings
#Need to combine them somehow


#Set up objects we want to populate

Y <- buoys_pca_mat #Y values
X <- buoys_pca_mat #X values 
#Covariance Matrix
cov_matrix <- matrix(nrow = ncol(buoys_pca_mat), ncol = ncol(buoys_pca_mat), data = NA)
#or
dim(S_daily)

####  List structure to fill with linear models of everything  ####

#Base level, for lm object
lm_list <- list("response_var" = list("lm_object" = list()))
#One for each response var
lm_list <- rep(lm_list, ncol(buoys_pca_mat))
names(lm_list) <- colnames(buoys_pca_mat)
#Down another level, for independent var
lm_list_2 <- rep(list("x_var" = lm_list), ncol(buoys_pca_mat))
names(lm_list_2) <- colnames(buoys_pca_mat)
#Can now index down for organization sake
lm_list_2$temp_001m_B$temp_001m_B$lm_object



#Function for pulling p value from lm object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p) }

#Lm Loopz
for (j in 1:ncol(buoys_pca_mat)) {
  for (k in 1:ncol(buoys_pca_mat)) {
   
    #Fill linear model list
    lm_list_2[[k]][[j]]$lm_object = lm(buoys_pca_mat[,j] ~ buoys_pca_mat[,k])
    
    #Extract p value seperately from each
    lm_list_2[[k]][[j]]$pval <- lmp(lm_list_2[[k]][[j]]$lm_object)
    
    #Significance boolean
    lm_list_2[[k]][[j]]$significant <- ifelse(lm_list_2[[k]][[j]]$pval <= 0.05, TRUE, FALSE)
    
    #As well as R-squared
    lm_list_2[[k]][[j]]$rsquared <- summary(lm_list_2[[k]][[j]]$lm_object)$r.squared
    
    #Slope
    lm_list_2[[k]][[j]]$slope <- coef(lm_list_2[[k]][[j]]$lm_object)[2]
    
    #Intercept
    lm_list_2[[k]][[j]]$intercept <- coef(lm_list_2[[k]][[j]]$lm_object)[1]
     
  }
}

#Coefficients can be pulled this way
lm_list_2$temp_001m_B$temp_020m_B$lm_object$coefficients
#pvalues are here
lm_list_2$temp_001m_B$temp_020m_B$pval
#Significance
lm_list_2$temp_001m_B$temp_020m_B$significant


#Do we want to put them into matrices?
base_mat <- matrix(data = NA, nrow = 46, ncol = 46)
colnames(base_mat) <- colnames(buoys_pca_mat)
rownames(base_mat) <- colnames(buoys_pca_mat)

#Use base mat structure
significance_mat <- base_mat
slope_mat        <- base_mat
intercept_mat    <- base_mat

#Matrix loops
for (j in 1:ncol(buoys_pca_mat)) {
  for (k in 1:ncol(buoys_pca_mat)) {
    
    significance_mat[k,j] <- lm_list_2[[k]][[j]]$significant
    slope_mat[k,j]        <- lm_list_2[[k]][[j]]$slope
    intercept_mat[k,j]    <- lm_list_2[[k]][[j]]$intercept
    
    
  }
}


# Filling in patchy DF with average regression parameters
buoy_interpolated <- buoys_pca_dat[,2:47]

#Loop 3
for (row_j in 1:nrow(buoy_interpolated)) {
  for (col_k in 1:ncol(buoy_interpolated)) {
    if(is.na(buoy_interpolated[row_j, col_k]) == TRUE) {
      #Mean Slope - significant regressions only
      m <- mean(slope_mat[which(significance_mat[, col_k] == TRUE), col_k])
      #Mean Intercept - significant regressions only
      b <- mean(intercept_mat[which(significance_mat[, col_k] == TRUE), col_k])
      #Mean X - all avaliable data from that day's measurements
      x <- mean(t(buoy_interpolated[row_j, ]), na.rm = T)
      
      #New Interpolated Value
      buoy_interpolated[row_j, col_k] <- m*x + b
    
      }
    
  }
  
}

#Add the dates back
buoy_interpolated <- cbind(buoys_pca_dat[,1], buoy_interpolated)

#What did we do
var_list <- colnames(buoys_pca_mat)
plot_list <- map(var_list, function(x){
  col_name <- sym(x)
  
  ggplot() +
    geom_line(data = buoy_interpolated, aes(Date, !!col_name, color = "Interpolated"), alpha = 0.8) + 
    geom_line(data = buoys_pca_dat, aes(Date, !!col_name, color = "Raw"), alpha = 0.8) +
    labs(x = NULL, y = paste0(col_name))

}) %>% setNames(var_list)

#Export the interpolated Buoy Data
write_csv(buoy_interpolated, 
          path = str_c(cpr_boxpath, "data/processed_data/buoy_pcadat_interpolated.csv", sep = "/"), 
          col_names = TRUE)


