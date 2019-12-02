####  Species-Species Correlation Over time + k-means clustering
####  11/25/2019
# Following up on 11/25/2019 meeting with Andy
# https://uc-r.github.io/kmeans_clustering#optimal

####  Packages  ####
library(cluster)
library(factoextra)
library(tidyverse)
library(here)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

#Set ggplot theme
theme_set(theme_classic())

####  Load Data  ####

# SST long-format with the lag periods
sst_long_lagged <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "SST_with_lags.csv", sep = "/")) %>% 
  mutate(period = case_when(
    period == "annual" ~ "Annual",
    period == "jf" ~ "January - February",
    period == "ma" ~ "March - April",
    period == "mj" ~ "May - June",
    period == "ja" ~ "July - August",
    period == "so" ~ "September - October",
    period == "nd" ~ "November - December"),
    period = factor(period, 
                    levels = c("Annual", "January - February", "March - April", "May - June", "July - August", "September - October", "November - December")))

# CPR species Data with the corrresponding SST
species_periods_long <- read_csv(str_c(cpr_boxpath, "data", "processed_data", "cpr_with_SSTlags.csv", sep = "/"))  %>% 
  mutate(period = case_when(
    period == "annual" ~ "Annual",
    period == "jf" ~ "January - February",
    period == "ma" ~ "March - April",
    period == "mj" ~ "May - June",
    period == "ja" ~ "July - August",
    period == "so" ~ "September - October",
    period == "nd" ~ "November - December")) %>% 
  mutate(
    period = factor(period, 
                    levels = c("Annual", 
                               "January - February", 
                               "March - April", 
                               "May - June", 
                               "July - August", 
                               "September - October", 
                               "November - December"))
    )


####  Species Abundance Timelines  ####
species_list <- list(
  "calanus" = filter(species_periods_long, species == "calanus"),
  "calanus1to4" = filter(species_periods_long, species == "calanus1to4"),
  "centropages" = filter(species_periods_long, species == "centropages"),
  "euphausiacea" = filter(species_periods_long, species == "euphausiacea"),
  "metridia" = filter(species_periods_long, species == "metridia"),
  "oithona" = filter(species_periods_long, species == "oithona"),
  "para_pseudocalanus" = filter(species_periods_long, species == "para_pseudocalanus")
)

#Export Timelines
imap(species_list, function(x, y) {
  species_plot <- ggplot(x, aes(year, anomaly)) +
    geom_hline(yintercept = 0, alpha = 0.3, color = "darkred", linetype = 2) +
    geom_point(color = gmri_cols("gmri blue")) +
    geom_line(color = gmri_cols("gmri blue"), group = 1) +
    labs(x = NULL, y = NULL, caption = y) +
    facet_wrap(~period, ncol = 2)
  
  ggsave(plot = species_plot, filename = here::here("R", "presentations", "abundance_timelines", str_c(y, ".png")), device = "png")
  
})




####  Abundance Timeline bi-plots  ####
comparison_df <- species_periods_long %>% 
  mutate(decade = floor_decade(year),
         decade = factor(decade),
         pca_period = ifelse(year <= 2010, "Early", "Late")) %>% 
  filter(period == "Annual")  %>% 
  arrange(year) %>% 
  pivot_wider(names_from = species, values_from = anomaly)
  

#Mode 1 Strong Positive: Centropages & Oithona
#Mode 2 Strong Positive: Calanus
#Mode 2 Strong Negative: Pseudocalanus
  
#Mode 1 and Mode 2 strong positive for each
comparison_df %>% 
  ggplot(aes(calanus, centropages, color = decade, linetype = pca_period)) +
    geom_point() +
    geom_path(alpha = 0.6)

comparison_df %>% 
  ggplot(aes(calanus, oithona, color = decade, linetype = pca_period)) +
  geom_point() +
  geom_path(alpha = 0.6)

#Opposite Responses to Mode 2
comparison_df %>% 
  ggplot(aes(x = calanus, y = para_pseudocalanus, color = decade, linetype = pca_period)) +
  geom_point() +
  geom_path(alpha = 0.6)


####  Cluster Analyses  ####
#Follows: https://uc-r.github.io/kmeans_clustering#optimal
cluster_df <- comparison_df %>%  select(calanus:para_pseudocalanus) %>% drop_na()
cluster_years <- comparison_df %>% select(year, calanus: para_pseudocalanus) %>% drop_na()

#Data Organization
#Rows are observations
#columns are variables
#standardize to mean = 0, standard deviation = 1 scale()
# remove NA values

#Get default euclidean distances
distance <- get_dist(cluster_df)

#Visualize distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


####  K-means Clustering  ####

# centers defines the number of clusters, nstart is the configuration attempt number
k5 <- kmeans(cluster_df, centers = 5, nstart = 25)


#Visualizing the clusters using PCA (default method if k > 2)
fviz_cluster(k5, data = cluster_df)

#Alternatively, can add the cluster details back to the df and do pairwise plots
cluster_years %>% 
  mutate(cluster = factor(k5$cluster)) %>% 
  ggplot(aes(calanus, centropages, color = cluster)) +
           geom_text(aes(label = year)) +
           ggforce::geom_mark_ellipse()



#Testing different k values
k2 <- kmeans(cluster_df, centers = 2, nstart = 25)
k3 <- kmeans(cluster_df, centers = 3, nstart = 25)
k4 <- kmeans(cluster_df, centers = 4, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = cluster_df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = cluster_df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = cluster_df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = cluster_df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(grobs = list(p1, p2, p3, p4), nrow = 2)



####  Determining Optimal Clusters  ####

#Elbow method - visualize determine where the "bend" in the following graph is
fviz_nbclust(cluster_df, kmeans, method = "wss")


# Average Silhouette - maximize the avargae silhouette
# # function to compute average silhouette for k clusters
# avg_sil <- function(k) {
#   km.res <- kmeans(cluster_df, centers = k, nstart = 25)
#   ss <- silhouette(km.res$cluster, dist(cluster_df))
#   mean(ss[, 3])
# }
# 
# # Compute and plot wss for k = 2 to k = 15
# k.values <- 2:15
# 
# # extract avg silhouette for 2-15 clusters
# avg_sil_values <- map_dbl(k.values, avg_sil)
# 
# plot(k.values, avg_sil_values,
#      type = "b", pch = 19, frame = FALSE, 
#      xlab = "Number of clusters K",
#      ylab = "Average Silhouettes")

#Or built in function...
fviz_nbclust(cluster_df, kmeans, method = "silhouette")


#Gap Statistic Method
# compute gap statistic
set.seed(123)
gap_stat <- clusGap(cluster_df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

fviz_gap_stat(gap_stat)


#Final Analysis
# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(cluster_df, 2, nstart = 25)
print(final)

#plotting
fviz_cluster(final, data = cluster_df)


#Species bi-plots
(sp_1 <- cluster_years %>% 
  mutate(cluster = factor(final$cluster),
         decade = factor(floor_decade(year))) %>% 
  ggplot(aes(centropages, calanus, color = cluster)) +
  geom_text(aes(label = year)) +
  ggforce::geom_mark_ellipse())

(sp_2 <- cluster_years %>% 
    mutate(cluster = factor(final$cluster),
           decade = factor(floor_decade(year))) %>% 
    ggplot(aes(oithona, calanus, color = cluster)) +
    geom_text(aes(label = year)) +
    ggforce::geom_mark_ellipse())


(sp_3 <- cluster_years %>% 
    mutate(cluster = factor(final$cluster),
           decade = factor(floor_decade(year))) %>% 
    ggplot(aes(para_pseudocalanus, calanus, color = cluster)) +
    geom_text(aes(label = year)) +
    ggforce::geom_mark_ellipse())

(sp_4 <- cluster_years %>% 
    mutate(cluster = factor(final$cluster),
           decade = factor(floor_decade(year))) %>% 
    ggplot(aes(calanus1to4, calanus, color = cluster)) +
    geom_text(aes(label = year)) +
    ggforce::geom_mark_ellipse())


grid.arrange(grobs = list(sp_1, sp_2, sp_3, sp_4), nrow = 2)
