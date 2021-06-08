####  Looking at matlab data from Karen Stamieszkin  ####

####  Packages  ####
library(tidyverse)
library(R.matlab)

####  Data  ####
karen_dat <- readMat("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/forAdam/CPRlengthData.mat")


####  Matlab Data Contents  ####

####  1. header  ####

#missing details on what the header is
karen_dat$header

####  2. copTmb  ####

#Copepod temperature mass regression coefficients (lengths are in micrometers um)
karen_dat$copTmb[[1]] #What species
karen_dat$copTmb[[2]] #Coefficients for stage 5 from karen_dat$stagerng
karen_dat$copTmb[[3]] #Coefficients for stage 5 from karen_dat$stagerng


#Pull the taxa out that Karen looked at:
karen_species <- vector(mode = "list", length = 84)
for (i in 1:84) {
  karen_species[[i]] <- karen_dat$copTmb[[i]][1,1]
}

names(karen_species) <- rep("species", length(karen_species))
karen_species <- map(karen_species, function(x) {x <- ifelse(is.numeric(x) == T, "NaN", x)})
karen_species <- map(karen_species, as.character)
karen_species <- bind_cols(karen_species) %>% 
  pivot_longer(names_to = "column order", values_to = "species", cols = 1:84) %>% 
  filter(species != "NaN") %>% 
  select(species)
karen_species



#Extracting the regression coefficients for each species
regression_coeff <- vector(mode = "list", length = nrow(karen_species))
names(regression_coeff) <- karen_species$species
 regression_coeff <- map(regression_coeff, function(x) {
  x <- list(slopes     = "not set",
            intercepts = "not set")
})
 
slopes     <- karen_dat$copTmb[seq(from = 2, to = 83, by = 3)] 
intercepts <- karen_dat$copTmb[seq(from = 3, to = 84, by = 3)]

for (i in 1:28) {
  regression_coeff[[i]]$slopes     <- slopes[[i]]
  regression_coeff[[i]]$intercepts <- intercepts[[i]]
  
}



#Now everything is together
regression_coeff$`Calanus finmarchicus`$slopes


####   3. metadata  ####

#Station information
station_data <- karen_dat$metadata
colnames(station_data) <- c("year", "month", "day", "hour", "minute", "lat", "long")
station_data <- as.data.frame(station_data)

####  4. originalconc  ####

#Species abundance/concentration
original_conc <- karen_dat$originalconc
colnames(original_conc) <- karen_species$species
original_conc <- as.data.frame(original_conc)

####  5. stagerng  ####

#Stage ranges for the 28 taxa
karen_dat$stagerng

#add to karen_species df
karen_species$min_stage <- 0
karen_species$max_stage <- 0
for (i in 1:nrow(karen_species)) {
  karen_species$min_stage[i] <- karen_dat$stagerng[1,i]
  karen_species$max_stage[i] <- karen_dat$stagerng[2,i]
}

karen_species <- karen_species %>% arrange(species)

####  6. T  ####

#Temperature
karen_dat$T

station_data$temp <- karen_dat$T


#Put the station information with the original concentrations
station_conc <- bind_cols(station_data, original_conc)


####__####
####  new R objects  ####
karen_species    # Species names with their min and max stages
station_conc     # Station data with original concentrations for the 28 species
regression_coeff # The modeling terms to get length from stage and temperature




####__####
####  Karen's steps  ####
#####  1. Convert original conc. to conc. for each stage  ####
# Assumes that they are present in equal proportions

#Changes list order to be alphabetical, be careful
new_concentrations <- map(split(karen_species, karen_species$species), #Taxa name and the min/max stages
  function(x,y) {
    
    # #Testing
    #x <- karen_species %>% filter(species == "Calanus finmarchicus")
    
    #Pull taxa details
    stages       <- as.character(x$min_stage:x$max_stage)
    stages       <- str_c("stage_", stages)
    n_stages     <- length(stages)
    
    #Pull original values
    total_conc <- data.frame(species_name = original_conc[, x$species])
    names(total_conc) <- x$species
    
    #Split by number of stages
    stage_list <- vector(mode = "list", length = n_stages)
    names(stage_list) <- stages
    
    #Only split it if there are numerous stages
    if(length(stage_list) > 0) { 
      
      #For each split divide the total concentration evenly
      stage_list <- imap(stage_list, function(x,y) {
        x_out <- data.frame(stage_num = total_conc/n_stages)
        names(x_out) <- y
        return(x_out)
      })
    
    #bind them as to one dataframe
    new_conc <- bind_cols(stage_list) } else { 
    names(total_conc) <- stages[1]
    new_conc <- total_conc }
    
    #Return the new values
    return(new_conc)
    
  }) 

# convert stages to lengths based on temmperature
# units are in micrometers (um)

new_concentrations #Concentration values split into stages
regression_coeff   #The regression coefficients that go with them

####  2. Get Lengths for Each stage according to temperature  ####
new_lengths <- imap(new_concentrations, function(x,y){
  
  # #Testing
  # x <- new_concentrations[["Calanus finmarchicus"]]
  # taxa <- "Calanus finmarchicus"
  
  #Constants
  taxa <- y
  temp <- station_conc$temp
  
  #Regression coefficients
  slopes <- regression_coeff[[taxa]]$slopes
  intercepts <- regression_coeff[[taxa]]$intercepts
  
  #Stages for the taxa
  stage_indices <- as.numeric(str_remove_all(names(x), pattern = "stage_"))
  
  #Coefficients to use, as vectors
  slopes     <- rep(slopes[stage_indices], nrow(x))
  intercepts <- rep(intercepts[stage_indices], nrow(x))
  
  #Out df
  lengths_out <- x
  
  #Convert concentrations to lengths (um)
  for (i in 1:length(stage_indices)) {
    lengths_out[,i] <- temp * slopes[i] + intercepts[i]
    
  }
  
  return(lengths_out)
  
  
})


# And Voila
new_lengths$`Calanus finmarchicus`




####  Re-Package everything to use with full CPR dataset
coefficient_table <- regression_coeff %>% map(function(x){
  x %>% 
    bind_rows() %>% 
    rowid_to_column(var = "copepodite_stage")}) %>% 
  bind_rows(.id = "taxa_stage") %>% 
  arrange(taxa_stage)


coefficient_table


# Export Table:
source(here::here("R", "cpr_helper_funs.R"))

write_csv(x = coefficient_table, 
          path = str_c(ccel_boxpath, "Data", "Gulf of Maine CPR", "2020_combined_data", "karen_s_coefficients.csv", sep = "/"), 
          col_names = T)
