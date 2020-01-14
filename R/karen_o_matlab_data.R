####  Looking at matlab data from Karen  ####

####  Packages  ####
library(R.matlab)

####  Data  ####
karen_dat <- readMat("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/forAdam/CPRlengthData.mat")


####  copTmb  ####
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

####   metadata  ####

#Station information
station_data <- karen_dat$metadata
colnames(station_data) <- c("year", "month", "day", "hour", "minute", "lat", "long")

####  originalconc  ####

#Species abundance/concentration
original_conc <- karen_dat$originalconc
colnames(original_conc) <- karen_species$species

####  stagerng  ####

#Stage ranges for the 28 taxa
karen_dat$stagerng

#add to karen_species df
karen_species$min_stage <- 0
karen_species$max_stage <- 0
for (i in 1:nrow(karen_species)) {
  karen_species$min_stage[i] <- karen_dat$stagerng[1,i]
  karen_species$max_stage[i] <- karen_dat$stagerng[2,i]
}

karen_species

####  T  ####

#Temperature
karen_dat$T

station_data$temp <- karen_dat$T

####  header  ####

#is missing details on what the header is
karen_dat$header
