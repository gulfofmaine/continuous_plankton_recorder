####  Taxon diagnostics for NOAA and SAHFOS cpr data  ####
####  1/2/2020

####  Packages  ####
library(tidyverse)
library(here)
library(patchwork)
library(gmRi)
library(sf)
library(rnaturalearth)

####  Functions  ####
source(here::here("R", "cpr_helper_funs.R"))

####____________________________####
noaa_phyto_key        <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/noaa_phyto_key_2019.csv")
noaa_phyto_abundances <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/noaa_phyto_abundances_2019.csv")

noaa_zoo_key          <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/noaa_zoo_key_2019.csv")
noaa_zoo_abundances   <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/noaa_zoo_abundances_2019.csv")

mc1_phyto <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/mc1_phyto_2019.csv")
mc1_eye   <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/mc1_eyecount_2019.csv")
mc1_trav  <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/mc1_traverse_2019.csv")
mc1_taxa  <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/mc1_taxa_key_2019.csv")

mc2_phyto <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/mc2_phyto_2019.csv")
mc2_eye   <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/mc2_eyecount_2019.csv")
mc2_trav  <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/mc2_traverse_2019.csv")
mc2_taxa  <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/mc2_taxa_key_2019.csv")


####  Heatmaps for in-use Taxa Diagnostics  ####

####  1. NOAA  Taxa Diagnostics   ####
taxa_diagnostics <- function(taxon_key, count_data) {
  taxon_list <- taxon_key %>% distinct(taxa) %>% pull()
  count_data <- count_data
  
  taxa_subsets <- map(taxon_list, function(taxon_list) {
    taxon_column_indexes <- str_which(string = names(count_data), pattern = tolower(taxon_list))
    taxon_subset <- count_data %>% select(1:10, taxon_column_indexes)
    return(taxon_subset)
  })
  
  #Set list names
  names(taxa_subsets) <- taxon_list
  return(taxa_subsets)
  
}

#List for each taxa
taxon_splits <- taxa_diagnostics(noaa_zoo_key, noaa_zoo_abundances)


#Reshape and heatplot

#Getting errors when there is no column matching the taxon
map(taxon_splits, dim)
# taxon_splits[["Sarcodina (not Foraminiferida)"]] <- NULL #No matches
# taxon_splits[["Decapoda-Arthropoda (not Brachyura)"]] <- NULL #No matches

#Plot them
noaa_heat_plots <- taxon_splits %>% imap(function (df_test, taxa_name) {

  #Check that there are taxa that match
  if (ncol(df_test) <= 10) {
    return("No associated Taxa")
    } else{

    #First and last taxon names to key on for pivot    
    start_taxa   <- names(df_test)[11]
    end_taxa     <- names(df_test)[ncol(df_test)]
  
    #Pivot longer for plot
    if(start_taxa == end_taxa | is.na(end_taxa) == TRUE) {
      names(df_test)[11] <- "abundance"
      df_test$taxon <- start_taxa
    } else {
      df_test <- df_test %>% pivot_longer(names_to = "taxon", values_to = "abundance", cols = start_taxa:end_taxa)
    }
    
    #heatmap plot
    heat_plot_out <- df_test %>% 
      group_by(year, taxon) %>% 
      summarise(
        abundance = sum(abundance, na.rm = T),
        presence = ifelse(abundance > 0, "present", "absent")) %>% 
      ungroup() %>% 
      mutate(year = factor(year)) %>% 
      ggplot(aes(year, taxon, fill = presence)) +
        geom_tile(color = "white", size = 0.1) +
        labs(x = NULL, y = NULL, title = taxa_name) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8), legend.title = element_blank())
  
    return(heat_plot_out)

    }
  
})

#Add index number to plot name
for (i in 1:length(noaa_heat_plots)) {
  names(noaa_heat_plots)[i] <- str_c(names(noaa_heat_plots)[i], i, sep = " - ")
}

#Top 8 from 05 paper
noaa_heat_plots$`Calanus finmarchicus - 16`
noaa_heat_plots$`Centropages typicus - 41`
noaa_heat_plots$`Oithona - 142`
noaa_heat_plots$`Pseudocalanus - 172`
noaa_heat_plots$`Metridia - 123`
noaa_heat_plots$`Euphausiacea - 85`
noaa_heat_plots$`Calanus - 20`
noaa_heat_plots$`Sarcodina (not Foraminiferida) - 181`


####  2. SAHFOS MC1  ####

# Split taxon by source
mc1_key_trav  <- mc1_taxa %>% filter(taxa_class == "trav") %>% rename(taxa = `taxon name`)
mc1_key_eye   <- mc1_taxa %>% filter(taxa_class == "eye") %>% rename(taxa = `taxon name`)
mc1_key_phyto <- mc1_taxa %>% filter(taxa_class == "phyto") %>% rename(taxa = `taxon name`)

#List for each taxa
taxon_splits <- taxa_diagnostics(mc1_key_trav, mc1_trav)

#Plot them
mc1_heat_plots <- taxon_splits %>% imap(function (df_test, taxa_name) {
  
  #Check that there are taxa that match
  if (ncol(df_test) <= 10) {
    return("No associated Taxa")
  } else{
    
    #First and last taxon names to key on for pivot    
    start_taxa   <- names(df_test)[11]
    end_taxa     <- names(df_test)[ncol(df_test)]
    
    #Pivot longer for plot
    if(start_taxa == end_taxa | is.na(end_taxa) == TRUE) {
      names(df_test)[11] <- "abundance"
      df_test$taxon <- start_taxa
    } else {
      df_test <- df_test %>% pivot_longer(names_to = "taxon", values_to = "abundance", cols = start_taxa:end_taxa)
    }
    
    #heatmap plot
    heat_plot_out <- df_test %>% 
      group_by(year, taxon) %>% 
      summarise(
        abundance = sum(abundance, na.rm = T),
        presence = ifelse(abundance > 0, "present", "absent")) %>% 
      ungroup() %>% 
      mutate(year = factor(year)) %>% 
      ggplot(aes(year, taxon, fill = presence)) +
      geom_tile(color = "white", size = 0.1) +
      labs(x = NULL, y = NULL, title = taxa_name) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8), legend.title = element_blank())
    
    return(heat_plot_out)
    
  }
  
})

#Plotting
mc1_heat_plots[1:10]

####  3. SAHFOS MC2  ####

# Split taxon by source
mc2_key_trav  <- mc2_taxa %>% filter(taxa_class == "trav") %>% rename(taxa = `taxon name`)
mc2_key_eye   <- mc2_taxa %>% filter(taxa_class == "eye") %>% rename(taxa = `taxon name`)
mc2_key_phyto <- mc2_taxa %>% filter(taxa_class == "phyto") %>% rename(taxa = `taxon name`)

#List for each taxa
taxon_splits <- taxa_diagnostics(mc2_key_trav, mc2_trav)

#Plot them
mc2_heat_plots <- taxon_splits %>% imap(function (df_test, taxa_name) {
  
  #Check that there are taxa that match
  if (ncol(df_test) <= 10) {
    return("No associated Taxa")
  } else{
    
    #First and last taxon names to key on for pivot    
    start_taxa   <- names(df_test)[11]
    end_taxa     <- names(df_test)[ncol(df_test)]
    
    #Pivot longer for plot
    if(start_taxa == end_taxa | is.na(end_taxa) == TRUE) {
      names(df_test)[11] <- "abundance"
      df_test$taxon <- start_taxa
    } else {
      df_test <- df_test %>% pivot_longer(names_to = "taxon", values_to = "abundance", cols = start_taxa:end_taxa)
    }
    
    #heatmap plot
    heat_plot_out <- df_test %>% 
      group_by(year, taxon) %>% 
      summarise(
        abundance = sum(abundance, na.rm = T),
        presence = ifelse(abundance > 0, "present", "absent")) %>% 
      ungroup() %>% 
      mutate(year = factor(year)) %>% 
      ggplot(aes(year, taxon, fill = presence)) +
      geom_tile(color = "white", size = 0.1) +
      labs(x = NULL, y = NULL, title = taxa_name) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8), legend.title = element_blank())
    
    return(heat_plot_out)
    
  }
  
})

#Plotting
mc2_heat_plots[1:10]




####__####

####  Taxa Consolidation  ####

#All in use taxa group from SAHFOS mc1 & mc2 sets, traverse data
SAHFOS_Zooplankton_Key <- bind_rows(list(
  "mc1" = filter(mc1_taxa, taxa_class != "phyto"),
  "mc2" = filter(mc2_taxa, taxa_class != "phyto")
), .id = "data_source") %>% 
  distinct(`taxon name`, data_source, taxa_class) %>% arrange(`taxon name`)



