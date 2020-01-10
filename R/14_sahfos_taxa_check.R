####  Taxon diagnostics for NOAA and SAHFOS cpr data  ####
####  1/2/2020

####  Packages  ####
library(tidyverse)
library(here)
library(patchwork)
library(gmRi)

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


####  NOAA  Taxa Diagnostics   ####
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

cf_test <- taxon_splits$`Calanus finmarchicus`

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


noaa_heat_plots$`Calanus finmarchicus - 16`
noaa_heat_plots$`Centropages typicus - 41`
noaa_heat_plots$`Oithona - 142`
noaa_heat_plots$`Pseudocalanus - 172`
noaa_heat_plots$`Metridia - 123`
noaa_heat_plots$`Euphausiacea - 85`
noaa_heat_plots$`Calanus - 20`
noaa_heat_plots$`Sarcodina (not Foraminiferida) - 181`


####  SAHFOS MC1 Taxa  ####

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

####  SAHFOS MC2 Taxa  ####

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

####  NOTES  ####

# NOAA dataset contains more complex/convoluted taxa-stage combinations
# This allows for more detailed information, if used consistently (it isn't)
# In order to pair with SAHFOS data you need to reduce to SAHFOS resolution

#All in use taxa group from SAHFOS mc1 & mc2 sets, traverse data
bind_rows(list("mc1" = mc1_key_trav, "mc2" = mc2_key_trav), .id = "data_source") %>% distinct(taxa, data_source) %>% arrange(taxa) %>% View("SAHFOS Traverse Key")
bind_rows(list("mc1" = mc1_key_eye, "mc2" = mc2_key_eye), .id = "data_source") %>% distinct(taxa, data_source) %>% arrange(taxa) %>% View("SAHFOS Eye-count Key")

bind_rows(list(
  "mc1" = filter(mc1_taxa, taxa_class != "phyto"),
  "mc2" = filter(mc2_taxa, taxa_class != "phyto")
), .id = "data_source") %>% distinct(`taxon name`, data_source, taxa_class) %>% arrange(`taxon name`) %>% View("SAHFOS Zooplankton Key")


#Create new columns that reflect what they are in the SAHFOS data
names(noaa_zoo_abundances) %>% sort() #this is the list of what they currently are

# Methodology:
# Reduce to species or taxa spp. when positive species identification is not available
# Specific stage groupings like Calanus Finmarchicus I-IV will be pulled out seperately if done so in SAHFOS data
noaa_zoo_refined <- noaa_zoo_abundances %>% 
  transmute(
    `acartia danae`                       = `acartia danae_unstaged` + `acartia danae_copepodite ii-vi` + `acartia danae_copepodite vi`,
    `acartia longiremis`                  = `acartia longiremis_copepodite ii-vi` + `acartia longiremis_unstaged`,
    `acartia tonsa`                       = `acartia tonsa_unstaged`,
    `acartia spp.`                        =  acartia_adult + acartia_unstaged + `acartia_copepodite i-iv` + `acartia_copepodite ii-vi` + `acartia_copepodite v-vi` + `acartia_copepodite vi`,
    `aetideidae spp.`                     =  aetideidae_unstaged,
    `amphipoda spp.`                      =  amphipoda_larva + amphipoda_unstaged,
    `anomalocera spp.`                    = `anomalocera petersoni_copepodite vi` + anomalocera_unstaged,
    `anomura spp.`                        =  anomura_unstaged,
    `appendicularia spp.`                 =  appendicularia_larva + appendicularia_unstaged,
    `atlanta spp.`                        = `atlanta_immature (sexually), juvenile, or adult`,
    `atlantidae spp.`                     = `atlantidae_immature (sexually), juvenile, or adult` + atlantidae_unstaged,
    `bivalvia spp.`                       =  bivalvia_larva + bivalvia_unstaged,
    `brachyura spp.`                      = `brachyura_immature (sexually, or juvenile)` + brachyura_nauplius + `brachyura_megalopa (postlarva)` +brachyura_zoea,
    `calanus finmarchicus i-iv`           = `calanus finmarchicus_copepodite i` + `calanus finmarchicus_copepodite ii` + `calanus finmarchicus_copepodite iii` + `calanus finmarchicus_copepodite iv` + `calanus finmarchicus_copepodite i-iv`,
    `calanus finmarchicus v-vi`           = `calanus finmarchicus_copepodite v` + `calanus finmarchicus_copepodite vi` + `calanus finmarchicus_unstaged`,
    `calanus i - iv`                      = `calanus_copepodite i-iv` + `calanus_copepodite v`,
    `calanus v-vi unidentified`           = `calanus_copepodite vi`,
    `calanus glacialis`                   = `calanus glacialis_copepodite v-vi` + `calanus glacialis_copepodite i-iv`,
    `calanus helgolandicus`               = `calanus helgolandicus_copepodite v-vi`,
    `calanus hyperboreus`                 = `calanus hyperboreus_copepodite iii-vi` + `calanus hyperboreus_copepodite i-vi`,
    `calocalanus pavo`                    = `calocalanus pavo_unstaged` + `calocalanus pavo_copepodite vi`,
    `calocalanus spp.`                    = calocalanus_adult + calocalanus_copepodite + `calocalanus_copepodite vi` + `calocalanus_copepodite v-vi` + `calocalanus_copepodite i-iv` + `calocalanus_immature (sexually), juvenile, or adult` + calocalanus_unstaged,
    `candacia armata`                     = `candacia armata_copepodite i-iv` + `candacia armata_copepodite iii-iv` + `candacia armata_copepodite v-vi` + `candacia armata_immature (sexually), juvenile, or adult` + `candacia armata_copepodite vi`,
    `candacia bipinnata`                  = `candacia bipinnata_copepodite vi` + `candacia bipinnata_unstaged`,
    `candacia curta`                      = `candacia curta_copepodite v` + `candacia curta_copepodite vi` + `candacia curta_copepodite v-vi` + `candacia curta_unstaged`,
    `candacia longimana`                  = `candacia longimana_unstaged`,
    `candacia norvegica`                  = `candacia norvegica_unstaged`,
    `candacia pachydactyla`               = `candacia pachydactyla_copepodite v`, + `candacia pachydactyla_copepodite i-iv` + `candacia pachydactyla_copepodite v-vi` + `candacia pachydactyla_copepodite vi` + `candacia pachydactyla_unstaged`,
    `candacia paenelongimana`             = `candacia paenelongimana_copepodite vi` + `candacia paenelongimana_unstaged`,
    `candacia simplex`                    = `candacia simplex_copepodite vi` + `candacia simplex_unstaged`,
    `candacia spp.`                       = candacia_copepodite + `candacia_copepodite ii` + `candacia_copepodite i-iv` + `candacia_copepodite v` +  `candacia_copepodite vi` + `candacia_copepodite v-vi` + `candacia_copepodite iii-iv`,
    `caprellidea spp.`                    = caprellidea_unstaged,
    `caridea spp.`                        = caridea_larva,
    `carinariidae spp.`                   = carinariidae_unstaged + `carinaria lamarcki_unstaged`,
    `cavoliniidae spp.`                   = cavoliniidae_unstaged,
    `centropages bradyi`                  = `centropages bradyi_adult` + `centropages bradyi_copepodite` + `centropages bradyi_copepodite i-iv` + `centropages bradyi_copepodite iv` + `centropages bradyi_copepodite iv-v` + `centropages bradyi_copepodite iv-vi` + `centropages bradyi_copepodite v` + `centropages bradyi_copepodite v-vi` + `centropages bradyi_copepodite vi` + `centropages bradyi_unstaged`,
    `centropages furcatus`                = `centropages furcatus_adult` + `centropages furcatus_copepodite iv-vi` + `centropages furcatus_copepodite vi` + `centropages furcatus_egg` + `centropages furcatus_unstaged`,
    `centropages hamatus`                 = `centropages hamatus_adult` + `centropages hamatus_copepodite iv` + `centropages hamatus_copepodite iv-vi` + `centropages hamatus_copepodite v` + `centropages hamatus_copepodite v-vi` + `centropages hamatus_copepodite vi` + `centropages hamatus_unstaged`,
    `centropages typicus`                 = `centropages typicus_adult` + `centropages typicus_copepodite` + `centropages typicus_copepodite iii` + `centropages typicus_copepodite iv` + `centropages typicus_copepodite iv-v` + `centropages typicus_copepodite iv-vi` + `centropages typicus_copepodite v` + `centropages typicus_copepodite v-vi` + `centropages typicus_copepodite vi` + `centropages typicus_egg` + `centropages typicus_unstaged`,
    `centropages violaceus`               = `centropages violaceus_copepodite vi` + `centropages violaceus_unstaged`,
    `centropages spp.`                    = `centropages_copepodite i-iv` + `centropages_copepodite v-vi` + centropages_egg + centropages_larva,
    `cephalopoda spp.`                    = cephalopoda_adult + cephalopoda_larva + cephalopoda_unstaged,
    `chaetognatha eyecount`               = `chaetognatha hpr eyecount_unstaged`,
    `chaetognatha traverse`               = `chaetognatha hpr traverse_adult` + `chaetognatha hpr traverse_unstaged` + chaetognatha_cyst,
    `cladocera spp.`                      = cladocera_larva + cladocera_unstaged,
    `clausocalanus spp.`                  = clausocalanus_adult + clausocalanus_copepodite + `clausocalanus_copepodite iv-v` + `clausocalanus_copepodite vi` + `clausocalanus_copepodite i-iv` + `clausocalanus_copepodite v-vi` + clausocalanus_unstaged,
    `clione spp.`                         = `clione_immature (sexually, or juvenile)` + clione_unstaged,
    `clytemnestra scutella`               = `clytemnestra scutellata_immature (sexually), juvenile, or adult` + `clytemnestra scutellata_unstaged`,
    `clytemnestra sp`                     = `clytemnestra_copepodite v-vi` + `clytemnestra_copepodite vi` + `clytemnestra_immature (sexually), juvenile, or adult` + clytemnestra_unstaged,
    `cnidaria spp.`                       = cnidaria_medusa + cnidaria_unstaged,
    `copepod eggs`                        = copepoda_egg,
    `copepod nauplii`                     = copepoda_nauplius,
    `copepoda spp.`                       = copepoda_copepodite + `copepoda_copepodite i-iv` + `copepoda_copepodite i-v` + `copepoda_copepodite i-vi` + `copepoda_copepodite iv-v` + `copepoda_copepodite iv-vi` + `copepoda_copepodite v` + `copepoda_copepodite v` + `copepoda_copepodite vi` + `copepoda_immature (sexually, or juvenile)` + `copepoda_parva (postlarva)` + copepoda_unstaged,
    `copilia mirabilis`                   = `copilia mirabilis_copepodite i-vi` + `copilia mirabilis_copepodite v-vi` + `copilia mirabilis_copepodite vi` + `copilia mirabilis_unstaged`,
    `copilia quadrata`                    = `copilia quadrata_copepodite vi` + `copilia quadrata_unstaged`,
    `copilia spp.`                        = copilia_unstaged + `copilia_copepodite vi` + `copilia_copepodite i-vi`,
    `corycaeus spp.`                      = corycaeus_adult + `corycaeus_copepodite i-vi` + `corycaeus_copepodite vi` + `corycaeus_immature (sexually), juvenile, or postlarva` + corycaeus_unstaged,
    `creseis spp.`                        = creseis_unstaged,
    `crustacea spp.`                      = crustacea_unstaged,
    `ctenophora spp.`                     = ctenophora_unstaged,
    `cumacea spp.`                        = cumacea_unstaged,
    `cyclopoida spp.`                     = `cyclopoida_copepodite vi`,
    `decapoda spp.`                       = `decapoda-arthropoda (not brachyura)_larva` + decapoda_larva,
    `diastylis rathkei`                   = `diastylis rathkei_unstaged`,
    `diastylis spp.`                      = diastylis_unstaged,
    `echinoderm larvae`                   = echinodermata_larva,
    `echinoderm egg`                      = echinodermata_larva,
    `echinoderm spp.`                     = `echinodermata_immature (sexually, or juvenile)` + echinodermata_unstaged,
    `ectoprocta cyphonautes`              = ectoprocta_cyphonautes,
    `ectoprocta spp.`                     = ectoprocta_larva,
    `eucalanidae spp.`                    = eucalanidae_copepodite + `eucalanidae_copepodite i-iv` + eucalanidae_nauplius + eucalanidae_unstaged, 
    `eucalanus spp.`                      = eucalanus_nauplius + `eucalanus_copepodite ii` + `eucalanus_copepodite v` + eucalanus_copepodite + `eucalanus_copepodite i-iv` + `eucalanus_copepodite i-vi` + `eucalanus_copepodite iii-iv` + `eucalanus_copepodite v-vi` + `eucalanus_copepodite vi` + `eucalanus_immature (sexually), juvenile, or adult` + eucalanus_unstaged,
    `euchaeta acuta`                      = `euchaeta acuta_copepodite v-vi` + `euchaeta acuta_unstaged`,
    `euchaeta marina`                     = `euchaeta marina_copepodite` + `euchaeta marina_copepodite i-iv` + `euchaeta marina_copepodite ii` + `euchaeta marina_copepodite iii` + `euchaeta marina_copepodite iii-iv` + `euchaeta marina_copepodite iv` + `euchaeta marina_copepodite iv-v` + `euchaeta marina_copepodite v` + `euchaeta marina_copepodite v-vi` + `euchaeta marina_copepodite v-vi` + `euchaeta marina_copepodite vi` + `euchaeta marina_unstaged`,
    `euchaeta media`                      = `euchaeta media_copepodite v-vi` + `euchaeta media_unstaged`,
    `euchaeta spp.`                       = `euchaeta_copepodite i-iv` + `euchaeta_copepodite i-v` + `euchaeta_copepodite i-vi` + `euchaeta_copepodite vi` + euchaeta_unstaged,
    `euchirella amoena`                   = `euchirella amoena_unstaged`,
    `euchirella intermedia`               =  `euchirella intermedia_copepodite vi` + `euchirella intermedia_unstaged`,
    `euchirella pulchra`                  = `euchirella pulchra_unstaged`,
    `euchirella rostrata`                 = `euchirella rostrata_copepodite vi` + `euchirella rostrata_copepodite vi` + `euchirella rostrata_unstaged`,
    `euchirella spp.`                     = `euchirella_copepodite v` + `euchirella_copepodite v-vi` + `euchirella_copepodite vi`,
    `euphausiacea eggs`                   = euphausiacea_egg,
    `euphausiacea calyptosis`             = `euphausiacea_calyptopis (protozoea)`,
    `euphausiacea nauplii`                = euphausiacea_nauplius,
    `euphausiacea total`                  = euphausiacea_adult + `euphausiacea_copepodite iv` + `euphausiacea_post calyptopis`,
    `eurytemora americana`                = `eurytemora americana_copepodite vi` + `eurytemora americana_unstaged`,
    `eurytemora herdmani`                 = `eurytemora herdmani_unstaged`,
    `eurytemora spp.`                     = `eurytemora_copepodite i-iv` + `eurytemora_copepodite v-vi` + eurytemora_unstaged,
    `evadne spp.`                         = evadne_adult + `evadne_immature (sexually), juvenile, or adult` + evadne_unstaged,
    `farranula gracilis`                  = `farranula gracilis_adult` + `farranula gracilis_copepodite vi` + `farranula gracilis_unstaged`,
    `farranula spp.`                      = `farranula_copepodite vi`,
    `foraminiferida spp.`                 = foraminiferida_adult + foraminiferida_unstaged,
    `galatheoidea spp.`                   = galatheoidea_unstaged,
    `gammaridea spp.`                     = gammaridae_unstaged + gammaridea_adult + `gammaridea_immature (sexually), juvenile, or adult` + gammaridea_larva + gammaridea_unstaged,
    `gastropoda spp.`                     = gastropoda_egg + gastropoda_larva + gastropoda_veliger + gastropoda_unstaged,
    `gymnosomata spp.`                    = gymnosomata_unstaged,
    `halithalestris spp.`                 = `halithalestris croni_unstaged`,
    `harpacticoida spp.`                  = `harpacticoida_copepodite i-v` + `harpacticoida_copepodite i-vi` + `harpacticoida_copepodite vi` + `harpacticoida_immature (sexually), juvenile, or adult` + harpacticoida_unstaged,
    `heteropoda spp.`                     = heteropoda,
    `heterorhabdus papilliger`            = `heterorhabdus papilliger_unstaged`,
    `homarus americanus`                  = `homarus americanus_larva` + `homarus americanus_unstaged`,
    `hydrozoa spp.`                       = hydrozoa_medusa + hydrozoa_unstaged,
    `hyperiidae`                          = hyperiidae_unstaged + hyperiidea_adult + hyperiidea_egg + hyperiidea_larva + `hyperiidea_immature (sexually), juvenile, or adult` + hyperiidea_larva + hyperiidea_unstaged,
    `ishnocalanus plumulosus`             = `ischnocalanus plumulosus_copepodite v` + `ischnocalanus plumulosus_copepodite vi` + `ischnocalanus plumulosus_unstaged`,
    `isopoda spp.`                        = `isopoda_immature (sexually), juvenile, or adult` + isopoda_larva + isopoda_unstaged,
    `labidocera acutifrons`               = `labidocera acutifrons_copepodite vi`,
    `labidocera aestiva`                  = `labidocera aestiva_copepodite vi` + `labidocera aestiva_copepedite iii-v` + `labidocera aestiva_copepodite v-vi`,
    `labidocera nerii`                    = `labidocera nerii_unstaged`,
    `labidocera spp.`                     = `labidocera_copepodite ii-vi` + `labidocera_copepodite vi` + `labidocera_copepodite v-vi` + `labidocera_copepodite vi` + labidocera_unstaged,
    `lepas nauplii`                       = lepas_nauplius, 
    `lepas adult`                         = lepas_adult,
    `limacina retroversa`                 = `limacina retroversa_unstaged`,
    `limacina spp.`                       = limacina_unstaged,
    `lucicutia flavicornis`               = `lucicutia flavicornis_copepodite i-vi` + `lucicutia flavicornis_copepodite vi` + `lucicutia flavicornis_unstaged`,
    `lucicutia spp.`                      = `lucicutia_copepodite iv-vi` + `lucicutia flavicornis_copepodite vi` + `lucicutia_copepodite v-vi`,
    `lucifer typus`                       = `lucifer typus_copepodite i-v (pseudocalanus / copepodite i-vi (paracalanus)` + `lucifer typus_larva` + `lucifer typus_immature (sexually), juvenile, or adult` + `lucifer typus_larva` + `lucifer typus_molt`,
    `macrosetella gracilis`               = `macrosetella gracilis_adult` + `macrosetella gracilis_copepodite i-vi` + `macrosetella gracilis_copepodite vi` + `macrosetella gracilis_immature (sexually), juvenile, or adult` + `macrosetella gracilis_unstaged`,
    `mecynocera clausi`                   = `mecynocera clausi_copepodite vi` + `mecynocera clausi_copepodite v-vi` + `mecynocera clausi_immature (sexually), juvenile, or adult` + `mecynocera clausi_immature (sexually), juvenile, or adult`,
    `mesocalanus tenuicornis`             = `mesocalanus tenuicornis_copepodite vi` + `mesocalanus tenuicornis_copepodite i-iv` + `mesocalanus tenuicornis_copepodite v-vi` + `mesocalanus tenuicornis_unstaged`,
    `metridia longa`                      = `metridia longa_copepodite v-vi`,
    `metridia lucens`                     = `metridia lucens_copepodite i` + `metridia lucens_copepodite i-iv` + `metridia lucens_copepodite ii` + `metridia lucens_copepodite iii` + `metridia lucens_copepodite iv` + `metridia lucens_copepodite v` + `metridia lucens_copepodite v-vi` + `metridia lucens_copepodite vi` + `metridia lucens_immature (sexually), juvenile, or postlarva` + `metridia lucens_unstaged`,
    `metridia spp.`                       = `metridia_copepodite v` + `metridia_immature (sexually), juvenile, or adult`,
    `metridia i-iv`                       = `metridia_copepodite i-iv`,
    `microcalanus spp.`                   = `microcalanus_copepodite v-vi`,
    `microsetella rosea`                  = `microsetella rosea_unstaged` + `microsetella rosea_copepodite i-iv`,
    `microsetella spp.`                   = `microcalanus_copepodite v-vi` + `microsetella_copepodite i-vi`,
    `miracia efferata`                    = `miracia efferata_unstaged`,
    `mollusca spp.`                       = mollusca_veliger,
    `musca spp.`                          = musca_unstaged,
    `mysida spp.`                         = mysida_zoea + mysida_unstaged + `mysida_immature (sexually), juvenile, or adult` + mysida_larva,
    `nannocalanus minor`                  = `nannocalanus minor_copepodite` + `nannocalanus minor_copepodite i-iv` + `nannocalanus minor_copepodite iv` + `nannocalanus minor_copepodite v` + `nannocalanus minor_copepodite vi` + `nannocalanus minor_copepodite v-vi` + `nannocalanus minor_unstaged`,
    `nannocalanus spp.`                   = nannocalanus_unstaged,
    `nanomia cara`                        = `nanomia cara_unstaged`,
    `nemata spp.`                         = nemata_unstaged,
    `neocalanus gracilis`                 = `neocalanus gracilis_unstaged`,
    `neocalanus robustior`                = `neocalanus robustior_copepodite vi`,
    `neocalanus spp.`                     = neocalanus_unstaged,
    `oculosetella gracilis`               = `oculosetella gracilis_copepodite vi`,
    `oithona linearis`                    = `oithona linearis_unstaged`,
    `oithona similis`                     = `oithona similis_copepodite vi`,
    `oithona spp.`                        = `oithona_copepodite i-iv` + `oithona_copepodite i-v` + `oithona_copepodite iv-vi` + `oithona_copepodite vi` + `oithona_copepodite v-vi` + oithona_unstaged,
    `oncaea spp.`                         = oncaea_adult + `oncaea_copepodite iv-v` + `oncaea_copepodite vi` + oncaea_unstaged,
    `osteichthyes spp.`                   = osteichthyes_egg + osteichthyes_larva + `osteichthyes_immature (sexually), juvenile, or adult` + `osteichthyes_immature (sexually, or juvenile)` + osteichthyes_unstaged,
    `ostracoda spp.`                      = ostracoda_adult + `ostracoda_immature (sexually), juvenile, or adult` + ostracoda_unstaged,
    `oxycephalus clausi`                  = `oxycephalus clausi_unstaged`,
    `paedoclione doliiformis`             = `paedoclione doliiformis_unstaged`,
    `paracalanus aculeatus`               = `paracalanus aculeatus_adult`,
    `paracalanus or pseudocalanus`        = `paracalanus or pseudocalanus_copepodite` + `paracalanus or pseudocalanus_unstaged` + `paracalanus or pseudocalanus_copepodite i-v` + `paracalanus or pseudocalanus_copepodite i-v (pseudocalanus / copepodite i-vi (paracalanus)`,
    `paracalanus spp.`                    = `paracalanus_adult` + `paracalanus_copepodite` + `paracalanus_copepodite i-iv` + `paracalanus_copepodite iii-vi` + `paracalanus_copepodite iv` + `paracalanus_copepodite v-vi` +  `paracalanus_copepodite vi` + `paracalanus_unstaged`,
    `paracandacia bispinosa`              = `paracandacia bispinosa_copepodite v-vi` + `paracandacia bispinosa_copepodite vi` + `paracandacia bispinosa_unstaged`,
    `paraeuchaeta norvegica`              = `paraeuchaeta norvegica_copepodite iii-vi` + `paraeuchaeta norvegica_copepodite iv` + `paraeuchaeta norvegica_copepodite v` + `paraeuchaeta norvegica_copepodite v-vi` + `paraeuchaeta norvegica_nauplius`+ `paraeuchaeta norvegica_unstaged`,
    `penilia avirostris`                  = `penilia avirostris_immature (sexually), juvenile, or adult`  + `penilia avirostris_unstaged`,
    `penilia spp.`                        = `penilia_unstaged`,
    `phaennidae spp.`                     = phaennidae_unstaged,
    `pleuromamma abdominalis`             = `pleuromamma abdominalis_copepodite iv` + `pleuromamma abdominalis_copepodite v` + `pleuromamma abdominalis_copepodite v-vi` + `pleuromamma abdominalis_copepodite vi` ,
    `pleuromamma borealis`                = `pleuromamma borealis_copepodite v` + `pleuromamma borealis_copepodite vi`,
    `pleuromamma gracilis`                = `pleuromamma gracilis_copepodite v-vi` + `pleuromamma gracilis_copepodite vi`,
    `pleuromamma piseki`                  = `pleuromamma piseki_copepodite i-vi` + `pleuromamma piseki_copepodite v-vi` + `pleuromamma piseki_copepodite vi`,
    `pleuromamma quadrangulata`           = `pleuromamma quadrungulata_unstaged`,
    `pleuromamma robusta`                 = `pleuromamma robusta_copepodite vi` + `pleuromamma robusta_copepodite v-vi`,
    `pleuromamma xiphias`                 = `pleuromamma xiphias_copepodite v-vi`,
    `pleuromamma spp.`                    =  `pleuromamma_copepodite` + `pleuromamma borealis or pleuromamma gracilis_copepodite vi` + `pleuromamma borealis or pleuromamma gracilis_unstaged` + `pleuromamma_copepodite i-iv` + `pleuromamma_copepodite ii-vi` + `pleuromamma_copepodite iv-v` +  `pleuromamma_copepodite v-vi` + `pleuromamma_copepodite vi` + `pleuromamma_unstaged`,
    `pneumodermopsis paucidens`           = `pneumodermopsis paucidens_unstaged`,
    `podon spp.`                          = podon_adult + `podon_immature (sexually), juvenile, or adult` + podon_unstaged,
    `polychaeta larva`                    = polychaeta_larva,
    `polychaete`                          = polychaeta_adult + polychaeta_trochophore + polychaeta_nauplius + `polychaeta_immature (sexually), juvenile, or adult` + polychaeta_unstaged,
    `pontella spp.`                       = `pontella_copepodite iv-v` + `pontella_copepodite vi` + `pontella_copepedite iii-v` + `pontella_copepodite v-vi` + `pontella_copepodite vi`,
    `pontellidae spp.`                    = `pontellidae_copepodite vi` + pontellidae_unstaged,
    `pontellina plumata`                  = `pontellina plumata_copepodite vi`,
    `pontellopsis perspicax`              = `pontellopsis perspicax_copepodite vi` + `pontellopsis perspicax_unstaged`,
    `pseudocalanus adult`                 = pseudocalanus_adult,
    `pseudocalanus spp.`                  = `pseudocalanus_copepodite`  + `pseudocalanus_copepodite i-iv` + `pseudocalanus_copepodite ii` + `pseudocalanus_copepodite v` + `pseudocalanus_copepodite vi`,
    `pycnogonida spp.`                    = pycnogonida_unstaged,
    `radiolaria spp.`                     = radiolaria_unstaged,
    `rhincalanus cornutus`                = `rhincalanus cornutus_copepodite` + `rhincalanus cornutus_copepodite i-iv` + `rhincalanus cornutus_copepodite i-v` + `rhincalanus cornutus_copepodite i-vi` + `rhincalanus cornutus_copepodite iii` + `rhincalanus cornutus_copepodite v` + `rhincalanus cornutus_copepodite v-vi` + `rhincalanus cornutus_copepodite vi` + `rhincalanus cornutus_immature (sexually), juvenile, or adult` + `rhincalanus cornutus_unstaged`,
    `rhincalanus nasutus`                 = `rhincalanus nasutus_copepodite iv-v` + `rhincalanus nasutus_copepodite v-vi` + `rhincalanus nasutus_copepodite vi` + `rhincalanus nasutus_immature (sexually), juvenile, or adult` + `rhincalanus nasutus_unstaged`,
    `rhincalanus spp.`                    = `rhincalanus_copepedite iii-v` + `rhincalanus_copepodite i` + `rhincalanus_copepodite i-iv` + `rhincalanus_copepodite i-v` + `rhincalanus_copepodite iv-v` + `rhincalanus_copepodite v-vi` + `rhincalanus_copepodite vi` + `rhincalanus_nauplius` + `rhincalanus_unstaged`,
    `salpidae total`                      = salpidae_unstaged,
    `sapphirina spp.`                     = `sapphirina_copepodite i-vi` + `sapphirina_copepodite iv-vi` + `sapphirina_copepodite v` + `sapphirina_copepodite v-vi` + `sapphirina_copepodite vi` + `sapphirina_immature (sexually), juvenile, or adult` + `sapphirina_unstaged` + `sapphirinidae_unstaged`,
    `sarcodina spp.`                      = `sarcodina (not foraminiferida)_unstaged`,
    `scina stebbingi`                     = `scina stebbingi_unstaged`,
    `scolecithrix bradyi`                 = `scolecithrix bradyi_copepodite vi`,
    `scolecithrix danae`                  = `scolecithrix bradyi_copepodite vi` + `scolecithrix danae_calyptopis i` + `scolecithrix danae_copepodite i-v` + `scolecithrix danae_copepodite iv` + `scolecithrix danae_copepodite v` + `scolecithrix danae_copepodite v-vi` + `scolecithrix danae_copepodite vi` + `scolecithrix danae_unstaged`,
    `scolecithrix spp.`                   = scolecithrix_unstaged,
    `scolecithricella spp.`               = scolecithricella_unstaged,
    `scottocalanus securifrons`           = `scottocalanus securifrons_unstaged`,
    `sessilia spp.`                       = sessilia_nauplius + sessilia_cypris,
    `siphonophorae spp.`                  = siphonophorae_medusa + siphonophorae_unstaged,
    `siphonostomoida spp.`                = siphonostomatoida_unstaged,
    `sipuncula spp.`                      = sipuncula_unstaged,
    `stellate bodies`                     = `stellate bodies_unstaged`,
    `stomatopoda spp.`                    = stomatopoda_larva + stomatopoda_unstaged,
    `subeucalanus crassus`                = `subeucalanus crassus_copepodite v-vi`,
    `temora longicornis`                  = `temora longicornis_adult` + `temora longicornis_copepodite` + `temora longicornis_copepodite i` + `temora longicornis_copepodite i-iii` + `temora longicornis_copepodite i-iv` + `temora longicornis_copepodite ii` + `temora longicornis_copepodite iii` + `temora longicornis_copepodite iv` + `temora longicornis_copepodite iv-vi` + `temora longicornis_copepodite v` + `temora longicornis_copepodite v-vi` + `temora longicornis_copepodite vi` + `temora longicornis_immature (sexually), juvenile, or adult` + `temora longicornis_unstaged`,
    `temora stylifera`                    = `temora stylifera_adult` + `temora stylifera_copepodite` + `temora stylifera_copepodite i-iv` + `temora stylifera_copepodite iv-v` + `temora stylifera_copepodite v-vi` + `temora stylifera_copepodite vi` + `temora stylifera_immature (sexually), juvenile, or adult` + `temora stylifera_unstaged`, 
    `temora turbinata`                    = `temora turbinata_adult` + `temora turbinata_copepodite` + `temora turbinata_copepodite iv-vi` + `temora turbinata_copepodite v-vi` + `temora turbinata_copepodite vi` + `temora turbinata_immature (sexually), juvenile, or adult` + `temora turbinata_unstaged`, 
    `temora spp.`                         = `temora_copepodite` + `temora_copepodite i-iii` + `temora_copepodite i-iv` + `temora_unstaged`,  
    `thalia democratica`                  = `thalia democratica_immature (sexually), juvenile, or adult` + `thalia democratica_unstaged`,
    `thaliacea spp.`                      = thaliacea_adult + `thaliacea_immature (sexually), juvenile, or adult` + thaliacea_unstaged,
    `thecosomata spp.`                    = `thecosomata unknown # aeg_unstaged` + `thecosomata_immature (sexually), juvenile, or adult` + `thecosomata_unstaged`, 
    `tintinnidae spp.`                    = tintinnidae_adult + tintinnidae_unstaged,
    `tomopteris spp.`                     = tomopteris_unstaged,
    `tortanus discaudatus`                = `tortanus discaudatus_unstaged` + `tortanus discaudatus_copepodite i-iv` + `tortanus discaudatus_copepodite i-vi` + `tortanus discaudatus_copepodite v-vi` + `tortanus discaudatus_copepodite vi`,
    `undeuchaeta minor`                   = `undeuchaeta minor_unstaged`,
    `undeuchaeta plumosa`                 = `undeuchaeta plumosa_copepodite vi` + `undeuchaeta plumosa_unstaged` + `undeuchaeta plumosa_copepodite v-vi`,
    `undinula vulgaris`                   = `undinula vulgaris_copepodite` + `undinula vulgaris_copepodite i-iv` + `undinula vulgaris_copepodite iv` + `undinula vulgaris_copepodite v` + `undinula vulgaris_copepodite v-vi` + `undinula vulgaris_copepodite vi`,
    `unidentified plankton and fragments` = `unidentified plankton and fragments_unstaged`
    
  ) 


#Pull the station information
noaa_zoo_stations <- noaa_zoo_abundances[,1:10]

#Join with the new columns
noaa_zoo_2 <- bind_cols(noaa_zoo_stations, noaa_zoo_refined)


####__####

#### Converting SAHFOS counts to # per meter cubed

mc1_trav %>% count(year)
mc2_trav %>% count(year)

#MC1 and MC2 are non-overlapping periods so we can combine into one group
sahfos_trav <- bind_rows(mc1_trav, mc2_trav)
sahfos_eye <- bind_rows(mc1_eye, mc2_eye)
sahfos_phyto <- bind_rows(mc1_phyto, mc2_phyto)


#Conversions - 

# A. subsample count to full transect
#phyto 1/8000th of transect counted
#traverse 1/40th of transect counted
#eyecount full transect counted


#Question: are the counts already converted to the number expected for a full transect?
ggplot() +
  geom_point(data = mc1_trav, aes(factor(year), `calanus i-iv`, color = "traverse")) +
  geom_point(data = mc1_eye, aes(factor(year), `calanus finmarchicus`, color = "eyecount")) +
  geom_point(data = mc2_trav, aes(factor(year), `calanus i-iv`, color = "traverse")) +
  geom_point(data = mc2_eye, aes(factor(year), `calanus finmarchicus`, color = "eyecount")) +
  labs(x = NULL, y = "C. finmarchicus", title = "Calanus Check")

#Appears that way, would not expect to see >1000 on 1.25 square cm
# Conclusion all three (eye, phyto, traverse) are in numbers per transect




# B. transect to water volume
# 1 transect = 10 nautical miles
# CPR aperture dimensions = 1.27 cm square entrance
# aperture area in square meters = 0.00016129
# 1852 meters in nautical mile
# volume in square meters per 10cm silk = 0.2987091 meters^3

conversion_rate <- 1/.2987091

# strav <- sahfos_trav %>% select(11:ncol(sahfos_trav))
# seye <- sahfos_eye  %>% select(11:ncol(sahfos_eye))
# sphyto <- sahfos_phyto  %>% select(12:ncol(sahfos_phyto))
sahfos_abundances <- list("traverse" = sahfos_trav %>% select(11:ncol(sahfos_trav)), 
                          "eyecount" = sahfos_eye  %>% select(11:ncol(sahfos_eye)), 
                          "phyto"    = sahfos_phyto  %>% select(11:ncol(sahfos_phyto)))
sahfos_m3 <-  map(sahfos_abundances, function(x){
  x_meters_cubed <- x * conversion_rate
  return(x_meters_cubed)
})


strav %>% select(sort(current_vars())) %>% View("traverse conversion test")
noaa_zoo_abundances %>% select(12:ncol(noaa_zoo_abundances)) %>% select(sort(current_vars())) %>% View("noaa m3 abundances")

# #compare how they jump  
noaa_zoo_abundances %>% count(`calanus_copepodite i-iv`)
strav %>% count(`calanus i-iv`)

#### Combining Traverse and eyecount  ####
strav_m3 <- sahfos_m3$traverse
seye_m3 <- sahfos_m3$eye

#Dataframe comparisons
library(janitor)

#All have the same number of rows, but columns are present in some but not others
map(sahfos_m3, dim)
janitor::compare_df_cols(strav_m3, seye_m3)
janitor::compare_df_cols_same(strav_m3, seye_m3)


sahfos_eye %>% select(1:10) %>% 
  bind_cols(seye_m3) %>% 
  pivot_longer(names_to = "taxa", values_to = "abundance", cols = names(seye_m3)[1]:names(seye_m3)[ncol(seye_m3)])


#Idea, create a dataframe with names found in both, make the values the added contribution from one or both the subsample types
