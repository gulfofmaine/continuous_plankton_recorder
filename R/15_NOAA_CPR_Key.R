####  NOAAA CPR TAXA Key  ####
# Purpose: reduce the number of taxa and their different development stages found in NOAA CPR data
# down to a reduced number that reflects the distinctions mades by SAHFOS and also reflects which 
# taxa-stage namings were actually used in practice by the NOAA identification group

# NOAA dataset contains more complex/convoluted taxa-stage combinations
# This allows for more detailed information, if used consistently (it isn't)
# In order to pair with SAHFOS data you need to reduce to SAHFOS resolution

####  Packages  ####
library(tidyverse)


####  Data  ####
noaa_zoo_abundances <- read_csv("/Users/akemberling/Box/Climate Change Ecology Lab/Data/Gulf of Maine CPR/2019_data_processing/noaa_zoo_abundances_2019.csv")


####  Cleaner NOAA Consolidation  ####


####  1. Remove unused Taxa  ####
#These names are not found at all in our data
no_records <- noaa_zoo_abundances %>% 
  pivot_longer(names_to = "taxon_stage", values_to = "abundance", cols = 11:ncol(noaa_zoo_abundances)) %>% 
  group_by(taxon_stage) %>% 
  summarise(presence = ifelse(sum(abundance, na.rm = T) > 0, "present", "absent")) %>% 
  ungroup() %>% 
  filter(presence == "absent") %>% 
  pull(taxon_stage)


#Take them out
noaa_inuse <- noaa_zoo_abundances %>% select(-one_of(no_records))


####  2. Make Column Key  ####
#column names to allocate into list
sort(names(noaa_inuse)[11:ncol(noaa_inuse)])

# Key that we can make changes to more easily, more robust to future changes
# Easier to look up what columns went into the aggregation this way
new_taxa_key <- list(
  `acartia danae`                       = c("acartia danae_copepodite ii-vi"),
  
  `acartia longiremis`                  = c("acartia longiremis_copepodite ii-vi", 
                                            "acartia longiremis_unstaged"),
  
  `acartia spp.`                        = c("acartia_copepodite i-v (pseudocalanus / copepodite i-vi (paracalanus)", 
                                            "acartia_copepodite ii-vi"),
  
  `amphipoda spp.`                      =  c("amphipoda_unstaged"),
  
  `appendicularia spp.`                 =  c("appendicularia_unstaged"),
  
  `bivalvia spp.`                       =  c("bivalvia_larva"),
  
  `brachyura spp.`                      = c("bivalvia_larva",                                                                            
                                            "brachyura_nauplius"),
  
  `calanus finmarchicus i-iv`           = c("calanus_copepodite i-iv"),
  
  `calanus finmarchicus v-vi`           = c("calanus finmarchicus_copepodite v-vi"),
  
  `calanus helgolandicus`               = c("calanus helgolandicus_copepodite v-vi"),
  
  `calanus hyperboreus`                 = c("calanus hyperboreus_copepodite i-vi",
                                            "calanus hyperboreus_copepodite iii-vi"),
  
  `calocalanus spp.`                    = c("calocalanus_copepodite i-iv",                                                               
                                            "calocalanus_copepodite v-vi"),
  
  `candacia spp.`                       = c("candacia_copepodite iii-iv"),
  
  `cavoliniidae spp.`                   = c("cavoliniidae_unstaged"),
  
  `centropages hamatus`                 = c("centropages hamatus_copepodite iv-vi",                                                      
                                            "centropages hamatus_copepodite vi",                                                         
                                            "centropages hamatus_unstaged"),
  
  `centropages typicus`                 = c("centropages typicus_copepodite iv",                                                         
                                            "centropages typicus_copepodite iv-v",                                                       
                                            "centropages typicus_copepodite iv-vi",                                                      
                                            "centropages typicus_copepodite vi" ),
  
  `centropages spp.`                    = c("centropages_copepodite i-iv",                                                               
                                            "centropages_copepodite v-vi"),
  
  `chaetognatha eyecount`               = c("chaetognatha hpr eyecount_unstaged"),
  
  `chaetognatha traverse`               = c("chaetognatha hpr traverse_unstaged"),
  
  `cladocera spp.`                      = c("cladocera_unstaged"),
  
  `clausocalanus spp.`                  = c("clausocalanus_copepodite vi"),
  
  `clione spp.`                         = c("clione_immature (sexually, or juvenile)",                                                   
                                            "clione_unstaged" ),
  
  `copepoda spp.`                       = c("copepoda_copepodite i-v",                                                                   
                                            "copepoda_nauplius"),
  
  `cumacea spp.`                        = c("cumacea_unstaged"),
  
  `cyclopoida spp.`                     = c("cyclopoida_copepodite vi"),
  
  `decapoda spp.`                       = c("decapoda_larva"),
  
  `echinoderm larvae`                   = c("echinodermata_larva"),
  
  `ectoprocta cyphonautes`              = c("ectoprocta_cyphonautes"),
  
  `eucalanus spp.`                      = c("eucalanus_copepodite i-vi"),
  
  `euchaeta acuta`                      = c("euchaeta acuta_copepodite v-vi",                                                            
                                            "euchaeta acuta_unstaged"),
  `euchaeta marina`                     = c("euchaeta marina_copepodite i-iv",                                                           
                                            "euchaeta marina_copepodite v-vi",                                                           
                                            "euchaeta marina_unstaged" ),
  
  `euchaeta spp.`                       = c("euchaeta_copepodite vi"),
  
  `euchirella rostrata`                 = c("euchirella rostrata_copepodite v-vi"),
  
  `euphausiacea calyptosis`             = c("euphausiacea_calyptopis (protozoea)"),
  
  `euphausiacea nauplii`                = c("euphausiacea_nauplius"),
  
  `euphausiacea spp.`                  = c("euphausiacea_post calyptopis"),
  
  `eurytemora americana`                = c("eurytemora americana_copepodite vi",                                                        
                                            "eurytemora americana_unstaged"),
  
  `eurytemora herdmani`                 = c("eurytemora herdmani_unstaged"),
  
  `eurytemora spp.`                     = c("eurytemora_copepodite i-iv",                                                                
                                            "eurytemora_copepodite v-vi",                                                                
                                            "eurytemora_unstaged"),
  
  `evadne spp.`                         = c("evadne_unstaged"),
  
  `foraminiferida spp.`                 = c("foraminiferida_unstaged"),
  
  `gammaridea spp.`                     = c("gammaridae_unstaged"),
  
  `gastropoda spp.`                     = c("gastropoda_larva",                                                                          
                                            "gastropoda_unstaged" ),
  
  `gymnosomata spp.`                    = c("gymnosomata_unstaged"),
  
  `halithalestris spp.`                 = c("halithalestris croni_unstaged"),
  
  `harpacticoida spp.`                  = c("harpacticoida_copepodite i-vi"),
  
  `heteropoda spp.`                     = c("heteropoda"),
  
  `heterorhabdus papilliger`            = c("heterorhabdus papilliger_unstaged"),
  
  `homarus americanus`                  = c("homarus americanus_larva",                                                                  
                                            "homarus americanus_unstaged"),
  
  `hyperiidae`                          = c("hyperiidea_unstaged"),
  
  `isopoda spp.`                        = c("isopoda_larva",                                                                             
                                            "isopoda_unstaged"),
  
  `labidocera aestiva`                  = c("labidocera aestiva_copepodite vi"),
  
  `limacina spp.`                       = c("limacina_unstaged"),
  
  `lucifer typus`                       = c("lucifer typus_immature (sexually), juvenile, or adult"),
  
  `macrosetella gracilis`               = c("macrosetella gracilis_copepodite i-vi"),
  
  `mecynocera clausi`                   = c("mecynocera clausi_copepodite v-vi"),
  
  `metridia longa`                      = c("metridia longa_copepodite v-vi"),
  
  `metridia lucens`                     = c("metridia lucens_copepodite v-vi",                                                           
                                            "metridia lucens_unstaged"),
  
  `metridia i-iv`                       = c("metridia_copepodite i-iv"),
  
  `microsetella rosea`                  = c("microsetella rosea_copepodite i-iv",                                                        
                                            "microsetella rosea_unstaged"),
  
  `musca spp.`                          = c("musca_unstaged"),
  
  `mysida spp.`                         = c("mysida_unstaged"),
  
  `nannocalanus minor`                  = c("nannocalanus minor_copepodite v",                                                           
                                            "nannocalanus minor_copepodite v-vi",                                                        
                                            "nannocalanus minor_copepodite vi"),
  
  `nemata spp.`                         = c("nemata_unstaged"),
  
  `oithona spp.`                        = c("oithona_copepodite iv-vi"),
  
  `oncaea spp.`                         = c("oncaea_copepodite vi"),
  
  `ostracoda spp.`                      = c("ostracoda_immature (sexually), juvenile, or adult",
                                            "ostracoda_unstaged"),
  
  `paedoclione doliiformis`             = c("paedoclione doliiformis_unstaged"),
  
  `paracalanus or pseudocalanus`        = c("paracalanus or pseudocalanus_copepodite i-v",
                                            "paracalanus or pseudocalanus_copepodite i-v (pseudocalanus / copepodite i-vi (paracalanus)"),
  
  `paracalanus spp.`                    = c("paracalanus_copepodite vi",                                                                 
                                            "paracalanus_unstaged" ),
  
  `paraeuchaeta norvegica`              = c("paraeuchaeta norvegica_copepodite iii-vi",                                                  
                                            "paraeuchaeta norvegica_nauplius" ),
  
  `penilia avirostris`                  = c("penilia avirostris_unstaged"),
  
  `pleuromamma piseki`                  = c("pleuromamma piseki_copepodite i-vi",                                                        
                                            "pleuromamma piseki_copepodite v-vi"),
  
  `pleuromamma robusta`                 = c("pleuromamma robusta_copepodite v-vi"),
  
  `pleuromamma spp.`                    = c("pleuromamma_copepodite ii-vi",                                                              
                                            "pleuromamma_copepodite iv-v",                                                               
                                            "pleuromamma_copepodite vi",                                                                 
                                            "pleuromamma_unstaged"),
  
  `pneumodermopsis paucidens`           = c("pneumodermopsis paucidens_unstaged"),
  
  `podon spp.`                          = c("podon_unstaged"),
  
  `polychaeta larva`                    = c("polychaeta_larva"),
  
  `pseudocalanus spp.`                  = c("pseudocalanus_copepodite vi"),
  
  `pycnogonida spp.`                    = c("pycnogonida_unstaged"),
  
  `rhincalanus nasutus`                 = c("rhincalanus nasutus_copepodite v-vi",                                                       
                                            "rhincalanus nasutus_unstaged"),
  
  `rhincalanus spp.`                    = c("rhincalanus_copepodite v-vi",                                                               
                                            "rhincalanus_copepodite vi",                                                                 
                                            "rhincalanus_unstaged"),
  
  `sapphirina spp.`                     = c("sapphirina_copepodite i-vi"),
  
  `sarcodina spp.`                      = c("sarcodina (not foraminiferida)_unstaged"),
  
  `sessilia spp.`                       = c("sessilia_cypris", 
                                            "sessilia_nauplius"),
  
  `siphonostomoida spp.`                = c("siphonostomatoida_unstaged"),
  
  `sipuncula spp.`                      = c("sipuncula_unstaged"),
  
  `stellate bodies`                     = c("stellate bodies_unstaged"),
  
  `stomatopoda spp.`                    = c("stomatopoda_larva"),
  
  `temora longicornis`                  = c("temora longicornis_copepodite iv-vi",                                                       
                                            "temora longicornis_unstaged"),
  
  `temora turbinata`                    = c("temora turbinata_copepodite iv-vi"), 
  
  `thalia democratica`                  = c("thalia democratica_unstaged"),
  
  `thaliacea spp.`                      = c("thaliacea_unstaged"),
  
  `thecosomata spp.`                    = c("thecosomata_unstaged"), 
  
  `tintinnidae spp.`                    = c("tintinnidae_unstaged"),
  
  `tortanus discaudatus`                = c("tortanus discaudatus_copepodite v-vi",                                                      
                                            "tortanus discaudatus_copepodite vi"),
  
  `undeuchaeta plumosa`                 = c("undeuchaeta plumosa_copepodite v-vi"),
  
  `unidentified plankton and fragments` = c("unidentified plankton and fragments_unstaged")
)




####  3. Use  Key to Consolidate  ####

# Pull the station information
noaa_zoo_stations <- noaa_zoo_abundances[,1:10]

# Consolidate in use taxa using key
noaa_zoo_refined <- imap(new_taxa_key, function(x, y) {
  
  if(length(x) == 1) {
    taxa_new <- data.frame(y = select(noaa_zoo_abundances, one_of(x)))
    taxa_new[taxa_new == -9999] <- 0
    colnames(taxa_new)[1] <- y
  } 
  
  #need some way to  sum the columns by row element, not collapse to sum the column
  if(length(x) > 1) {
    
    mult_columns_df <- as.data.frame(matrix(nrow = nrow(noaa_zoo_abundances), ncol = length(x) + 1))
    
    for (i in 1:length(x)) {
      mult_columns_df[,i] <- select(noaa_zoo_abundances, one_of(x[i]))
    }
    
    #Fix any instances where -9999 was used to denote observed but not counted
    mult_columns_df[mult_columns_df == -9999] <- 0
    
    
    for (i in 1:nrow(mult_columns_df)) {
      mult_columns_df[i, length(x) + 1] <- sum(mult_columns_df[i, 1:length(x)], na.rm = T)
    }
    
    taxa_new <- data.frame(y = mult_columns_df[, length(x) + 1])
    colnames(taxa_new)[1] <- y
  }
  
  return(taxa_new)
  
}) %>% bind_cols()



#Join with the new columns
noaa_zoo_2 <- bind_cols(noaa_zoo_stations, noaa_zoo_refined)

#remove unnecesary objects for use when sourcing
rm(noaa_zoo_stations, no_records, noaa_inuse, noaa_zoo_refined)




####__####

####  Old Method  ####
# Manual Addition of columns
# Works, but assumes no transcription errors are made
# also includes columns that never have abundances, which makes the code cumbersome

#### Replaced by code in 15_NOAA_CPR_Key.R
# #Create new columns that reflect what they are in the SAHFOS data
# names(noaa_zoo_abundances) %>% sort() #this is the list of what they currently are
# 
# 
# 
# # Methodology:
# # Reduce to species or taxa spp. when positive species identification is not available
# # Specific stage groupings like Calanus Finmarchicus I-IV will be pulled out seperately if done so in SAHFOS data
# noaa_zoo_refined <- noaa_zoo_abundances %>% 
#   transmute(
#     `acartia danae`                       = `acartia danae_unstaged` + `acartia danae_copepodite ii-vi` + `acartia danae_copepodite vi`,
#     `acartia longiremis`                  = `acartia longiremis_copepodite ii-vi` + `acartia longiremis_unstaged`,
#     `acartia tonsa`                       = `acartia tonsa_unstaged`,
#     `acartia spp.`                        =  acartia_adult + acartia_unstaged + `acartia_copepodite i-iv` + `acartia_copepodite ii-vi` + `acartia_copepodite v-vi` + `acartia_copepodite vi`,
#     `aetideidae spp.`                     =  aetideidae_unstaged,
#     `amphipoda spp.`                      =  amphipoda_larva + amphipoda_unstaged,
#     `anomalocera spp.`                    = `anomalocera petersoni_copepodite vi` + anomalocera_unstaged,
#     `anomura spp.`                        =  anomura_unstaged,
#     `appendicularia spp.`                 =  appendicularia_larva + appendicularia_unstaged,
#     `atlanta spp.`                        = `atlanta_immature (sexually), juvenile, or adult`,
#     `atlantidae spp.`                     = `atlantidae_immature (sexually), juvenile, or adult` + atlantidae_unstaged,
#     `bivalvia spp.`                       =  bivalvia_larva + bivalvia_unstaged,
#     `brachyura spp.`                      = `brachyura_immature (sexually, or juvenile)` + brachyura_nauplius + `brachyura_megalopa (postlarva)` +brachyura_zoea,
#     `calanus finmarchicus i-iv`           = `calanus finmarchicus_copepodite i` + `calanus finmarchicus_copepodite ii` + `calanus finmarchicus_copepodite iii` + `calanus finmarchicus_copepodite iv` + `calanus finmarchicus_copepodite i-iv`,
#     `calanus finmarchicus v-vi`           = `calanus finmarchicus_copepodite v-vi` +  `calanus finmarchicus_copepodite v` + `calanus finmarchicus_copepodite vi` + `calanus finmarchicus_unstaged`,
#     `calanus i-iv`                        = `calanus_copepodite i-iv` + `calanus_copepodite v`,
#     `calanus v-vi unidentified`           = `calanus_copepodite vi`,
#     `calanus glacialis`                   = `calanus glacialis_copepodite v-vi` + `calanus glacialis_copepodite i-iv`,
#     `calanus helgolandicus`               = `calanus helgolandicus_copepodite v-vi`,
#     `calanus hyperboreus`                 = `calanus hyperboreus_copepodite iii-vi` + `calanus hyperboreus_copepodite i-vi`,
#     `calocalanus pavo`                    = `calocalanus pavo_unstaged` + `calocalanus pavo_copepodite vi`,
#     `calocalanus spp.`                    = calocalanus_adult + calocalanus_copepodite + `calocalanus_copepodite vi` + `calocalanus_copepodite v-vi` + `calocalanus_copepodite i-iv` + `calocalanus_immature (sexually), juvenile, or adult` + calocalanus_unstaged,
#     `candacia armata`                     = `candacia armata_copepodite i-iv` + `candacia armata_copepodite iii-iv` + `candacia armata_copepodite v-vi` + `candacia armata_immature (sexually), juvenile, or adult` + `candacia armata_copepodite vi`,
#     `candacia bipinnata`                  = `candacia bipinnata_copepodite vi` + `candacia bipinnata_unstaged`,
#     `candacia curta`                      = `candacia curta_copepodite v` + `candacia curta_copepodite vi` + `candacia curta_copepodite v-vi` + `candacia curta_unstaged`,
#     `candacia longimana`                  = `candacia longimana_unstaged`,
#     `candacia norvegica`                  = `candacia norvegica_unstaged`,
#     `candacia pachydactyla`               = `candacia pachydactyla_copepodite v`, + `candacia pachydactyla_copepodite i-iv` + `candacia pachydactyla_copepodite v-vi` + `candacia pachydactyla_copepodite vi` + `candacia pachydactyla_unstaged`,
#     `candacia paenelongimana`             = `candacia paenelongimana_copepodite vi` + `candacia paenelongimana_unstaged`,
#     `candacia simplex`                    = `candacia simplex_copepodite vi` + `candacia simplex_unstaged`,
#     `candacia spp.`                       = candacia_copepodite + `candacia_copepodite ii` + `candacia_copepodite i-iv` + `candacia_copepodite v` +  `candacia_copepodite vi` + `candacia_copepodite v-vi` + `candacia_copepodite iii-iv`,
#     `caprellidea spp.`                    = caprellidea_unstaged,
#     `caridea spp.`                        = caridea_larva,
#     `carinariidae spp.`                   = carinariidae_unstaged + `carinaria lamarcki_unstaged`,
#     `cavoliniidae spp.`                   = cavoliniidae_unstaged,
#     `centropages bradyi`                  = `centropages bradyi_adult` + `centropages bradyi_copepodite` + `centropages bradyi_copepodite i-iv` + `centropages bradyi_copepodite iv` + `centropages bradyi_copepodite iv-v` + `centropages bradyi_copepodite iv-vi` + `centropages bradyi_copepodite v` + `centropages bradyi_copepodite v-vi` + `centropages bradyi_copepodite vi` + `centropages bradyi_unstaged`,
#     `centropages furcatus`                = `centropages furcatus_adult` + `centropages furcatus_copepodite iv-vi` + `centropages furcatus_copepodite vi` + `centropages furcatus_egg` + `centropages furcatus_unstaged`,
#     `centropages hamatus`                 = `centropages hamatus_adult` + `centropages hamatus_copepodite iv` + `centropages hamatus_copepodite iv-vi` + `centropages hamatus_copepodite v` + `centropages hamatus_copepodite v-vi` + `centropages hamatus_copepodite vi` + `centropages hamatus_unstaged`,
#     `centropages typicus`                 = `centropages typicus_adult` + `centropages typicus_copepodite` + `centropages typicus_copepodite iii` + `centropages typicus_copepodite iv` + `centropages typicus_copepodite iv-v` + `centropages typicus_copepodite iv-vi` + `centropages typicus_copepodite v` + `centropages typicus_copepodite v-vi` + `centropages typicus_copepodite vi` + `centropages typicus_egg` + `centropages typicus_unstaged`,
#     `centropages violaceus`               = `centropages violaceus_copepodite vi` + `centropages violaceus_unstaged`,
#     `centropages spp.`                    = `centropages_copepodite i-iv` + `centropages_copepodite v-vi` + centropages_egg + centropages_larva,
#     `cephalopoda spp.`                    = cephalopoda_adult + cephalopoda_larva + cephalopoda_unstaged,
#     `chaetognatha eyecount`               = `chaetognatha hpr eyecount_unstaged`,
#     `chaetognatha traverse`               = `chaetognatha hpr traverse_adult` + `chaetognatha hpr traverse_unstaged` + chaetognatha_cyst,
#     `cladocera spp.`                      = cladocera_larva + cladocera_unstaged,
#     `clausocalanus spp.`                  = clausocalanus_adult + clausocalanus_copepodite + `clausocalanus_copepodite iv-v` + `clausocalanus_copepodite vi` + `clausocalanus_copepodite i-iv` + `clausocalanus_copepodite v-vi` + clausocalanus_unstaged,
#     `clione spp.`                         = `clione_immature (sexually, or juvenile)` + clione_unstaged,
#     `clytemnestra scutella`               = `clytemnestra scutellata_immature (sexually), juvenile, or adult` + `clytemnestra scutellata_unstaged`,
#     `clytemnestra sp`                     = `clytemnestra_copepodite v-vi` + `clytemnestra_copepodite vi` + `clytemnestra_immature (sexually), juvenile, or adult` + clytemnestra_unstaged,
#     `cnidaria spp.`                       = cnidaria_medusa + cnidaria_unstaged,
#     `copepod eggs`                        = copepoda_egg,
#     `copepod nauplii`                     = copepoda_nauplius,
#     `copepoda spp.`                       = copepoda_copepodite + `copepoda_copepodite i-iv` + `copepoda_copepodite i-v` + `copepoda_copepodite i-vi` + `copepoda_copepodite iv-v` + `copepoda_copepodite iv-vi` + `copepoda_copepodite v` + `copepoda_copepodite v` + `copepoda_copepodite vi` + `copepoda_immature (sexually, or juvenile)` + `copepoda_parva (postlarva)` + copepoda_unstaged,
#     `copilia mirabilis`                   = `copilia mirabilis_copepodite i-vi` + `copilia mirabilis_copepodite v-vi` + `copilia mirabilis_copepodite vi` + `copilia mirabilis_unstaged`,
#     `copilia quadrata`                    = `copilia quadrata_copepodite vi` + `copilia quadrata_unstaged`,
#     `copilia spp.`                        = copilia_unstaged + `copilia_copepodite vi` + `copilia_copepodite i-vi`,
#     `corycaeus spp.`                      = corycaeus_adult + `corycaeus_copepodite i-vi` + `corycaeus_copepodite vi` + `corycaeus_immature (sexually), juvenile, or postlarva` + corycaeus_unstaged,
#     `creseis spp.`                        = creseis_unstaged,
#     `crustacea spp.`                      = crustacea_unstaged,
#     `ctenophora spp.`                     = ctenophora_unstaged,
#     `cumacea spp.`                        = cumacea_unstaged,
#     `cyclopoida spp.`                     = `cyclopoida_copepodite vi`,
#     `decapoda spp.`                       = `decapoda-arthropoda (not brachyura)_larva` + decapoda_larva,
#     `diastylis rathkei`                   = `diastylis rathkei_unstaged`,
#     `diastylis spp.`                      = diastylis_unstaged,
#     `echinoderm larvae`                   = echinodermata_larva,
#     `echinoderm egg`                      = echinodermata_larva,
#     `echinoderm spp.`                     = `echinodermata_immature (sexually, or juvenile)` + echinodermata_unstaged,
#     `ectoprocta cyphonautes`              = ectoprocta_cyphonautes,
#     `ectoprocta spp.`                     = ectoprocta_larva,
#     `eucalanidae spp.`                    = eucalanidae_copepodite + `eucalanidae_copepodite i-iv` + eucalanidae_nauplius + eucalanidae_unstaged, 
#     `eucalanus spp.`                      = eucalanus_nauplius + `eucalanus_copepodite ii` + `eucalanus_copepodite v` + eucalanus_copepodite + `eucalanus_copepodite i-iv` + `eucalanus_copepodite i-vi` + `eucalanus_copepodite iii-iv` + `eucalanus_copepodite v-vi` + `eucalanus_copepodite vi` + `eucalanus_immature (sexually), juvenile, or adult` + eucalanus_unstaged,
#     `euchaeta acuta`                      = `euchaeta acuta_copepodite v-vi` + `euchaeta acuta_unstaged`,
#     `euchaeta marina`                     = `euchaeta marina_copepodite` + `euchaeta marina_copepodite i-iv` + `euchaeta marina_copepodite ii` + `euchaeta marina_copepodite iii` + `euchaeta marina_copepodite iii-iv` + `euchaeta marina_copepodite iv` + `euchaeta marina_copepodite iv-v` + `euchaeta marina_copepodite v` + `euchaeta marina_copepodite v-vi` + `euchaeta marina_copepodite v-vi` + `euchaeta marina_copepodite vi` + `euchaeta marina_unstaged`,
#     `euchaeta media`                      = `euchaeta media_copepodite v-vi` + `euchaeta media_unstaged`,
#     `euchaeta spp.`                       = `euchaeta_copepodite i-iv` + `euchaeta_copepodite i-v` + `euchaeta_copepodite i-vi` + `euchaeta_copepodite vi` + euchaeta_unstaged,
#     `euchirella amoena`                   = `euchirella amoena_unstaged`,
#     `euchirella intermedia`               =  `euchirella intermedia_copepodite vi` + `euchirella intermedia_unstaged`,
#     `euchirella pulchra`                  = `euchirella pulchra_unstaged`,
#     `euchirella rostrata`                 = `euchirella rostrata_copepodite vi` + `euchirella rostrata_copepodite vi` + `euchirella rostrata_unstaged`,
#     `euchirella spp.`                     = `euchirella_copepodite v` + `euchirella_copepodite v-vi` + `euchirella_copepodite vi`,
#     `euphausiacea eggs`                   = euphausiacea_egg,
#     `euphausiacea calyptosis`             = `euphausiacea_calyptopis (protozoea)`,
#     `euphausiacea nauplii`                = euphausiacea_nauplius,
#     `euphausiacea total`                  = euphausiacea_adult + `euphausiacea_copepodite iv` + `euphausiacea_post calyptopis`,
#     `eurytemora americana`                = `eurytemora americana_copepodite vi` + `eurytemora americana_unstaged`,
#     `eurytemora herdmani`                 = `eurytemora herdmani_unstaged`,
#     `eurytemora spp.`                     = `eurytemora_copepodite i-iv` + `eurytemora_copepodite v-vi` + eurytemora_unstaged,
#     `evadne spp.`                         = evadne_adult + `evadne_immature (sexually), juvenile, or adult` + evadne_unstaged,
#     `farranula gracilis`                  = `farranula gracilis_adult` + `farranula gracilis_copepodite vi` + `farranula gracilis_unstaged`,
#     `farranula spp.`                      = `farranula_copepodite vi`,
#     `foraminiferida spp.`                 = foraminiferida_adult + foraminiferida_unstaged,
#     `galatheoidea spp.`                   = galatheoidea_unstaged,
#     `gammaridea spp.`                     = gammaridae_unstaged + gammaridea_adult + `gammaridea_immature (sexually), juvenile, or adult` + gammaridea_larva + gammaridea_unstaged,
#     `gastropoda spp.`                     = gastropoda_egg + gastropoda_larva + gastropoda_veliger + gastropoda_unstaged,
#     `gymnosomata spp.`                    = gymnosomata_unstaged,
#     `halithalestris spp.`                 = `halithalestris croni_unstaged`,
#     `harpacticoida spp.`                  = `harpacticoida_copepodite i-v` + `harpacticoida_copepodite i-vi` + `harpacticoida_copepodite vi` + `harpacticoida_immature (sexually), juvenile, or adult` + harpacticoida_unstaged,
#     `heteropoda spp.`                     = heteropoda,
#     `heterorhabdus papilliger`            = `heterorhabdus papilliger_unstaged`,
#     `homarus americanus`                  = `homarus americanus_larva` + `homarus americanus_unstaged`,
#     `hydrozoa spp.`                       = hydrozoa_medusa + hydrozoa_unstaged,
#     `hyperiidae`                          = hyperiidae_unstaged + hyperiidea_adult + hyperiidea_egg + hyperiidea_larva + `hyperiidea_immature (sexually), juvenile, or adult` + hyperiidea_larva + hyperiidea_unstaged,
#     `ishnocalanus plumulosus`             = `ischnocalanus plumulosus_copepodite v` + `ischnocalanus plumulosus_copepodite vi` + `ischnocalanus plumulosus_unstaged`,
#     `isopoda spp.`                        = `isopoda_immature (sexually), juvenile, or adult` + isopoda_larva + isopoda_unstaged,
#     `labidocera acutifrons`               = `labidocera acutifrons_copepodite vi`,
#     `labidocera aestiva`                  = `labidocera aestiva_copepodite vi` + `labidocera aestiva_copepedite iii-v` + `labidocera aestiva_copepodite v-vi`,
#     `labidocera nerii`                    = `labidocera nerii_unstaged`,
#     `labidocera spp.`                     = `labidocera_copepodite ii-vi` + `labidocera_copepodite vi` + `labidocera_copepodite v-vi` + `labidocera_copepodite vi` + labidocera_unstaged,
#     `lepas nauplii`                       = lepas_nauplius, 
#     `lepas adult`                         = lepas_adult,
#     `limacina retroversa`                 = `limacina retroversa_unstaged`,
#     `limacina spp.`                       = limacina_unstaged,
#     `lucicutia flavicornis`               = `lucicutia flavicornis_copepodite i-vi` + `lucicutia flavicornis_copepodite vi` + `lucicutia flavicornis_unstaged`,
#     `lucicutia spp.`                      = `lucicutia_copepodite iv-vi` + `lucicutia flavicornis_copepodite vi` + `lucicutia_copepodite v-vi`,
#     `lucifer typus`                       = `lucifer typus_copepodite i-v (pseudocalanus / copepodite i-vi (paracalanus)` + `lucifer typus_larva` + `lucifer typus_immature (sexually), juvenile, or adult` + `lucifer typus_larva` + `lucifer typus_molt`,
#     `macrosetella gracilis`               = `macrosetella gracilis_adult` + `macrosetella gracilis_copepodite i-vi` + `macrosetella gracilis_copepodite vi` + `macrosetella gracilis_immature (sexually), juvenile, or adult` + `macrosetella gracilis_unstaged`,
#     `mecynocera clausi`                   = `mecynocera clausi_copepodite vi` + `mecynocera clausi_copepodite v-vi` + `mecynocera clausi_immature (sexually), juvenile, or adult` + `mecynocera clausi_immature (sexually), juvenile, or adult`,
#     `mesocalanus tenuicornis`             = `mesocalanus tenuicornis_copepodite vi` + `mesocalanus tenuicornis_copepodite i-iv` + `mesocalanus tenuicornis_copepodite v-vi` + `mesocalanus tenuicornis_unstaged`,
#     `metridia longa`                      = `metridia longa_copepodite v-vi`,
#     `metridia lucens`                     = `metridia lucens_copepodite i` + `metridia lucens_copepodite i-iv` + `metridia lucens_copepodite ii` + `metridia lucens_copepodite iii` + `metridia lucens_copepodite iv` + `metridia lucens_copepodite v` + `metridia lucens_copepodite v-vi` + `metridia lucens_copepodite vi` + `metridia lucens_immature (sexually), juvenile, or postlarva` + `metridia lucens_unstaged`,
#     `metridia spp.`                       = `metridia_copepodite v` + `metridia_immature (sexually), juvenile, or adult`,
#     `metridia i-iv`                       = `metridia_copepodite i-iv`,
#     `microcalanus spp.`                   = `microcalanus_copepodite v-vi`,
#     `microsetella rosea`                  = `microsetella rosea_unstaged` + `microsetella rosea_copepodite i-iv`,
#     `microsetella spp.`                   = `microcalanus_copepodite v-vi` + `microsetella_copepodite i-vi`,
#     `miracia efferata`                    = `miracia efferata_unstaged`,
#     `mollusca spp.`                       = mollusca_veliger,
#     `musca spp.`                          = musca_unstaged,
#     `mysida spp.`                         = mysida_zoea + mysida_unstaged + `mysida_immature (sexually), juvenile, or adult` + mysida_larva,
#     `nannocalanus minor`                  = `nannocalanus minor_copepodite` + `nannocalanus minor_copepodite i-iv` + `nannocalanus minor_copepodite iv` + `nannocalanus minor_copepodite v` + `nannocalanus minor_copepodite vi` + `nannocalanus minor_copepodite v-vi` + `nannocalanus minor_unstaged`,
#     `nannocalanus spp.`                   = nannocalanus_unstaged,
#     `nanomia cara`                        = `nanomia cara_unstaged`,
#     `nemata spp.`                         = nemata_unstaged,
#     `neocalanus gracilis`                 = `neocalanus gracilis_unstaged`,
#     `neocalanus robustior`                = `neocalanus robustior_copepodite vi`,
#     `neocalanus spp.`                     = neocalanus_unstaged,
#     `oculosetella gracilis`               = `oculosetella gracilis_copepodite vi`,
#     `oithona linearis`                    = `oithona linearis_unstaged`,
#     `oithona similis`                     = `oithona similis_copepodite vi`,
#     `oithona spp.`                        = `oithona_copepodite i-iv` + `oithona_copepodite i-v` + `oithona_copepodite iv-vi` + `oithona_copepodite vi` + `oithona_copepodite v-vi` + oithona_unstaged,
#     `oncaea spp.`                         = oncaea_adult + `oncaea_copepodite iv-v` + `oncaea_copepodite vi` + oncaea_unstaged,
#     `osteichthyes spp.`                   = osteichthyes_egg + osteichthyes_larva + `osteichthyes_immature (sexually), juvenile, or adult` + `osteichthyes_immature (sexually, or juvenile)` + osteichthyes_unstaged,
#     `ostracoda spp.`                      = ostracoda_adult + `ostracoda_immature (sexually), juvenile, or adult` + ostracoda_unstaged,
#     `oxycephalus clausi`                  = `oxycephalus clausi_unstaged`,
#     `paedoclione doliiformis`             = `paedoclione doliiformis_unstaged`,
#     `paracalanus aculeatus`               = `paracalanus aculeatus_adult`,
#     `paracalanus or pseudocalanus`        = `paracalanus or pseudocalanus_copepodite` + `paracalanus or pseudocalanus_unstaged` + `paracalanus or pseudocalanus_copepodite i-v` + `paracalanus or pseudocalanus_copepodite i-v (pseudocalanus / copepodite i-vi (paracalanus)`,
#     `paracalanus spp.`                    = `paracalanus_adult` + `paracalanus_copepodite` + `paracalanus_copepodite i-iv` + `paracalanus_copepodite iii-vi` + `paracalanus_copepodite iv` + `paracalanus_copepodite v-vi` +  `paracalanus_copepodite vi` + `paracalanus_unstaged`,
#     `paracandacia bispinosa`              = `paracandacia bispinosa_copepodite v-vi` + `paracandacia bispinosa_copepodite vi` + `paracandacia bispinosa_unstaged`,
#     `paraeuchaeta norvegica`              = `paraeuchaeta norvegica_copepodite iii-vi` + `paraeuchaeta norvegica_copepodite iv` + `paraeuchaeta norvegica_copepodite v` + `paraeuchaeta norvegica_copepodite v-vi` + `paraeuchaeta norvegica_nauplius`+ `paraeuchaeta norvegica_unstaged`,
#     `penilia avirostris`                  = `penilia avirostris_immature (sexually), juvenile, or adult`  + `penilia avirostris_unstaged`,
#     `penilia spp.`                        = `penilia_unstaged`,
#     `phaennidae spp.`                     = phaennidae_unstaged,
#     `pleuromamma abdominalis`             = `pleuromamma abdominalis_copepodite iv` + `pleuromamma abdominalis_copepodite v` + `pleuromamma abdominalis_copepodite v-vi` + `pleuromamma abdominalis_copepodite vi` ,
#     `pleuromamma borealis`                = `pleuromamma borealis_copepodite v` + `pleuromamma borealis_copepodite vi`,
#     `pleuromamma gracilis`                = `pleuromamma gracilis_copepodite v-vi` + `pleuromamma gracilis_copepodite vi`,
#     `pleuromamma piseki`                  = `pleuromamma piseki_copepodite i-vi` + `pleuromamma piseki_copepodite v-vi` + `pleuromamma piseki_copepodite vi`,
#     `pleuromamma quadrangulata`           = `pleuromamma quadrungulata_unstaged`,
#     `pleuromamma robusta`                 = `pleuromamma robusta_copepodite vi` + `pleuromamma robusta_copepodite v-vi`,
#     `pleuromamma xiphias`                 = `pleuromamma xiphias_copepodite v-vi`,
#     `pleuromamma spp.`                    =  `pleuromamma_copepodite` + `pleuromamma borealis or pleuromamma gracilis_copepodite vi` + `pleuromamma borealis or pleuromamma gracilis_unstaged` + `pleuromamma_copepodite i-iv` + `pleuromamma_copepodite ii-vi` + `pleuromamma_copepodite iv-v` +  `pleuromamma_copepodite v-vi` + `pleuromamma_copepodite vi` + `pleuromamma_unstaged`,
#     `pneumodermopsis paucidens`           = `pneumodermopsis paucidens_unstaged`,
#     `podon spp.`                          = podon_adult + `podon_immature (sexually), juvenile, or adult` + podon_unstaged,
#     `polychaeta larva`                    = polychaeta_larva,
#     `polychaete`                          = polychaeta_adult + polychaeta_trochophore + polychaeta_nauplius + `polychaeta_immature (sexually), juvenile, or adult` + polychaeta_unstaged,
#     `pontella spp.`                       = `pontella_copepodite iv-v` + `pontella_copepodite vi` + `pontella_copepedite iii-v` + `pontella_copepodite v-vi` + `pontella_copepodite vi`,
#     `pontellidae spp.`                    = `pontellidae_copepodite vi` + pontellidae_unstaged,
#     `pontellina plumata`                  = `pontellina plumata_copepodite vi`,
#     `pontellopsis perspicax`              = `pontellopsis perspicax_copepodite vi` + `pontellopsis perspicax_unstaged`,
#     `pseudocalanus adult`                 = pseudocalanus_adult,
#     `pseudocalanus spp.`                  = `pseudocalanus_copepodite`  + `pseudocalanus_copepodite i-iv` + `pseudocalanus_copepodite ii` + `pseudocalanus_copepodite v` + `pseudocalanus_copepodite vi`,
#     `pycnogonida spp.`                    = pycnogonida_unstaged,
#     `radiolaria spp.`                     = radiolaria_unstaged,
#     `rhincalanus cornutus`                = `rhincalanus cornutus_copepodite` + `rhincalanus cornutus_copepodite i-iv` + `rhincalanus cornutus_copepodite i-v` + `rhincalanus cornutus_copepodite i-vi` + `rhincalanus cornutus_copepodite iii` + `rhincalanus cornutus_copepodite v` + `rhincalanus cornutus_copepodite v-vi` + `rhincalanus cornutus_copepodite vi` + `rhincalanus cornutus_immature (sexually), juvenile, or adult` + `rhincalanus cornutus_unstaged`,
#     `rhincalanus nasutus`                 = `rhincalanus nasutus_copepodite iv-v` + `rhincalanus nasutus_copepodite v-vi` + `rhincalanus nasutus_copepodite vi` + `rhincalanus nasutus_immature (sexually), juvenile, or adult` + `rhincalanus nasutus_unstaged`,
#     `rhincalanus spp.`                    = `rhincalanus_copepedite iii-v` + `rhincalanus_copepodite i` + `rhincalanus_copepodite i-iv` + `rhincalanus_copepodite i-v` + `rhincalanus_copepodite iv-v` + `rhincalanus_copepodite v-vi` + `rhincalanus_copepodite vi` + `rhincalanus_nauplius` + `rhincalanus_unstaged`,
#     `salpidae total`                      = salpidae_unstaged,
#     `sapphirina spp.`                     = `sapphirina_copepodite i-vi` + `sapphirina_copepodite iv-vi` + `sapphirina_copepodite v` + `sapphirina_copepodite v-vi` + `sapphirina_copepodite vi` + `sapphirina_immature (sexually), juvenile, or adult` + `sapphirina_unstaged` + `sapphirinidae_unstaged`,
#     `sarcodina spp.`                      = `sarcodina (not foraminiferida)_unstaged`,
#     `scina stebbingi`                     = `scina stebbingi_unstaged`,
#     `scolecithrix bradyi`                 = `scolecithrix bradyi_copepodite vi`,
#     `scolecithrix danae`                  = `scolecithrix bradyi_copepodite vi` + `scolecithrix danae_calyptopis i` + `scolecithrix danae_copepodite i-v` + `scolecithrix danae_copepodite iv` + `scolecithrix danae_copepodite v` + `scolecithrix danae_copepodite v-vi` + `scolecithrix danae_copepodite vi` + `scolecithrix danae_unstaged`,
#     `scolecithrix spp.`                   = scolecithrix_unstaged,
#     `scolecithricella spp.`               = scolecithricella_unstaged,
#     `scottocalanus securifrons`           = `scottocalanus securifrons_unstaged`,
#     `sessilia spp.`                       = sessilia_nauplius + sessilia_cypris,
#     `siphonophorae spp.`                  = siphonophorae_medusa + siphonophorae_unstaged,
#     `siphonostomoida spp.`                = siphonostomatoida_unstaged,
#     `sipuncula spp.`                      = sipuncula_unstaged,
#     `stellate bodies`                     = `stellate bodies_unstaged`,
#     `stomatopoda spp.`                    = stomatopoda_larva + stomatopoda_unstaged,
#     `subeucalanus crassus`                = `subeucalanus crassus_copepodite v-vi`,
#     `temora longicornis`                  = `temora longicornis_adult` + `temora longicornis_copepodite` + `temora longicornis_copepodite i` + `temora longicornis_copepodite i-iii` + `temora longicornis_copepodite i-iv` + `temora longicornis_copepodite ii` + `temora longicornis_copepodite iii` + `temora longicornis_copepodite iv` + `temora longicornis_copepodite iv-vi` + `temora longicornis_copepodite v` + `temora longicornis_copepodite v-vi` + `temora longicornis_copepodite vi` + `temora longicornis_immature (sexually), juvenile, or adult` + `temora longicornis_unstaged`,
#     `temora stylifera`                    = `temora stylifera_adult` + `temora stylifera_copepodite` + `temora stylifera_copepodite i-iv` + `temora stylifera_copepodite iv-v` + `temora stylifera_copepodite v-vi` + `temora stylifera_copepodite vi` + `temora stylifera_immature (sexually), juvenile, or adult` + `temora stylifera_unstaged`, 
#     `temora turbinata`                    = `temora turbinata_adult` + `temora turbinata_copepodite` + `temora turbinata_copepodite iv-vi` + `temora turbinata_copepodite v-vi` + `temora turbinata_copepodite vi` + `temora turbinata_immature (sexually), juvenile, or adult` + `temora turbinata_unstaged`, 
#     `temora spp.`                         = `temora_copepodite` + `temora_copepodite i-iii` + `temora_copepodite i-iv` + `temora_unstaged`,  
#     `thalia democratica`                  = `thalia democratica_immature (sexually), juvenile, or adult` + `thalia democratica_unstaged`,
#     `thaliacea spp.`                      = thaliacea_adult + `thaliacea_immature (sexually), juvenile, or adult` + thaliacea_unstaged,
#     `thecosomata spp.`                    = `thecosomata unknown # aeg_unstaged` + `thecosomata_immature (sexually), juvenile, or adult` + `thecosomata_unstaged`, 
#     `tintinnidae spp.`                    = tintinnidae_adult + tintinnidae_unstaged,
#     `tomopteris spp.`                     = tomopteris_unstaged,
#     `tortanus discaudatus`                = `tortanus discaudatus_unstaged` + `tortanus discaudatus_copepodite i-iv` + `tortanus discaudatus_copepodite i-vi` + `tortanus discaudatus_copepodite v-vi` + `tortanus discaudatus_copepodite vi`,
#     `undeuchaeta minor`                   = `undeuchaeta minor_unstaged`,
#     `undeuchaeta plumosa`                 = `undeuchaeta plumosa_copepodite vi` + `undeuchaeta plumosa_unstaged` + `undeuchaeta plumosa_copepodite v-vi`,
#     `undinula vulgaris`                   = `undinula vulgaris_copepodite` + `undinula vulgaris_copepodite i-iv` + `undinula vulgaris_copepodite iv` + `undinula vulgaris_copepodite v` + `undinula vulgaris_copepodite v-vi` + `undinula vulgaris_copepodite vi`,
#     `unidentified plankton and fragments` = `unidentified plankton and fragments_unstaged`
#     
#   ) 
# 
# 
# #Pull the station information
# noaa_zoo_stations <- noaa_zoo_abundances[,1:10]
# 
# #Join with the new columns
# noaa_zoo_2 <- bind_cols(noaa_zoo_stations, noaa_zoo_refined)
