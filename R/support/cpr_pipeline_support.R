####  CPR Pipeline Functions
# How to get from raw data to analysis sets
# pipeline tracked using {targets}


####  Packages  ####
library(here)
library(mgcv)
library(gmRi)
library(readxl)
library(sf)
library(tidyverse)



####  Constants  ####
# Base path
ccel_boxpath <- "/Users/akemberling/Box/Climate Change Ecology Lab"
gom_cpr_path <- str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/")

#### NOAA Taxa Key  ####
# Used to resolve inconsistent groups in noaa GOM taxonomic groups
noaa_gom_taxa_key <- list( 
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
                           
                           `calanus i-iv`                        = c("calanus_copepodite i-iv"),
                           
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
                           
                           `hyperiidea spp.`                          = c("hyperiidea_unstaged"),
                           
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
                           
                           `para-pseudocalanus spp.`             = c("paracalanus or pseudocalanus_copepodite i-v",
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
####  End NOAA Key  ####


####__####
####  NOAA GOM Data Data  ####
# takes the header and reformats it so the taxxa stages become new column names

# The column Key lets us make changes to more easily, more robust to future changes
# Easier to look up what columns went into the aggregation this way
# though its a lot of vertical text here
import_noaa_cpr <- function(sample_type = c("phyto", "zoo"), return_option = c("abundances", "key")){
  
  
  ## Zooplankton Import
  if(sample_type == "phyto"){
    noaa_phyto <- readxl::read_xlsx(str_c(gom_cpr_path, "NOAA_1961-2013/Gulf of Maine CPR (Feb 14, 2014 update).xlsx"),
                                    skip = 2,
                                    sheet = 1)
  
  
    #Pull data out from the header
    noaa_phyto_abundances <- noaa_phyto[2:nrow(noaa_phyto),]
    
    #Fix the names
    phyto_group_stage_names <- rep(NA, ncol(noaa_phyto) - 10)
    for (i in 1:(ncol(noaa_phyto)-10)) {
      phyto_group_stage_names[i] <- names(noaa_phyto[i + 10])                                 #Just the names of the taxa
      phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "'")           #Drop 's
      phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], " \\d")        #Drop first digit
      phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\d")         #Drop second digit
      phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\d")         #Drop third digit
      phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\.")         #Drop first period
      phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\.")         #Drop second period
      phyto_group_stage_names[i] <- str_remove_all(phyto_group_stage_names[i], "\\.")         #Drop third period
      phyto_group_stage_names[i] <- str_replace_all(phyto_group_stage_names[i], " _", "_")    #Drop space before _
    }
    head(phyto_group_stage_names)
    
    #Replace old names with correct names
    names(noaa_phyto_abundances)[11:ncol(noaa_phyto_abundances)] <- phyto_group_stage_names
    names(noaa_phyto_abundances) <- str_replace_all(names(noaa_phyto_abundances), "'", "")
    head(names(noaa_phyto_abundances), 12)
    
    #Fix header, and make taxon key
    noaa_phyto_header <- noaa_phyto[1, 10:ncol(noaa_phyto)]
    names(noaa_phyto_header) <- c("code_type", phyto_group_stage_names)
    head(noaa_phyto_header, 10)
    
    #Reshape Taxon Key
    noaa_phyto_key <- noaa_phyto_header %>%
      pivot_longer(cols = -code_type, names_to = "Taxon Name", values_to = "Accepted ID") %>% 
      arrange(`Taxon Name`) %>% 
      distinct(`Taxon Name`, .keep_all = T)
    
    ####  Repair MARMAP Key  
    #MARMAP code key: https://www.nefsc.noaa.gov/nefsc/Narragansett/taxcodesA.html
    noaa_phyto_key <- noaa_phyto_key %>% 
      pivot_wider(names_from = code_type, values_from = `Accepted ID`) %>% 
      rename_all(tolower)
    
    #Repair taxon names using NEFSC marmap key
    which(noaa_phyto[1,] == 9101) #Index for Trichodesmium
    which(noaa_phyto[1,] == 9169) #Index for Ceratium ranipes
    colnames(noaa_phyto_abundances)[which(noaa_phyto[1,] == 9101)] <- "Trichodesmium"
    colnames(noaa_phyto_abundances)[which(noaa_phyto[1,] == 9169)] <- "Ceratium ranipes"
    
    #Repair key using NEFSC marmap key
    noaa_phyto_key <- noaa_phyto_key %>% mutate(
      `taxon name` = ifelse(`marmap code:` == 9169, "Ceratium ranipes", `taxon name`),
      `taxon name` = ifelse(`marmap code:` == 9101, "Trichodesmium", `taxon name`))
    
    
    ####  Prep for Export  
    noaa_phyto_abundances <- noaa_phyto_abundances %>%  
      rename_all(tolower)  %>% 
      mutate(cruise = str_replace_all(cruise, "'", ""))
    
    out_options <- list(
      "abundances" = noaa_phyto_abundances,
      "key"        = noaa_phyto_key 
    )
    
    return(out_options[[return_option]])
  
  
  }
  
  # Import controls for zooplankton
  if(sample_type == "zoo") {
    
    # Load zooplankton file
    noaa_zoo <- readxl::read_xlsx(str_c(gom_cpr_path, "NOAA_1961-2013/Gulf of Maine CPR (Feb 14, 2014 update).xlsx"),
                                  skip = 2,
                                  sheet = 2)
    
    #Pull data out from under the header
    noaa_zoo_abundances <- noaa_zoo[4:nrow(noaa_zoo),]
    
    #Fix names, there's 10 columns before we get to the zooplankton groups and stages
    group_stage_names <- rep(NA, ncol(noaa_zoo) - 10)
    for (i in 1:(ncol(noaa_zoo)-10)) {
      group_stage_names[i] <- str_c(names(noaa_zoo[i + 10]), noaa_zoo[1, i + 10], sep = "_")
      group_stage_names[i] <- str_remove_all(group_stage_names[i], "'")           #Drop 's
      group_stage_names[i] <- str_remove_all(group_stage_names[i], " \\d")        #Drop first digit
      group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\d")         #Drop second digit
      group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\d")         #Drop third digit
      group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\.")         #Drop first period
      group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\.")         #Drop second period
      group_stage_names[i] <- str_remove_all(group_stage_names[i], "\\.")         #Drop third period
      group_stage_names[i] <- str_replace_all(group_stage_names[i], " _", "_")    #Drop space before _
    }
    head(group_stage_names)
    
    #Replace old names with correct names
    names(noaa_zoo_abundances)[11:ncol(noaa_zoo_abundances)] <- group_stage_names
    names(noaa_zoo_abundances) <- str_replace_all(names(noaa_zoo_abundances), "'", "")
    head(names(noaa_zoo_abundances), 12)
    
    #Fix header, and make taxon key
    noaa_zoo_header <- noaa_zoo[2:3, 10:ncol(noaa_zoo)]
    names(noaa_zoo_header) <- c("code_type", group_stage_names)
    head(names(noaa_zoo_header), 10)
    
    #Reshape Taxon Key
    noaa_zoo_key <- noaa_zoo_header %>% 
      pivot_longer(cols = -code_type, names_to = "Taxon Name", values_to = "Accepted ID") %>% 
      arrange(`Taxon Name`) %>% 
      distinct(`Taxon Name`, .keep_all = T)
    
    
    ####  Repair MARMAP Key 
    noaa_zoo_key <- noaa_zoo_key %>% 
      pivot_wider(names_from = code_type, values_from = `Accepted ID`) %>% 
      rename_all(tolower)
    

    #Repair taxon names that are incorrect using NEFSC marmap key
    which(noaa_zoo[2,] == 3300) #Index for Heteropoda
    which(noaa_zoo[2,] == 4039) #Index for something...or nothing...
    colnames(noaa_zoo_abundances)[which(noaa_zoo[2,] == 3300)] <- "Heteropoda"
    colnames(noaa_zoo_abundances)[which(noaa_zoo[2,] == 4039)] <- "No record"
    
    #Drop column that has no taxa match for the marmap code 
    noaa_zoo_abundances[,"No record"] <- NULL
    
    #Repair key using NEFSC marmap key
    noaa_zoo_key <- noaa_zoo_key %>% mutate(
      `taxon name` = ifelse(`marmap taxonomic code:` == 3300, "Heteropoda", `taxon name`),
      `taxon name` = ifelse(`marmap taxonomic code:` == 4039, "No record", `taxon name`)) %>% 
      filter(`taxon name` != "No record")
    
    #Seperate taxa and stage
    noaa_zoo_key <- noaa_zoo_key %>% mutate(
      taxa  = str_extract(`taxon name`, "[^_]*"),
      stage = str_extract(`taxon name`, "_.*"),
      stage = str_replace(stage, "_", ""))
    
    
    ####  Prep for Export  
    noaa_zoo_abundances <- noaa_zoo_abundances %>%  
      rename_all(tolower) %>% 
      mutate(cruise = str_replace_all(cruise, "'", ""))
    
    
    out_options <- list(
      "abundances" = noaa_zoo_abundances,
      "key"        = noaa_zoo_key 
    )
    
    return(out_options[[return_option]])
  }
  
} # Close import_noaa_cpr




####__####
####  SAHFOS Data Import  ####


####  Cleanup function for MC datasets



# Build taxa key from headers for formatting sahfos data
sahfos_taxa_key <- function(mc_option = "mc1"){
  
  if(mc_option == "mc1"){
    #MC part 1
    mc1_phyto   <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part1.xlsx"), sheet = 1)
    mc1_eye     <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part1.xlsx"), sheet = 2)
    mc1_trav    <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part1.xlsx"), sheet = 3)
    
    # Combining Taxon Keys for phytoplankton, eye, and trav groups
    mc1_t_phyto <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part1.xlsx"), skip = 1, sheet = 5) %>% 
      mutate(taxa_class = "phyto") %>% 
      mutate(note = NA)
    mc1_t_trav  <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part1.xlsx"), skip = 1, sheet = 6) %>% 
      rename(note = 3) %>% 
      mutate(taxa_class = "trav")
    mc1_t_eye   <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part1.xlsx"), skip = 1, sheet = 7) %>% 
      rename(note = 3) %>% 
      mutate(taxa_class = "eye")
    
    #Combine Taxa Key
    mc1_taxa <- bind_rows(mc1_t_phyto, mc1_t_trav, mc1_t_eye) %>%
      arrange(taxa_class, `Taxon Name`) %>% 
      select(taxa_class, `Taxon Name`, `Accepted ID`, note)
    
    # tidy up
    rm(mc1_t_phyto, mc1_t_trav, mc1_t_eye)
    
    return(mc1_taxa)
  }
  
  
  # MC2 Table Taxa Key
  if(mc_option == "mc2"){
    
    
    mc2_phyto <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part 2.xlsx"), sheet = 1)
    mc2_eye   <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part 2.xlsx"), sheet = 2)
    mc2_trav  <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part 2.xlsx"), sheet = 3)
    
    # Combining Taxon Keys for phytoplankton, eye, and trav groups
    mc2_t_phyto   <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part 2.xlsx"), 
                                       sheet = 4, skip = 1) %>% 
      select(`Taxon Name` = 2, `Accepted ID` = 1, note = 5) %>% 
      mutate(taxa_class = "phyto") %>% 
      filter(is.na(`Taxon Name`) == FALSE)
    
    mc2_t_trav   <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part 2.xlsx"), 
                                      sheet = 4, skip = 1) %>% 
      select(`Taxon Name` = 8, `Accepted ID` = 7, note = 10) %>% 
      mutate(taxa_class = "trav") %>% 
      filter(is.na(`Taxon Name`) == FALSE)
    
    mc2_t_eye   <- readxl::read_xlsx(str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part 2.xlsx"), 
                                     sheet = 4, skip = 1) %>% 
      select(`Taxon Name` = 14, `Accepted ID` = 13, note = 16) %>% 
      mutate(taxa_class = "eye") %>% 
      filter(is.na(`Taxon Name`) == FALSE)
    
    mc2_taxa <- bind_rows(mc2_t_phyto, mc2_t_trav, mc2_t_eye) %>% 
      arrange(taxa_class, `Taxon Name`) %>% 
      select(taxa_class, `Taxon Name`, `Accepted ID`, note)
    
    return(mc2_taxa)
  }
  
  
  
  
} #close sahfos_taxa_key



mc_cleanup <- function(messy_df = mc1_phyto, taxon_key = mc1_taxa, taxa = "phyto") {
  
  #Create new columns to match NOAA data
  clean_df <- messy_df %>% 
    mutate(Cruise = str_extract(Sample_Id, "[^-]*"),
           Station = str_extract(Sample_Id, "-.*"),
           Station = str_replace(Station, "-", ""),
           Day = lubridate::day(Midpoint_Date_Local),
           Hour = lubridate::hour(Midpoint_Date_Local),
           Minute = lubridate::minute(Midpoint_Date_Local)) 
  
  #Phytoplankton sheet has the phytoplankton color index
  if(taxa == "phyto"){
    
    clean_df <- clean_df %>% 
      select(Sample_Id, Cruise, Station, Year, Month, Day, Hour, Minute, 
             `Latitude (degrees)` = Latitude, `Longitude (degrees)` = Longitude, 
             `Phytoplankton Color Index` = Chlorophyll_Index,  everything()) %>% 
      select(-Midpoint_Date_Local)
  } else {
    
    clean_df <- clean_df %>% 
      select(Sample_Id, Cruise, Station, Year, Month, Day, Hour, Minute, 
             `Latitude (degrees)` = Latitude, `Longitude (degrees)` = Longitude, everything()) %>% 
      select(-Midpoint_Date_Local)
  }
  
  #Pull taxon key for just phytoplankton
  tkey <- taxon_key %>% filter(taxa_class == taxa)
  
  #Swap column names using lookup table
  data.table::setnames(clean_df, old = as.character(tkey$`Accepted ID`), new = tkey$`Taxon Name`, skip_absent = TRUE)
  
  return(clean_df)
} # Close mc_cleanup







# Original Source: 13_allspecies_cpr_cleanup.R
import_sahfos_mc1 <- function( mc_taxa_key = mc1_taxa, sample_type = c("phyto", "eye", "trav")){
  
  # excel path
  xl_path <- str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part1.xlsx")
  
  # excel sheet number
  sheet_num <- switch(sample_type,
                      "phtyo" = 1,
                      "eye"   = 2,
                      "trav"  = 3)

  # Read the correct sheet of excel file
  mc1_data <- readxl::read_xlsx(xl_path, sheet = sheet_num)
  
  # do cleanup on it
  mc1_clean <- mc_cleanup(messy_df = mc1_data, 
                          taxon_key = mc_taxa_key, 
                          taxa = sample_type) %>% 
    rename_all(tolower)
  
  #prep for export
  return(mc1_clean)
  
  
} 
  
 
# MC2 Tables
import_sahfos_mc2 <- function(mc_taxa_key = mc2_taxa, sample_type = c("phyto", "eye", "trav")){
  
  
  # excel path
  xl_path <- str_c(gom_cpr_path, "SAHFOS-MBA_2013-2017/MC part 2.xlsx")
  
  # excel sheet number
  sheet_num <- switch(sample_type,
                      "phtyo" = 1,
                      "eye"   = 2,
                      "trav"  = 3)
  
  # Read the correct sheet of excel file
  mc2_data <- readxl::read_xlsx(xl_path, sheet = sheet_num)
  
  
  # do cleanup on it
  mc2_clean <- mc_cleanup(messy_df = mc2_data, 
                          taxon_key = mc_taxa_key,
                          taxa = sample_type) %>% 
    rename_all(tolower) 
  
  #prep for export
  return(mc2_clean)
  
}



####  SAHFOS Combine Sample Scales  ####

# bind rows on the two mc table sources
join_mc_data <- function(mc1_data, mc2_data){
  bind_rows(mc1_data, mc2_data)
}


# Pull out the first ten columns as sample metadata
pull_sahfos_metadata <- function(sahfos_data){
  sahfos_meta <- select(sahfos_data, 1:10)
}


# convert the different sample counting scales to a common unit of 100 cubic meters
sahfos_to_100 <- function(abundance_per_transect){
  
  
  # Separate abundances from metadata
  abundance_data <- abundance_per_transect %>% select(11:ncol(.))
  
  #Conversions - 
  
  # A. sub-sample count to full transect
  
  #phyto 1/8000th of transect counted
  #traverse 1/40th of transect counted
  #eyecount full transect counted
  
  # B. transect to water volume
  
  # 1 transect = 10 nautical miles
  # CPR aperture dimensions = 1.27 cm square entrance
  # aperture area in square meters = 0.00016129
  # 1852 meters in nautical mile * 10
  # volume in square meters per 10cm silk = 2.987091 meters^3
  
  # Original calculation to get to # per meters cubed
  conversion_rate <- 1 / 2.987091
  
  #Multiply by 100 to get to 100 cubic meters
  conversion_rate <- conversion_rate * 100
  
  # convert all abundance data using the conversion rate
  sahfos_100meters_cubed <- abundance_data * conversion_rate
  return(sahfos_100meters_cubed)
  
  
}


# Join the two zooplankton groups together
join_zooplankton <- function(sahfos_trav, sahfos_eye, sahfos_meta){
  
  strav_m3 <- sahfos_trav
  seye_m3  <- sahfos_eye
  
  #Idea, create a dataframe with names found in both, make the values the added contribution from one or both the subsample types
  unique_names     <- sort(unique(c(names(strav_m3), names(seye_m3))))
  new_df           <- data.frame(matrix(0, nrow = nrow(strav_m3), ncol = length(unique_names)))
  colnames(new_df) <- unique_names
  
  
  #Function to add columns as they appear in either set. 
  #Overwrites NA's with zeros
  taxa_fill <- function(empty_frame = new_df,  df_1 = strav_m3, df_2 = seye_m3) {
    
    #Taxon Names We want for the output
    taxa_names <- colnames(empty_frame)
    
    # 1. Abundances from the traverse subsampling procedure
    
    # Make a list that matches output names
    traverse_counts <- vector(mode = "list", length = length(taxa_names))
    names(traverse_counts) <- taxa_names
    
    # Fill that list with traverse abundances when they match
    traverse_counts <- imap(traverse_counts, function(x,y) {
      #Baseline of 0
      taxa_counts <- rep(0, nrow(df_1))
      
      if(y %in% names(df_1)) {
        taxa_counts <- df_1 %>% select(one_of(y)) %>% pull()
        taxa_counts[is.na(taxa_counts)] <- 0
      }
      
      return(taxa_counts)
    })
    
    #Bind list to a dataframe
    traverse_out <- bind_cols(traverse_counts)
    
    # 2. Abundances from eyecount subsampling procedure
    
    #Make a list that matches output names
    eyecount_counts <- vector(mode = "list", length = length(taxa_names))
    names(eyecount_counts) <- taxa_names
    
    # Fill that list with eyecount abundances
    eyecount_counts <- imap(eyecount_counts, function(x,y) {
      #Baseline of zero
      taxa_counts <- rep(0, nrow(df_2))
      
      if(y %in% names(df_2)) {
        taxa_counts <- df_2 %>% select(one_of(y)) %>% pull()
        taxa_counts[is.na(taxa_counts)] <- 0
      }
      
      return(taxa_counts)
    })
    
    #Bind list to a dataframe
    eyecount_out <- bind_cols(eyecount_counts)
    
    
    #Add the two of them to get combined abundance
    zooplankton_abundances <- traverse_out + eyecount_out
    return(zooplankton_abundances)
    
    
    
    
  }
  

  ####  Combine Traverse and Eyecounts Abundances
  # 1:1 sum of traverse and eyecount abunndances in # per 100 cubic meters
  sahfos_zoo <- taxa_fill(empty_frame = new_df, df_1 = strav_m3, df_2 = seye_m3)
  sahfos_zoo <- bind_cols(sahfos_meta, sahfos_zoo)
  
  return(sahfos_zoo)
  
}




####  Prepare NOAA and SAHFOS for Join  ####


consolidate_noaa_taxa <- function(noaa_gom_abundances){
  
  ####  Cleaner NOAA Consolidation  
  
  
  ####  1. Remove unused Taxa  
  #These names are not found at all in our data
  no_records <- noaa_gom_abundances %>% 
    pivot_longer(names_to = "taxon_stage", values_to = "abundance", 
                 cols = 11:ncol(noaa_gom_abundances)) %>% 
    mutate(abundance = as.numeric(abundance)) %>% 
    group_by(taxon_stage) %>% 
    summarise(presence = ifelse(sum(abundance, na.rm = T) > 0, "present", "absent")) %>% 
    ungroup() %>% 
    filter(presence == "absent") %>% 
    pull(taxon_stage)
  
  
  #Take the species with abundances out  of the rest of the data
  noaa_inuse <- noaa_gom_abundances %>% select(-one_of(no_records))
  
  
  ####  2. Make Column Key,  prep abundance and metadata
  
  # Key moved to top level ^  

  # Pull the station information
  cpr_metadata <- noaa_gom_abundances[,1:10]
  
  # Make sure abundances are numeric for combining them correctly
  cpr_abundances <- noaa_gom_abundances[, 11:ncol(noaa_gom_abundances)] %>% 
    mutate(across(everything(), as.numeric))
  
  ####  3. Use  Key to Consolidate Abundances
  
  
  # Consolidate in use taxa by stepping through groups of the taxa key
  # Keys that match a single column are treated as one column
  # Groups of multiple taxa stages are added together
  noaa_gom_refined <- imap(noaa_gom_taxa_key, function(x, y) {
    
    # Set the value used to replace -9999
    # NOTE -9999 was replaced with 0 for the publication I worked on with Andy
    # In hindsight it makes more sense to set them as the lowest value on scale or NA
    uncounted_replacement_value <- NA
    
    
    # So for names that have exact matches the transfer is easy
    if(length(x) == 1) {
      #Pull the correct column
      taxa_new <- data.frame(y = select(cpr_abundances, one_of(x)))
      # replace -9999
      taxa_new[taxa_new == -9999] <-  uncounted_replacement_value
      #set name
      colnames(taxa_new)[1] <- y
    } 
    
    # For cases where you are consolidating it is a little different:
    # need some way to sum the columns by row element, not collapse to sum the column
    if(length(x) > 1) {
      
      # Build dataframe of appropriate dimensions
      mult_columns_df <- as.data.frame(matrix(nrow = nrow(cpr_abundances), ncol = length(x) + 1))
      
      # fill in values from the columns that constitute the group
      for (i in 1:length(x)) {
        mult_columns_df[,i] <- select(cpr_abundances, one_of(x[i])) }
      
      # replace -9999
      mult_columns_df[mult_columns_df == -9999] <- uncounted_replacement_value
      
      # Add the columns together to get a sum column
      for (i in 1:nrow(mult_columns_df)) {
        mult_columns_df[i, length(x) + 1] <- sum(mult_columns_df[i, 1:length(x)], na.rm = T) }
      
      # Pull the sum column, set the name
      taxa_new <- data.frame(y = mult_columns_df[, length(x) + 1])
      colnames(taxa_new)[1] <- y
    }
    
    # return the new taxa groups
    return(taxa_new)
    
  }) %>% bind_cols()
  
  
  #Join the new columns back to the station_metadata
  noaa_gom_prepared <- bind_cols(cpr_metadata, noaa_gom_refined)
  
  return(noaa_gom_prepared)
  
}



# Original Source: 17_noaa_sahfos_eda.R
# join_cpr_sources <- function(sahfos_gom_abundances){
# 
# }