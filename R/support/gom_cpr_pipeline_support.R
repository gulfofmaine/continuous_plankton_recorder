####  CPR Pipeline Functions
# How to get from raw data to analysis sets
# pipeline tracked using {targets}


####  Packages  ####
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(mgcv))
suppressPackageStartupMessages(library(gmRi))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(tidyverse))



####  Constants  ####
# # Base path
# ccel_boxpath <- "/Users/akemberling/Box/Climate Change Ecology Lab"
# gom_cpr_path <- str_c(ccel_boxpath, "/Data/Gulf of Maine CPR/")
gom_cpr_path <- box_path("climate change ecology lab", "Data/Gulf of Maine CPR")

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
   
    
    #Replace old names with correct names
    names(noaa_phyto_abundances)[11:ncol(noaa_phyto_abundances)] <- phyto_group_stage_names
    names(noaa_phyto_abundances) <- str_replace_all(names(noaa_phyto_abundances), "'", "")
    
    
    ####  Repair MARMAP Key  ####
    # Prepare header, and make taxon key
    # MARMAP code key: https://www.nefsc.noaa.gov/nefsc/Narragansett/taxcodesA.html
    
    
    # Pull columns referencing taxa
    noaa_phyto_header <- noaa_phyto[1, 10:ncol(noaa_phyto)]
    
    # Swap in the formatted names
    names(noaa_phyto_header) <- c("code_type", phyto_group_stage_names)
    
    
    # Make Taxon Key for marmap values
    noaa_phyto_key <- noaa_phyto_header %>% 
      pivot_longer(cols = -code_type, names_to = "Taxon Name", values_to = "Accepted ID") %>% 
      arrange(`Taxon Name`)
    
    
    # Check for duplicates before reshaping:
    noaa_phyto_dups <- noaa_phyto_key %>% 
      pivot_wider(names_from = code_type, values_from = `Accepted ID`, values_fn = length) %>% 
      filter(`Marmap Code:` > 1)
    
    
    # Address duplicate marmap codes through manually looking them up:
    colnames(noaa_phyto_abundances)[which(noaa_phyto[1,] == 9101)] <- "Trichodesmium"
    colnames(noaa_phyto_abundances)[which(noaa_phyto[1,] == 9169)] <- "Ceratium ranipes"
    
    
    # Replace accepted ID with marmap code once duplicates have been resolved
    noaa_phyto_key <- noaa_phyto_key %>% 
      mutate(
        `Taxon Name` = ifelse( `Accepted ID` == 9101, "Trichodesmium", `Taxon Name`),
        `Taxon Name` = ifelse( `Accepted ID` == 9169, "Ceratium ranipes", `Taxon Name`)) %>% 
      pivot_wider(names_from = code_type, values_from = `Accepted ID`) %>% 
      rename_all(tolower)
    
    
    ####  Prep for Export  ####
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
   
    #Replace old names with correct names
    names(noaa_zoo_abundances)[11:ncol(noaa_zoo_abundances)] <- group_stage_names
    names(noaa_zoo_abundances) <- str_replace_all(names(noaa_zoo_abundances), "'", "")
    
    #Fix header, and make taxon key
    noaa_zoo_header <- noaa_zoo[2:3, 10:ncol(noaa_zoo)]
    names(noaa_zoo_header) <- c("code_type", group_stage_names)
    
    #Reshape Taxon Key to show taxa and stage with marmap codes
    noaa_zoo_key <- noaa_zoo_header %>% 
      pivot_longer(cols = -code_type, names_to = "Taxon Name", values_to = "Accepted ID") %>% 
      arrange(`Taxon Name`)
    
    # Check duplicates, should all be unique
    key_dups <- noaa_zoo_key %>% 
      pivot_wider(names_from = code_type, values_from = `Accepted ID`, values_fn = length) %>% 
      filter(`Marmap Taxonomic Code:` > 1 | `Marmap Stage Code:` > 1 ) %>% 
      pull(`Taxon Name`)
    
    
    
    # What duplicate Marmap numbers are representing currently, should be unique to taxa
    noaa_zoo_key %>% filter(`Taxon Name` %in% key_dups) %>% distinct()
    
    #### Replace taxon names using NEFSC marmap key
    
    # # Which we need to change, manually checked*
    colnames(noaa_zoo_abundances)[which(noaa_zoo[2,] == 3300)] <- "Heteropoda"
    colnames(noaa_zoo_abundances)[which(noaa_zoo[2,] == 4039)] <- "No record"
    
    #Drop column that has no taxa match for the marmap code 
    noaa_zoo_abundances[,"No record"] <- NULL
    
    
    ####  Repair MARMAP Key  ####
    
    # Make changes to key as well so there is no duplication of codes
    noaa_zoo_key <- noaa_zoo_key %>% 
      mutate(
        `Taxon Name` = ifelse(`Accepted ID` == 3300, "Heteropoda", `Taxon Name`),
        `Taxon Name` = ifelse(`Accepted ID` == 4039, "No record", `Taxon Name`)) %>% 
      distinct() %>% 
      pivot_wider(names_from = code_type, values_from = `Accepted ID`) %>% 
      rename_all(tolower)%>% 
      filter(`taxon name` != "No record")
    
    
    
    #Seperate taxa and stage
    noaa_zoo_key <- noaa_zoo_key %>% mutate(
      taxa = str_extract(`taxon name`, "[^_]*"),
      stage = str_extract(`taxon name`, "_.*"),
      stage = str_replace(stage, "_", "")
    )
    
    
    ####  Prep for Export  ####
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
bind_mc_tables <- function(mc1_data, mc2_data){
  bind_rows(mc1_data, mc2_data)
}


# Pull out the first ten columns as sample metadata structure
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
  
  # rename to reuse original workflow code
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


consolidate_noaa_taxa <- function(noaa_abundances){
  
  ####  Cleaner NOAA Consolidation  
  
  
  ####  1. Remove unused Taxa  
  #These names are not found at all in our data
  no_records <- noaa_abundances %>% 
    pivot_longer(names_to = "taxon_stage", values_to = "abundance", 
                 cols = 11:ncol(noaa_abundances)) %>% 
    mutate(abundance = as.numeric(abundance)) %>% 
    group_by(taxon_stage) %>% 
    summarise(presence = ifelse(sum(abundance, na.rm = T) > 0, "present", "absent")) %>% 
    ungroup() %>% 
    filter(presence == "absent") %>% 
    pull(taxon_stage)
  
  
  #Take the species with abundances out  of the rest of the data
  noaa_inuse <- noaa_abundances %>% select(-one_of(no_records))
  
  
  ####  2. Make Column Key,  prep abundance and metadata
  
  # Key moved to top level ^  

  # Pull the station information
  cpr_metadata <- noaa_abundances[,1:10]
  
  # Make sure abundances are numeric for combining them correctly
  cpr_abundances <- noaa_abundances[, 11:ncol(noaa_abundances)] %>% 
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
  
  # format station column to bind later with sahfos
  noaa_gom_prepared$station <- as.character(noaa_gom_prepared$station)
  
  return(noaa_gom_prepared)
  
}



# Original Source: 17_noaa_sahfos_eda.R
# match sahfos column names to the noaa taxa groups
match_sahfos_to_noaa <- function(sahfos_zoo){
  
  #Renaming of sahfos data to match the refined noaa list
  sahfos_zoo_2 <- sahfos_zoo %>% 
    rename(
      `acartia spp.`              = `acartia spp. (unidentified)`,
      `amphipoda spp.`            = `amphipoda (unidentified)`,
      `appendicularia spp.`       =  appendicularia,
      `bivalvia spp.`             = `bivalvia larvae`,
      `calanus finmarchicus v-vi` = `calanus finmarchicus`,
      `calanus i-iv`              = `calanus i-iv`,
      `calanus spp.`              = `calanus v-vi unidentified`, 
      `centropages spp.`          = `centropages spp. (unidentified)`,
      `cumacea spp.`              =  cumacea,
      `doliolidae spp.`           =  doliolidae,
      `euchaeta spp.`             = `euchaetidae (unidentified)`,
      `euphausiacea spp.`         = `euphausiacea total`,
      `foraminifera spp.`         = `foraminifera (total)`,
      `gammaridea spp.`           =  gammaridea,
      `gastropoda spp.`           = `gastropoda (unidentified)`,
      `gymnosomoata spp.`         = `gymnosomata (unidentified)`,
      `harpacticoida spp.`        = `harpacticoida total traverse`,
      `hyperiidea spp.`           = `hyperiidea (total)`,
      `ischnocalanus spp.`        =  ischnocalanus,
      `lepas spp.`                = `lepas nauplii`,
      `metridia spp.`             = `metridia spp. (v-vi) (unidentified)`,
      `monstrilloida spp.`        =  monstrilloida,
      `ostracoda spp.`            =  ostracoda,
      `pleuromamma spp.`          = `pleuromamma spp. (unidentified)`,
      `polychaeta larva`          = `polychaete larvae (unidentified)`,
      `salpidae spp.`             = `salpidae (total)`,
      `siphonostomatoida spp.`    =  siphonostomatoida,
      `thecosomata spp.`          = `thecosomata (north atlantic)`,
      `tintinnidae spp.`          = `tintinnida total` ) 
  
  #This section is for when multiple columns need to be reduced to a single aggregate
  sahfos_zoo_renamed <- sahfos_zoo_2 %>% 
    mutate(
      `candacia spp.`                     = `candacia i-iv` + `candacia spp. (unidentified)`, 
      `candacia i-iv`                     =  NULL,
      `candacia spp. (unidentified)`      =  NULL,
      `copepoda spp.`                     = `copepod eggs` + `copepod nauplii`,
      `copepod eggs`                      =  NULL,
      `copepod nauplii`                   =  NULL,
      `decapoda spp.`                     = `decapod megalopa` + `decapod zoea` + `decapoda larvae (total)`,
      `decapod megalopa`                  =  NULL, 
      `decapod zoea`                      =  NULL,
      `decapoda larvae (total)`           =  NULL,
      `fish eggs`                         = `fish eggs (total)` + `fish eggs with oil globules` + `fish eggs without oil globules`,
      `fish eggs (total)`                 =  NULL,
      `fish eggs with oil globules`       =  NULL,
      `fish eggs without oil globules`    =  NULL,
      `pseudocalanus spp.`                = `pseudocalanus spp. adult atlantic` + `pseudocalanus spp. adult total`,
      `pseudocalanus spp. adult atlantic` =  NULL, 
      `pseudocalanus spp. adult total`    =  NULL,
      `radiolaria spp.`                   = `radiolaria non-acantharian` + `radiolaria total`,
      `radiolaria non-acantharian`        =  NULL,
      `radiolaria total`                  =  NULL,
      sample_id                           =  NULL)
  
  return(sahfos_zoo_renamed)
  
  
  
}




#Bind the two data sources
join_zoo_sources <- function(noaa_zoo_refined, sahfos_zoo_renamed){
  
  combined_set <- bind_rows(
    list("NOAA"   = noaa_zoo_refined, 
         "SAHFOS" = sahfos_zoo_renamed), 
    .id = "Data Source")
  
  return(combined_set)
}



####________________________####
####  Seasonal Spline Prep  ####

cpr_spline_prep <- function(cpr_abundances){
  cpr_date_prepped <- cpr_abundances %>% 
    mutate(cal_date = as.POSIXct(str_c(year, month, day, sep = "/"), format = "%Y/%m/%d"), .after = "day") %>% 
    mutate(jday = lubridate::yday(cal_date), .after = "cal_date") %>% 
    rename(lon = `longitude (degrees)`,
           lat = `latitude (degrees)`)
  
  return(cpr_date_prepped)
}





#### Trim Data to Study Area BBox  ####



#' @title CPR Area Filter
#' 
#' @description Subset the Gulf of Maine CPR survey data using a specific survey area, specified by name. 
#' Extents for the following areas are available: 
#'
#' @param study_area Indication of what area to subset with. Includes GOM, GOM_new, CCB, WGOM, EGOM, SS.
#'
#' @return 
#' @export
#'
#' @examples
cpr_area_crop <- function(cpr_dat, study_area = "GOM_new"){
  
  area_bboxes <- tribble( ##### Area BBbox Open  ####
                          ~"area",  ~"lon",  ~"lat",
                          #Gulf of Maine - Historic
                          "gom",   -70.000000,	42.200000,
                          "gom",   -68.600000,	42.200000,
                          "gom",   -68.600000,	42.400000,
                          "gom",   -66.600000,	42.400000,
                          "gom",   -66.600000,	43.400000,
                          "gom",   -68.600000,	43.400000,
                          "gom",   -68.600000,	43.200000,
                          "gom",   -70.000000,	43.200000,
                          "gom",   -70.000000,	42.200000,
                          
                          #Gulf of Maine - Extended North to capture cpr route change
                          "gom_new",   -70.000000,	42.200000, 
                          "gom_new",   -66.600000,	42.200000, 
                          "gom_new",   -66.600000,	43.800000, 
                          "gom_new",   -70.000000,	43.800000, 
                          "gom_new",   -70.000000,	42.200000,
                          
                          "ccb",   -70.800000,	42.200000,
                          "ccb",   -70.000000,	42.200000,
                          "ccb",   -70.000000,	42.800000,
                          "ccb",   -70.800000,	42.800000,
                          "ccb",   -70.800000,	42.200000,
                          
                          #Western Gulf of Maine
                          "wgom",  -70.000000, 	42.200000,
                          "wgom",  -68.600000, 	42.200000,
                          "wgom",  -68.600000, 	43.200000,
                          "wgom",  -70.000000, 	43.200000,
                          "wgom",  -70.000000, 	42.200000,
                          
                          #Eastern Gulf of Maine
                          "egom",  -68.600000,	42.400000,
                          "egom",  -66.600000,	42.400000,
                          "egom",  -66.600000,	43.400000,
                          "egom",  -68.600000,	43.400000,
                          "egom",  -68.600000,	42.400000,
                          
                          "ss",    -66.600000,	42.600000,
                          "ss",    -65.400000,	42.600000,
                          "ss",    -65.400000,	43.400000,
                          "ss",    -66.600000,	43.400000,
                          "ss",    -66.600000,	42.600000,
  ) %>% arrange(area) ##### Area BBbox Close  ####
  
  
  #Filter to the correct area
  study_area_bbox <- filter(area_bboxes, area == tolower(study_area))
  
  #subset data that fits
  cpr_dat <- cpr_dat %>% mutate(lon = ifelse(lon > 0, lon * -1, lon))
  
  cpr_dat <- cpr_dat %>% 
    filter(
      between(lon, min(study_area_bbox$lon), max(study_area_bbox$lon)),
      between(lat, min(study_area_bbox$lat), max(study_area_bbox$lat)))
  
  # Return the cpr data
  return(cpr_dat)
}


####  Spline Tools  ####


# Split taxa into list, drop groups only present in NOAA or SAHFOS:
split_cpr_by_taxa <- function(cpr_spline_prepped){
  
  # testing: 
  # tar_load(gom_area_cropped); cpr_spline_prepped <- gom_area_cropped
  
  # Start / End Taxa, instead of assuming no columns get added before/after
  start_taxa <- which(names(cpr_spline_prepped) == "phytoplankton color index")
  end_taxa <- which(names(cpr_spline_prepped) == "radiolaria spp.")
  
  # Identify the columns that represent abundances
  # taxa_cols <- names(cpr_spline_prepped)[12:ncol(cpr_spline_prepped)]
  taxa_cols <- names(cpr_spline_prepped)[start_taxa:end_taxa]
  names(taxa_cols) <- taxa_cols
  
  # Make a list with details on each taxa
  taxa_list <- map(taxa_cols, function(x){
    taxa_name <- sym(x)
    taxa_subset <- cpr_spline_prepped %>% 
      select(year, jday, lat, lon, abundance = !!taxa_name)
  })
  
  #Find those pesky NA taxa
  na_counts <- map(taxa_list, function(x){
    sum(is.na(x$abundance))}) %>% 
    bind_cols() %>%
    pivot_longer(names_to = "taxa", values_to = "total NA's", cols = everything()) 
  
  # falg taxa that are in either period, or just completely absent
  na_counts <- na_counts %>%
    mutate(status = case_when(
      `total NA's` == 290 ~ "NOAA only",
      `total NA's` == 4799 ~ "SAHFOS only",
      `total NA's` == 0 ~ "Found in both",
      `total NA's` > 4799 ~ "Too Many NA's",
      `total NA's` == nrow(na_counts) ~ "drop",
      TRUE ~ "unclear"
      
    ))
  
  # #Taxa with full time series
  # keepers <- filter(na_counts, status == "Found in both")
  keepers <- filter(na_counts, status != "drop")
  fullts_taxa <- taxa_list[names(taxa_list) %in% keepers$taxa]
  
  # Return just the taxa that exist in both periods
  return(fullts_taxa)
  
}



#' Continuous Plankton Recorder Seasonal Spline Tool
#'
#' @param cpr_dat Takes output of cpr_area_crop. A data.frame containing the following columns: year, julian day, lat, lon, and abundance.
#' @param spline_bins Integer value indicating the desired number of basis functions for the seasonal spline fit, default = 10
#' @param season_bins Integer value indicating the number of periods you wish there to be in a recurring 365 day cycle. 
#' These are used as labels, not in GAM fitting.
#'
#' @return  List containing 1. the original dataframe with log transformed abundances, labeled periods, and log transformed anomalies
#'  2. the mean, standard deviation, and sample size for each period for each year, and 3. the GAMS themselves
#' @export
#'
#' @examples
cpr_spline_fun <- function(cpr_dat = cpr_data, spline_bins = 10, season_bins = 4) {
  ####  Fit Seasonal Trend using all data  ####
  
  
  #Check Input dimensions are correct
  if(ncol(cpr_dat) != 5) {return(print('dat format requires 5 columns: [year, jday, lat, lon, #]'))}
  
  #Transform abundance to log abundance
  cpr_dat <- cpr_dat %>% 
    mutate(
      abundance = as.numeric(abundance),
      log_abund = log(abundance),
      log_abund = ifelse(is.infinite(log_abund), 0, log_abund)
    )
  
  
  #Build spline model using mgsv::gam using cyclic penalized cubic regression spline smooth
  cc_spline_mod <- gam(log_abund ~  s(jday, bs = "cc", k = spline_bins),
                       data = cpr_dat)
  
  
  #Add Predictions back to the data
  cpr_dat$spline_pred <- predict(cc_spline_mod, cpr_dat, type = "response")
  
  
  #Calculate anomalies
  cpr_dat$anomaly <- cpr_dat$log_abund - cpr_dat$spline_pred
  
  #Calculate Anomalies relative to standard deviation in observed log abundance:
  #overall_sd <- sd(cpr_dat$spline_pred, na.rm = T)
  overall_sd <- sd(cpr_dat$log_abund, na.rm = T)
  cpr_dat$rel_anomaly <- cpr_dat$anomaly / overall_sd
  
  
  ####  Calculate Seasonal Averages  ####
  #Get Period Split Points from number of season_bins
  bin_splits <- c(seq(0, 365, by = ceiling(365 / (season_bins))), 365)
  
  #Set period number in data based on desired number of splits
  period <- data.frame(
    period = rep(0, nrow(cpr_dat)),
    min_date = rep(NA, nrow(cpr_dat)),
    max_date = rep(NA, nrow(cpr_dat)))
  
  #Add period label and datebound
  for (n in 1:nrow(cpr_dat)) {
    for (i in 1:season_bins) {
      if( cpr_dat$jday[n] > bin_splits[i] & cpr_dat$jday[n] <=  bin_splits[i+1]) {
        period[n, "period"]   <- i
        period[n, "min_date"] <- bin_splits[i]
        period[n, "max_date"] <- bin_splits[i+1]
      }
    }
  }
  
  # Bind period column and predictions back to original data
  period <- period %>% 
    mutate(datebounds = str_c(min_date, "-", max_date)) %>% 
    select(-c(min_date, max_date))
  
  cpr_dat <- bind_cols(cpr_dat, period)
  
  #Get Period Means
  seasonal_summary <- cpr_dat %>% 
    group_by(year, period, datebounds) %>% 
    summarise(
      abund_mu = mean(abundance, na.rm = T),
      abund_sd = sd(abundance, na.rm = T),
      log_abund_mu = log(abund_mu),
      log_abund_mu = ifelse(is.infinite(log_abund_mu), 0, log_abund_mu),
      anom_mu = mean(anomaly, na.rm = T),
      anom_sd = sd(anomaly, na.rm = T),
      anom_z = mean(rel_anomaly, na.rm = T),
      n_stations = n(),
      .groups = "drop") %>% 
    ungroup() %>% 
    mutate(period = as.character(period))
  
  #Get Annual Means
  annual_summary <- cpr_dat %>% 
    group_by(year) %>% 
    summarise(
      abund_mu = mean(abundance, na.rm = T),
      abund_sd = sd(abundance, na.rm = T),
      log_abund_mu = log(abund_mu),
      log_abund_mu = ifelse(is.infinite(log_abund_mu), 0, log_abund_mu),
      anom_mu = mean(anomaly, na.rm = T),
      anom_sd = sd(anomaly, na.rm = T),
      anom_z = mean(rel_anomaly, na.rm = T),
      n_stations = n(),
      .groups = "drop") %>% 
    mutate(period = "annual",
           datebounds = "1-365")
  
  #Put the annual and period means together
  period_summaries <- bind_rows(seasonal_summary, annual_summary) %>% 
    arrange(year, period)
  
  # Return Pre out
  ts_out <- list(
    "cprdat_predicted" = cpr_dat,
    "period_summs"     = period_summaries,
    "spline_model"     = cc_spline_mod
  )
  return(ts_out)
  
  
} 




####_________________________####



# reshape the anomalies, subset into lists by years to include in each set
prep_PCA_periods <- function(cpr_anomalies_long, 
                             matrix_var = "standardized anomalies",
                             use_focal_species = FALSE,
                             year_subsets = list("1961-2003" = c(1961,2003))){
  
  
  # Testing:
  # tar_load(gom_seasonal_avgs); cpr_anomalies_long <- gom_seasonal_avgs
  
  
  #### a.  Subset to focal species:
  if(use_focal_species){
    
    # focal species used in 2005 paper
    species_05 <- c("Calanus I-IV", "Calanus finmarchicus V-VI", "Centropages typicus",
                    "Oithona spp.","Para-Pseudocalanus spp.",
                    "Metridia lucens",  "Euphausiacea spp.", "phytoplankton color index")
    
    
    # reformat names and filter the focal taxa out
    cpr_long <- cpr_anomalies_long %>% 
      mutate(taxa = tolower(taxa),
             taxa = str_replace_all(taxa, "para_pseu", "para-pseu"),
             taxa = str_replace_all(taxa, "_", " "),
             taxa = str_replace_all(taxa, "spp", "spp."),
             taxa = str_replace_all(taxa, "spp..", "spp.")) %>% 
      filter(taxa %in% tolower(species_05))
    
  }
  
  
  #### b. Re-format for PCA
 
  # Column to pivot
  var_col <- switch (matrix_var,
    "standardized anomalies" = "anom_z",
    "mean anomalies" = "anom_mu",
    "mean abundances" = "abund_mu")
  var_col <- sym(var_col)
  
  
  # Pivot wider to return a matrix
  cpr_wide <- cpr_long %>% 
    select(taxa, year, period, datebounds, anom_z) %>% 
    pivot_wider(names_from = taxa, values_from = {{var_col}}) %>% 
    janitor::clean_names()
  
  ####  Filter the years for each group, and grab just the taxa.
  subset_years <- map(year_subsets, function(x){
    
    # filter years
    x_years <- filter(cpr_wide, between(year, x[[1]], x[[2]]))
    
  })
  
  # return the list of wide data and metadata
  return(subset_years)
  
  
}




# Pick whether to use annual/time periods
prep_PCA_matrices <- function(period_list, periodicity = "annual"){
  
  # Testing:
  # tar_load(cpr_pca_periods); period_list <- cpr_pca_periods
  
  
  # For any of the period(s) pull abundance matrix and metadata to match
  map(period_list, function(period_x){
    
    # Flag the season/yearly periodicity we want
    abundance_data <- switch (periodicity,
                              "annual" = filter(period_x, datebounds == "1-365"),
                              "seasons"= filter(period_x, datebounds != "1-365"),
                              "winter" = filter(period_x, datebounds == "0-92"),
                              "spring" = filter(period_x, datebounds == "92-184"),
                              "summer" = filter(period_x, datebounds == "184-276"),
                              "fall"   = filter(period_x, datebounds == "276-365"))
    
    # Pull away the metadata
    meta_data <- select(abundance_data, year, period, datebounds, `phytoplankton_color_index`)
    
    
    # Pull matrix to pass to PCA
    pca_mat <- select(abundance_data, -c(year, period, datebounds, `phytoplankton_color_index`))
    
    # return metadata to match matrix
    return(list(pca_matrix = pca_mat,
                metadata = meta_data))
    
  })
  
  
  
}




