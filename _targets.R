####  CPR Data Targets Pipeline
# 4/28/2021


# Load packages and set specific options for the workflow

library(targets)
library(tarchetypes)
library(sf)
library(here)
library(raster)
library(tidyverse)
source(here::here("R", "cpr_helper_funs.R"))
source(here("R", "support/gom_cpr_pipeline_support.R"))

options(tidyverse.quiet = T)


# Additional packages to load specific to target(s)
tar_option_set( packages = c("raster", "sf", "rmarkdown", "tidyverse", "gmRi") )






# Define target pipeline: Outlines high-level steps of the analysis
# Format is just a list of all the targets
# Order is not important, package sorts out connections for everything
list(
  
  #### GOM Import  ####
  
  #### Raw NOAA
  tar_target(
    name = gom_noaa_zoo,
    command = import_noaa_cpr(sample_type = "zoo", return_option = "abundances")),
  tar_target(
    name = gom_noaa_zoo_key,
    command = import_noaa_cpr(sample_type = "zoo", return_option = "key")),
  tar_target(
    name = gom_noaa_phyto,
    command = import_noaa_cpr(sample_type = "phyto", return_option = "abundances")),
  tar_target(
    name = gom_noaa_phyto_key,
    command = import_noaa_cpr(sample_type = "phyto", return_option = "key")),
  
  
  ####  Raw SAHFOS
  
  # MC1 Tables
  tar_target(sahfos_mc1_taxa,
             sahfos_taxa_key("mc1")),
  tar_target(
    name = gom_sahfos_eye_mc1,
    command = import_sahfos_mc1(mc_taxa_key = sahfos_mc1_taxa, sample_type = "eye")),
  tar_target(
    name = gom_sahfos_phyto_mc1,
    command = import_sahfos_mc1(mc_taxa_key = sahfos_mc1_taxa, sample_type = "phyto")),
  tar_target(
    name = gom_sahfos_trav_mc1,
    command = import_sahfos_mc1(mc_taxa_key = sahfos_mc1_taxa, sample_type = "trav")),
  
  # MC2 Tables
  tar_target(sahfos_mc2_taxa,
             sahfos_taxa_key("mc2")),
  tar_target(
    name = gom_sahfos_eye_mc2,
    command = import_sahfos_mc2(mc_taxa_key = sahfos_mc2_taxa, sample_type = "eye")),
  tar_target(
    name = gom_sahfos_phyto_mc2,
    command = import_sahfos_mc2(mc_taxa_key = sahfos_mc2_taxa, sample_type = "phyto")),
  tar_target(
    name = gom_sahfos_trav_mc2,
    command = import_sahfos_mc2(mc_taxa_key = sahfos_mc2_taxa, sample_type = "trav")),

  # join the two mc periods
  tar_target(sahfos_phyto,
             join_mc_data(gom_sahfos_phyto_mc1, gom_sahfos_phyto_mc2)),
  tar_target(sahfos_trav,
             join_mc_data(gom_sahfos_trav_mc1, gom_sahfos_trav_mc2)),
  tar_target(sahfos_eye,
             join_mc_data(gom_sahfos_eye_mc1, gom_sahfos_eye_mc2)),
  tar_target(sahfos_meta,
             pull_sahfos_metadata(sahfos_trav)),
  
  
  
  #### Unit Conversions  ####
  # sahfos data is in number per silk transect currently
  # which are values centered around a categorical counting system
  # conversion goes from actual numbers captured from actual cpr water volume
  # to what those rates are in # per 100 meters cubed
  tar_target(sahfos_phyto_100m,
             sahfos_to_100(sahfos_phyto)),
  tar_target(sahfos_trav_100m,
             sahfos_to_100(sahfos_trav)),
  tar_target(sahfos_eye_100m,
             sahfos_to_100(sahfos_eye)),
  
  
  # Combine Traverse and Eyecount Zooplankton Groups
  # this is what sourcing 16_SAHFOS_CPR_Cleanup.R returns
  tar_target(sahfos_zoo_100m,
             join_zooplankton(sahfos_trav = sahfos_trav_100m, 
                              sahfos_eye = sahfos_eye_100m, 
                              sahfos_meta = sahfos_meta)),
  
  #### Resolving Taxa Differences ####
  tar_target(noaa_taxa_resolved,
             consolidate_noaa_taxa(noaa_gom_abundances = gom_noaa_zoo)),
  
  # match the column names to the noaa columns
  tar_target(sahfos_zoo_renamed,
             match_sahfos_to_noaa(sahfos_zoo_100m)),
  
  
  ####  Joining Different Sources  ####
  tar_target(gom_combined_zooplankton,
             join_gom_zoo_sources(noaa_taxa_resolved, sahfos_zoo_renamed))
  
  
  
  ####  Seasonal Splines  ####
  
  
  
  
  #### MAB Data  ####
  
  
  
  
  
  
)
# End of _targets.R