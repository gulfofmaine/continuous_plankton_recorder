####  CPR Data Targets Pipeline
# 4/28/2021


# Load packages and set specific options for the workflow
options(tidyverse.quiet = T)
suppressPackageStartupMessages(library(targets))
suppressPackageStartupMessages(library(tarchetypes))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(tidyverse))
# source(here::here("R", "cpr_helper_funs.R"))
source(here("R", "support/gom_cpr_pipeline_support.R"))




# Additional packages to load specific to target(s)
tar_option_set( packages = c("raster", "sf", "rmarkdown", "tidyverse", "gmRi") )






# Define target pipeline: Outlines high-level steps of the analysis
# Format is just a list of all the targets
# Order is not important, package sorts out connections for everything
list(
  
  
  
  #### GOM Analyses  ####
  
  #### Raw NOAA
  tar_target(
    name = noaa_zoo,
    command = import_noaa_cpr(sample_type = "zoo", return_option = "abundances")),
  tar_target(
    name = noaa_zoo_key,
    command = import_noaa_cpr(sample_type = "zoo", return_option = "key")),
  tar_target(
    name = noaa_phyto,
    command = import_noaa_cpr(sample_type = "phyto", return_option = "abundances")),
  tar_target(
    name = noaa_phyto_key,
    command = import_noaa_cpr(sample_type = "phyto", return_option = "key")),
  
  
  ####  Raw SAHFOS
  
  # MC1 Tables
  tar_target(sahfos_mc1_taxa,
             sahfos_taxa_key("mc1")),
  tar_target(
    name = sahfos_eye_mc1,
    command = import_sahfos_mc1(mc_taxa_key = sahfos_mc1_taxa, sample_type = "eye")),
  tar_target(
    name = sahfos_phyto_mc1,
    command = import_sahfos_mc1(mc_taxa_key = sahfos_mc1_taxa, sample_type = "phyto")),
  tar_target(
    name = sahfos_trav_mc1,
    command = import_sahfos_mc1(mc_taxa_key = sahfos_mc1_taxa, sample_type = "trav")),
  
  # MC2 Tables
  tar_target(sahfos_mc2_taxa,
             sahfos_taxa_key("mc2")),
  tar_target(
    name = sahfos_eye_mc2,
    command = import_sahfos_mc2(mc_taxa_key = sahfos_mc2_taxa, sample_type = "eye")),
  tar_target(
    name = sahfos_phyto_mc2,
    command = import_sahfos_mc2(mc_taxa_key = sahfos_mc2_taxa, sample_type = "phyto")),
  tar_target(
    name = sahfos_trav_mc2,
    command = import_sahfos_mc2(mc_taxa_key = sahfos_mc2_taxa, sample_type = "trav")),

  # join the two mc periods
  tar_target(sahfos_phyto,
             bind_mc_tables(sahfos_phyto_mc1, sahfos_phyto_mc2)),
  tar_target(sahfos_trav,
             bind_mc_tables(sahfos_trav_mc1, sahfos_trav_mc2)),
  tar_target(sahfos_eye,
             bind_mc_tables(sahfos_eye_mc1, sahfos_eye_mc2)),
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
             consolidate_noaa_taxa(noaa_abundances = noaa_zoo)),
  
  # match the column names to the noaa columns
  tar_target(sahfos_renamed,
             match_sahfos_to_noaa(sahfos_zoo_100m)),
  
  
  ####  Joining Different Sources  ####
  tar_target(combined_zooplankton,
             join_zoo_sources(noaa_taxa_resolved, sahfos_renamed)),
  
  
  
  ####  Seasonal Splines  ####
  # Format Dates 
  tar_target(cpr_spline_prepped,
             cpr_spline_prep(combined_zooplankton)),
  
  # Crop to study area
 tar_target(gom_area_cropped,
            cpr_area_crop(cpr_spline_prepped, study_area = "gom_new")),
  
 # Pull taxa into lists
 tar_target(taxa_abundance_list,
            split_cpr_by_taxa(gom_area_cropped)),
  
  
  # Run Models
  tar_target(gom_seasonal_splines, 
             command = map(.x = taxa_abundance_list,
                           .f = cpr_spline_fun, 
                           spline_bins = 10, 
                           season_bins = 4)),
  
  # Store Predicted Anomalies
  tar_target(gom_anomalies,
             command = map(gom_seasonal_splines, pluck, "cprdat_predicted")),
  
  # Store Seasonal Averages (yearly and seasonal averages)
  tar_target(gom_seasonal_avgs,
             command = map_dfr(gom_seasonal_splines, function(x){
               pluck(x, "period_summs")}, .id = "taxa" )),
  
  # Store the GAMS
  tar_target(gom_spline_models,
             command = map(gom_seasonal_splines, pluck, "spline_model"))
 
 
 
 ####  PCA work  ####
  
 # reshape as matrix (pick abundance or anomalies here)
 
 
 
 # select time period
 
 
 
 # select taxa to include
 
 
 
 # perform PCA's
  
 
 ####______________________####
  #### MAB Analyses  ####
  
  
  
  
  
  
)
# End of _targets.R