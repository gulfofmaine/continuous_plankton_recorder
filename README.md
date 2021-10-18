# Gulf of Maine Continuous Plankton Recorder Analysis

This repository is home to the data reconciliation of the Gulf of Maine CPR obtained from NOAA Fisheries (NEFSC) and the Sir Alister Hardy Foundation (SAHFOS), as well as analyses looking at patterns in abundance anomalies.

## Analysis Pipeline Documentation

Zooplankton analyses contained here focus on identifying changes in plankton abundance over time, and their relationship with physical properties of the environment.

The analysis pipeline has been documented using the {targets} package and can be seen in the `_targets.R` file.


## Project Data Provenance Documentation

Full documentation of the data provenance for this project have been made available on [github using bookdown and github pages here:](https://adamkemberling.github.io/continuous_plankton_recorder/docs/)

The above link is a repackaging of the original documentation done [here](https://adamkemberling.github.io/continuous_plankton_recorder/R/data_dictionaries/CPR_Data_Provenance.html)


## Rmd Reports

Various rendered reports have been made available using github pages. Links to the various reports can be found at the [index page](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/index.md)



---

## Repo Contents: File Descriptions

[README.md](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/README.md) - Introduction to the project and project navigation   
 
[index.md](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/index.md) - Quick links to reports, presentations, & web apps   
 
[R](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R) - Folder containing R code for data cleanup and analyses   

[matlab](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/matlab) - Folder with matlab CalProc routine for estimating seasonal splines
 
 
## R: Contents
[00_cprdata_first_look.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/00_cprdata_first_look.R): Primary investigation of **bi-monthly** cpr anomaly dataset   
 
[00b_cprdata_first_look.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/00b_cprdata_first_look.R): Primary investigation of **bi-monthly** cpr anomaly dataset   
 
[01_cpr_eda.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/01_cpr_eda.R): Exploratory data analysis of bi-monthly cpr data   
 
[02_cpr_pca.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/02_cpr_PCA.R): Recreation of figures from *Pershing et al. 2005* PCA analysis   
 
[02b_cpr_sst_pca.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/02b_cpr_sst_pca.R): Incorporation of bi-monthly SST data with CPR dataset. Creation of processed CPR and lagged SST datasets used in later analyses: `cpr_with_SSTlags.csv`, `SST_with_lags.csv`     
 
[02c_quarterly_cpr_sst_regressions.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/02c_quarterly_cpr_sst_regressions.R): Regression analyses between quarterly CPR and SST datasets with and without period-lags. Creation of processed quarterly CPR+SST dataset: `quarterly_cpr_sst.csv`   
 
[03_regime_shifts.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/03_regime_shifts.R): Identification of regime shifts in bi-monthly datasets   
 
[03b_bestelmeyer2011_code.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/03b_bestelmeyer2011_code.R): Threshold detection code from Bestelmeyer et al. 2011   
 
[03c_Bestel_Test.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/03c_Bestel_Test.R): Test of Bestelmeyer et al. 2011's methods   
 
[04_bimonthly_sst_corrplots.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/04_bimonthly_sst_corrplots.R): Correlation plots for bi-monthly CPR and SST data. Full timeseries and late-period PCA timelines following Pershing et al. 2005 methods   
 
[04b_quarterly_sst_corrplots.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/04b_quarterly_sst_corrplots.R): Quarterly correlation plots of CPR and SST data   
 
[05_cluster_analysis.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/05_cluster_analysis.R): k-means clustering analysis of bi-monthly CPR and SST data   
 
[05_cpr_clustering.Rmd](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/05_cpr_clustering.Rmd): Rmarkdown document synthesizing research steps taken up to this point   
 
[05_cpr_clustering.html](https://adamkemberling.github.io/continuous_plankton_recorder/R/05_cpr_clustering.html): Rendered output of Rmarkdown document   
 
[06_buoy_data_mgmt.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/06_buoy_data_mgmt.R): Data management steps for synthesizing *NERACOOS* buoy data. Creation of `buoys_aggregated.csv` & `buoys_daily.csv`   
 
[07_buoy_regressions.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/07_buoy_regressions.R): Regression analysis of quarterly bouy aggregate data and CPR data. Creation of `cpr_quarters_buoys.csv`      
 
[08_buoy_anomaly_calculations.R](https://github.com/adamkemberling/continuous_plankton_recorder/commit/1a8410716575429d7e5be300ae3904c3afed734c): Calculation of daily buoy anomalies for temperature and salinity. Creation of `buoy_anomalies.csv`   
 
[09_buoy_quarterly_pca.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/09_Buoy_quarterly_PCA.R): Quarterly buoy aggregate corrplots and PCA analysis   
 
[10_buoy_daily_interpolations.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/10_buoy_daily_interpolations.R): Estimation of interpolated values for daily NA values in the buoy sensor data. Export of the non-interpolated data used to generate daily buoy PCA `buoy_pcadat_raw.csv` & the interpolated dataset resulting from multiple-regression interpolation `buoy_pcadat_interpolated.csv`   
 
[11_buoy_daily_pca.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/11_buoy_daily_PCA.R): PCA on daily buoy sensor measurements, then applied to interpolated buoy dataset   
 
[12_paper_layout.Rmd](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/12_paper_layout.Rmd): Consolidation report of analyses run thus far   
 
[12_paper_layout.html](https://adamkemberling.github.io/continuous_plankton_recorder/R/12_paper_layout.html): Rendered output of Rmarkdown document   

[13_sahfos_cpr_cleanup.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/13_sahfos_cpr_cleanup.R): Reads in the full CPR datasets from NOAA and from SAHFOS. Splits both into a key created from the header space, and the abundances/concentrations by station.  

[14_taxa_check_heatplots.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/14_taxa_check_heatplots.R): Visualization tools for determining which taxa-stage groupings are in use and how that changes over time.   

[15_NOAA_CPR_Cleanup.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/15_NOAA_CPR_Cleanup.R): Script for reducing the NOAA dataset down to groupings that are more similar to the SAHFOS data, drops taxa that are not ever observed.   

[16_SAHFOS_CPR_Cleanup.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/16_SAHFOS_CPR_Cleanup.R): Partner script to the NOAA CPR Key. Converts SAHFOS data from concentration per 10cm silk mesh to concentration per $m^3$   

[17_noaa_sahfos_eda.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/17_noaa_sahfos_eda.R): Exploratory Data Analysis of the NOAA/SAHFOS data, post-cleanup.   

[18_karen_s_analysis_funs.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/18_karen_s_analysis_funs.R): Re-coding of MATLAB code obtained from Karen Stamieszkin. Splits taxa proportionally into contributing stages. Determines the predicted length for each stage at a given temperature. 
 
[19_cpr_splines.R](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/19_cpr_splines.R): Seasonal abundance anomaly calculations for combined NOAA/SAHFOS CPR data
 
[cpr_buoy_DE](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/cpr_buoy_DE): Shiny App Files for exploring correlations between buoy sensor data and quarterly CPR data   
 
[cpr_helper_funs.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/cpr_helper_funs.R): Shortcut to commonly used functions   
 
[NAO_import](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/NAO_import): Code for importing north atlantic oscillation data   
 
[new_anom_analyses](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R): Scripts for refined workflow using NOAA/SAHFOS combined dataset
 
[presentations](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/presentations): Code & images for xaringan presentations   
 
[stylesheets](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/stylesheets): CSS stylesheets   
 


## R/new_anom_analyses: Contents

[01_new_anoms_eda.R](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/new_anom_analyses/01_new_anoms_eda.R): Verificaation that new anomalies resemble matlab anomaly routine with exploratory analyses.

[02_new_anoms_pca.R](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/new_anom_analyses/02_new_anoms_pca.R): Recreation of PCA analyses using R routine anomaly data

[03_new_anoms_quarterly_sst.R](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/new_anom_analyses/03_new_anoms_quarterly_sst.R): Quarterly SST anomaly regression figures for all taxa in new dataset

[04_new_anoms_sst_corrplots.R](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/new_anom_analyses/04_new_anoms_sst_corrplots.R): Correlograms between CPR taxa anomalies and SST anomalies

[05_new_anoms_buoys.R](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/new_anom_analyses/05_new_anoms_buoys.R): Quarterly buoy sensor data correlations

[figures](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/new_anom_analyses/figures): Figure recreations created using the new R anomaly routine from the combined NOAA/SAHFOS dataset
 
 


