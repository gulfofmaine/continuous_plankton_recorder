# Gulf of Maine Continuous Plankton Recorder Analysis

Analysis of zooplankton trends in the gulf of maine region. Identifying changes in plankton abundance over time, and their relationship with physical properties of the environment as well as regional climate cycles.

## Repository Contents

 1. [README.md](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/README.md) - Introduction to the project and project navigation   
 
 2. [index.md](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/index.md) - Quick links to reports, presentations, & web apps   
 
 3. [R](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R) - Folder containing R code for data cleanup and analyses   
 
 
 
## R: Contents
 1. [00_cprdata_first_look.R(https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/00_cprdata_first_look.R): Primary investigation of **bi-monthly** cpr anomaly dataset   
 
 2. [00b_cprdata_first_look.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/00b_cprdata_first_look.R): Primary investigation of **bi-monthly** cpr anomaly dataset   
 
 3. [01_cpr_eda.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/01_cpr_eda.R): Exploratory data analysis of bi-monthly cpr data   
 
 4. [02_cpr_pca.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/02_cpr_PCA.R): Recreation of figures from *Pershing et al. 2005* PCA analysis   
 
 5. [02b_cpr_sst_pca.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/02b_cpr_sst_pca.R): Incorporation of bi-monthly SST data with CPR dataset. Creation of processed CPR and lagged SST datasets used in later analyses: `cpr_with_SSTlags.csv`, `SST_with_lags.csv`     
 
 6. [02c_quarterly_cpr_sst_regressions.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/02c_quarterly_cpr_sst_regressions.R): Regression analyses between quarterly CPR and SST datasets with and without period-lags. Creation of processed quarterly CPR+SST dataset: `quarterly_cpr_sst.csv`   
 
 7. [03_regime_shifts.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/03_regime_shifts.R): Identification of regime shifts in bi-monthly datasets   
 
 8. [03b_bestelmeyer2011_code.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/03b_bestelmeyer2011_code.R): Threshold detection code from Bestelmeyer et al. 2011   
 
 9. [03c_Bestel_Test.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/03c_Bestel_Test.R): Test of Bestelmeyer et al. 2011's methods   
 
 10. [04_bimonthly_sst_corrplots.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/04_bimonthly_sst_corrplots.R): Correlation plots for bi-monthly CPR and SST data. Full timeseries and late-period PCA timelines following Pershing et al. 2005 methods   
 
 11. [04b_quarterly_sst_corrplots.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/04b_quarterly_sst_corrplots.R): Quarterly correlation plots of CPR and SST data   
 
 12. [05_cluster_analysis.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/05_cluster_analysis.R): k-means clustering analysis of bi-monthly CPR and SST data   
 
 13. [05_cpr_clustering.Rmd]https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/05_cpr_clustering.Rmd): Rmarkdown document synthesizing research steps taken up to this point   
 
 14. [05_cpr_clustering.html](https://adamkemberling.github.io/continuous_plankton_recorder/R/05_cpr_clustering.html): Rendered output of Rmarkdown document   
 
 15. [06_buoy_data_mgmt.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/06_buoy_data_mgmt.R): Data management steps for synthesizing *NERACOOS* buoy data. Creation of `buoys_aggregated.csv` & `buoys_daily.csv`   
 
 16. [07_buoy_regressions.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/07_buoy_regressions.R): Regression analysis of quarterly bouy aggregate data and CPR data. Creation of `cpr_quarters_buoys.csv`      
 
 17. [08_buoy_anomaly_calculations.R](https://github.com/adamkemberling/continuous_plankton_recorder/commit/1a8410716575429d7e5be300ae3904c3afed734c): Calculation of daily buoy anomalies for temperature and salinity. Creation of `buoy_anomalies.csv`   
 
 18. [09_buoy_quarterly_pca.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/09_Buoy_quarterly_PCA.R): Quarterly buoy aggregate corrplots and PCA analysis   
 
 19. [10_buoy_daily_interpolations.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/10_buoy_daily_interpolations.R): Estimation of interpolated values for daily NA values in the buoy sensor data. Export of the non-interpolated data used to generate daily buoy PCA `buoy_pcadat_raw.csv` & the interpolated dataset resulting from multiple-regression interpolation `buoy_pcadat_interpolated.csv`   
 
 20. [11_buoy_daily_pca.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/11_buoy_daily_PCA.R): PCA on daily buoy sensor measurements, then applied to interpolated buoy dataset   
 
 21. [12_paper_layout.Rmd](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/12_paper_layout.Rmd): Consolidation report of analyses run thus far   
 
 22. [12_paper_layout.html](https://adamkemberling.github.io/continuous_plankton_recorder/R/12_paper_layout.html): Rendered output of Rmarkdown document   
 
 23. [cpr_buoy_DE](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/cpr_buoy_DE): Shiny App Files for exploring correlations between buoy sensor data and quarterly CPR data   
 
 24. [cpr_helper_funs.R](https://github.com/adamkemberling/continuous_plankton_recorder/blob/master/R/cpr_helper_funs.R): Shortcut to commonly used functions   
 
 25. [NAO_import](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/NAO_import): Code for importing north atlantic oscillation data   
 
 26. [presentations](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/presentations): Code & images for xaringan presentations   
 
 27. [stylesheets](https://github.com/adamkemberling/continuous_plankton_recorder/tree/master/R/stylesheets): CSS stylesheets   
 


 
 
 
 


