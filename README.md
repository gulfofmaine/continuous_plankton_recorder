# Gulf of Maine Continuous Plankton Recorder Analysis

Analysis of zooplankton trends in the gulf of maine region. Identifying changes in plankton abundance over time, and their relationship with physical properties of the environment as well as regional climate cycles.

## Repository Contents

 1. `README.md` - Introduction to the project and project navigation   
 2. `index.md` - Quick links to reports, presentations, & web apps   
 3. `R` - Folder containing R code for data cleanup and analyses
 
 
## R: Contents
 1. `00_cprdata_first_look.R`: Primary investigation of **bi-monthly** cpr anomaly dataset   
 2. `00b_cprdata_first_look.R`: Primary investigation of **bi-monthly** cpr anomaly dataset   
 3. `01_cpr_eda.R` : Exploratory data analysis of bi-monthly cpr data   
 4. `02_cpr_pca.R` : Recreation of figures from *Pershing et al. 2005* PCA analysis   
 5. `02b_cpr_sst_pca.R` : Incorporation of bi-monthly SST data with CPR dataset. Creation of processed CPR and lagged SST datasets used in later analyses: `cpr_with_SSTlags.csv`, `SST_with_lags.csv`     
 6. `02c_quarterly_cpr_sst_regressions.R` : Regression analyses between quarterly CPR and SST datasets with and without period-lags. Creation of processed quarterly CPR+SST dataset: `quarterly_cpr_sst.csv`   
 7. `03_regime_shifts.R`: Identification of regime shifts in bi-monthly datasets   
 8. `03b_bestelmeyer2011_code.R` : Threshold detection code from Bestelmeyer et al. 2011   
 9. `03c_Bestel_Test.R` : Test of Bestelmeyer et al. 2011's methods   
 10. `04_bimonthly_sst_corrplots.R` : Correlation plots for bi-monthly CPR and SST data. Full timeseries and late-period PCA timelines following Pershing et al. 2005 methods   
 11. `04b_quarterly_sst_corrplots.R` : Quarterly correlation plots of CPR and SST data   
 12. `05_cluster_analysis.R` : k-means clustering analysis of bi-monthly CPR and SST data   
 13. `05_cpr_clustering.Rmd` : Rmarkdown document synthesizing research steps taken up to this point   
 14. `05_cpr_clustering.html` : Rendered output of Rmarkdown document   
 15. `06_buoy_data_mgmt.R` : Data management steps for synthesizing *NERACOOS* buoy data. Creation of `buoys_aggregated.csv` & `buoys_daily.csv`   
 16. `07_buoy_regressions.R`   
 17. `08_buoy_anomaly_calculations.R`   
 18. `09_buoy_quarterly_pca.R.R`   
 19. `10_buoy_daily_interpolations.R`   
 20. `11_buoy_daily_pca.R`   
 21. `12_paper_layout.Rmd`   
 22. `12_paper_layout.html`   
 23. `buoy_data`   
 24. `cpr_buoy_DE`   
 25. `cpr_helper_funs.R`   
 26. `NAO_import`   
 27. `presentations`   
 28. `stylesheets`   


 
 
 
 


