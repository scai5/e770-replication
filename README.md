# ECON 770 Replication Project 
This project replicates an adapted version of “Effects of the State-Level Earned Income Tax Credit Laws in the US on Maternal Health Behaviors and Infant Health Outcomes” (Markowitz et al. 2017). All code is written in R. This repository does not contain data or data documentation. 
- `analysis` contains all analysis scripts. `analysis/_main.R` calls all relevant scripts in order, including cleaning and analysis scripts.
- All data cleaning code is in the `data-code` directory. `data-code/cleaning.R` cleans raw data from `data/raw` and generates cleaned data into `data/generated`. The `data` directory is not included in this respository. 
- `results` contains all output from analysis. 
