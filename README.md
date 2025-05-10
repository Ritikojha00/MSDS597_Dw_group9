# Global Aviation Navaid Analysis

**Author:** Ritik Ojha


## Overview  
This project evaluates navigational‐aid (navaid) coverage, allocation, and calibration across airports worldwide to identify underserved regions and recommend improvements.

## What I Did  
- **Data Ingestion & Cleaning**  
  - Loaded raw `airports.csv`  
  - Standardized column names and data types  
  - Handled missing values and anomalies  

- **Feature Engineering**  
  - Created indicators for hemisphere, elevation category, airport size, and world region  
  - Computed magnetic variation zones for recalibration analysis  

- **Data Backup Automation**  
  - Saved timestamped snapshots of raw, cleaned, and enriched datasets in `backup/`  
  - Logged row/column counts and transformation steps  

- **Country & Region Analysis**  
  - Generated summary tables of airport counts and navaid types by country and region  
  - Identified gaps in navaid coverage (e.g., Sub‑Saharan Africa)  

- **Data Merging & Integration**  
  - Merged enriched airport data with navaid summary tables  
  - Prepared consolidated datasets for visualization  

- **Visualization & Reporting**  
  - Created global distribution maps (airport types, navaid gaps) using `ggplot2` and `maps`  
  - Built interactive HTML report (`dw_final_project_merged_group9.html`)  

