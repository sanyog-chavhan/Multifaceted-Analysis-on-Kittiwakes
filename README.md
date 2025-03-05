# Kittiwake Breeding Analysis

This repository contains **statistical analyses** and **R code** exploring breeding trends of Kittiwakes (a type of gull). The project includes:
- **R script** performing data loading, ANOVA, regression, and visualisations.
- **CSV datasets** located in the `data/` folder.
- **PDF report** summarising the findings, methods, and conclusions.

## Files & Folders

- **`data/`**  
  Contains the following CSV files:
  - `Kittiwake_Observation.csv`
  - `Kittiwake_Historical.csv`
  - `Kittiwake_Measurement.csv`
  - `Kittiwake_Location.csv`

- **`Multifaceted_Analysis_on_Kittiwakes.R`**  
  - R script for tasks including:
    - Reading and summarising observation data
    - Performing ANOVA on historical breeding data
    - Conducting correlation and t-tests on measurement data
    - Fitting linear and log-linear models on location data

- **`Understanding_Kittiwake_Trends_A_Multifaceted_Analysis.pdf`**  
  - A detailed report covering:
    - Exploratory data analysis
    - Statistical tests and findings
    - Visualisations and interpretations

## How to Run

1. **Clone this repository** (instructions below) or download the files locally.
2. Open **RStudio** (or any R environment).
3. Ensure you have the required packages installed (see below).
4. **Open** `Multifaceted_Analysis_on_Kittiwakes.R`.
5. **Run** the script chunk by chunk (or all at once) to reproduce the analyses.

## Requirements

- **R** (version 4.0+ recommended)
- **R packages**:  
  - `ggplot2`, `dplyr`, `reshape2`, `tidyr`, `car`, `MASS`  
  - Install with `install.packages(c("ggplot2","dplyr","reshape2","tidyr","car","MASS"))`

## Results

- **Statistical summaries** of Kittiwake observations
- **ANOVA** indicating whether decline in breeding pairs is site-dependent
- **Correlation and t-tests** on measurements for different sub-species
- **Linear vs. log-linear models** comparing breeding pairs across environmental factors

## Author

- **Sanyog Chavhan**  
  MSc Student, Data Science & Statistics

## Cloning

1. **Clone the repository**:
   ```bash
   git clone https://github.com/YOUR_USERNAME/Kittiwake-Analysis.git
