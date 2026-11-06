# Reproducibility Package: Intermittent Demand Forecasting for Paper Sales

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R Version](https://img.shields.io/badge/R-v4.4.2-blue.svg)](https://www.r-project.org/)

This repository contains the complete R code pipeline, parameter settings, curated datasets, and evaluation scripts required to reproduce all empirical findings, figures, and statistical tests reported in the manuscript **"Intermittent Demand Forecasting Performance Across Syntetos-Boylan Categories in Paper Product Sales"** submitted to *CAUCHY: Jurnal Matematika Murni dan Aplikasi*.

---

## 💻 Software Requirements
All computations were executed using **R (version 4.4.2)** and **RStudio**.

### Required R Packages
```R
install.packages(c("readxl", "openxlsx", "writexl", "dplyr", "tidyr", "ggplot2", "forecast", "tsintermittent", "PMCMRplus"))
```

---

## 📁 Repository Structure & Script Pipeline

```text
.
├── data/
│   ├── raw_paper_sales.csv            # Original Kaggle dataset
│   └── Data_Kertas_Lolos_Screening.xlsx # Filtered dataset (23 SKUs)
├── R/
│   └── main_analysis.R                # Single unified R script with section headers
└── outputs/
    ├── figures/                       # Generated plots
    └── tables/                        # Formatted Excel/LaTeX tables
```

### Overview of `R/main_analysis.R` Sections:
1. **Section 1: Data Preprocessing & Screening:** Filters initial sales observations (positive sales threshold).
2. **Section 2: ADI & $CV^2$ Categorization:** Computes demand characteristics strictly using the first 50-week training period per SKU to avoid look-ahead leakage.
3. **Section 3: Rolling-Origin Forecasting:** Executes 10 rolling origins ($h=4$) for ARIMA, ETS, Croston, and SBA models; computes MAE and MASE metrics.
4. **Section 4: Inferential Statistical Tests:** Executes Fisher's Exact Test (contingency analysis), Friedman test (mean ranks), and post-hoc Nemenyi pairwise comparisons.

---

## 🚀 Instructions for Reproduction

To replicate all outputs reported in the paper:

1. **Clone or Download the Repository:**
   ```bash
   git clone [https://github.com/username/paper-sales-forecasting-reproducibility.git](https://github.com/username/paper-sales-forecasting-reproducibility.git)
   cd paper-sales-forecasting-reproducibility
   ```

2. **Open Project & Run Script:**
   - Open `paper-sales-forecasting.Rproj` or set working directory to project root in RStudio.
   - Run the complete analysis script:
     ```R
     source("R/Script_cauchy_AD CV^2.R")
     ```

3. **Outputs Location:**
   - Evaluated metrics (MAE, MASE, Contingency tables, Friedman ranks) are saved to `outputs/tables/` and exported as Excel workbooks.
   - Generated visualizations are saved to `outputs/figures/`.