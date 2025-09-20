# DRS-HTE-RCT

This folder contains the scripts to run the simulation study described in the manuscript.  

Results are saved locally under the `results/` directory. Make sure to run the steps in order, as later scripts depend on output from earlier ones.  

---

## Step 1: Main analysis (Manuscript Part 1)

- **`main_analysis.R`**  
  Run this script first to generate data for the base case and all trial settings (`a`–`n`) described in the supplement (defined in `trial_settings.R`).  
  - Output: Results saved in `results/`.  
  - The **base_case** scenario generates **Figure 2**.  

---

## Step 2: Trends across trial settings

- **`trends_plots.R`**  
  Produces **Figure 3**.  
  - Requires results for all scenarios (`base_case` and `a`–`n`) from Step 1.  

---

## Step 3: Optimizing the split-sample (RSS) method

### 3a. Varying % of controls used to train the DRS model

- **`main_alt_ss_drs_train_pct.R`**  
  Run for trial sizes of **500**, **1,000**, and **3,600 (base case)**.  
  Store results.  

- **`alt_ss_train_pct_summary_plot.R`**  
  Uses stored results to generate **Figure 4**.  

### 3b. Comparing LOO vs. repeated split sample

- **`LOO.R`** and **`repeated_SS.R`**  
  Run both and store results.  
  -  **Note**: `LOO.R` is slow — ~15 minutes per treatment effect scenario using 10 cores on a MacBook Pro (M1, 32 GB RAM). Runtime will vary with hardware.  

- **`LOO_RSS_Fig.R`**  
  Uses stored results to generate **Figure 5**.  

---

## Step 4: Supplementary analyses — quartile stratification

- **`main_quantile_approach.R`**  
  Generates **Supplemental Figure B2**. Store results.  

- **`quantile_trends_plots.R`**  
  Uses stored results to generate **Supplemental Figure B3**.  

---
  
