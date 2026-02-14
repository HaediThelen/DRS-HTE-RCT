# DRS-HTE-RCT

This repository contains code to reproduce the simulation study of disease risk score (DRS) methods for assessing heterogeneity of treatment effects (HTE) in randomized controlled trials.

------------------------------------------------------------------------

## Repository structure

-   **`scripts/`** – main analysis scripts and plotting scripts.\
    Start here if you want to reproduce results. See the [`scripts/README.md`](scripts/README.md) for details.\
-   **`functions/`** – helper functions used by the scripts.\
-   **`results/`** – output directory.\
    This is generated automatically when scripts are run and is empty in the public repo (except for a the README.md file).

------------------------------------------------------------------------

## Notes

-   This is the **public version** of the code. Exploratory and archived code has been excluded for clarity.\
-   Data are **simulated** within the scripts; no real patient data are included.\
-   Results (CSV/PDF files) are generated locally when scripts are run and are not tracked in the repository.

------------------------------------------------------------------------

## Requirements

This project uses an **R environment managed with [`renv`](https://rstudio.github.io/renv/)** to ensure reproducibility.

-   To set up the same environment used for the simulations, run:

\`\`\`r install.packages("renv") renv::restore()
