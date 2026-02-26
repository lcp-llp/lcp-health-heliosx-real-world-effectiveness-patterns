# Analytical Code: Real-World Effectiveness and Affordability-Driven Treatment Patterns of GLP-1 Receptor Agonists in Adults with Overweight or Obesity  
*A Natural Experiment in One Million Self-Paying Adults in the United Kingdom*

## Overview

This repository contains the analytical R code provided as a technical supplement to the manuscript:

**“Real-World Effectiveness and Affordability-Driven Treatment Patterns of GLP-1 Receptor Agonists in Adults with Overweight or Obesity: A Natural Experiment in One Million Self-Paying Adults in the United Kingdom.”**

The study is a retrospective cohort analysis of UK adults prescribed subcutaneous semaglutide or tirzepatide for overweight or obesity via the digital health platform *MedExpress*. Patient demographics, weight-loss outcomes, and patterns of treatment initiation, switching, persistence, and discontinuation were analysed before and after a treatment price increase.

Multivariable logistic regression models were used to examine associations between demographic and clinical factors and initial treatment choice.

## Contacts

- **Anna Mulligan** – anna.mulligan@lcp.uk.com  
- **Mattia Ficarelli, PhD** – mattia.ficarelli@lcp.uk.com  
- **Ben Bray, MBChB** – ben.bray@lcp.uk.com  

## Repository Contents

### Programming Scripts

- `data_prep.R`  
  Performs cohort construction, exclusions, and derivation of key variables, including switching, discontinuation, persistence, and weight measurements at multiple time points.

- `summarise.R`  
  Contains functions for generating descriptive demographic statistics across defined cohort subsets.

- `pct_weight_loss_summary.R`  
  Calculates summary statistics (mean, median) for weight, BMI, and percentage weight loss at specified follow-up time points.

- `logistic_regression_function_starters.R`  
  Generates multivariable logistic regression model outputs assessing predictors of initial treatment selection (tirzepatide vs semaglutide) in both pre- and post-price increase periods.

- `tables.R`  
  Produces tables corresponding directly to those reported in the manuscript and supplementary materials.

- `requirements.R`  
  Lists and loads all R packages required to run the analyses.

## Order of Operations

Scripts should be run in the following order:

1. **`data_prep.R`**  
   Constructs the analytic cohort and derives key exposure, outcome, and follow-up variables.

2. **`summarise.R`**  
   Generates descriptive demographic summaries for the cohort and relevant subgroups.

3. **`pct_weight_loss_summary.R`**  
   Computes weight, BMI, and percentage weight-loss summaries at each time point.

4. **`logistic_regression_function_starters.R`**  
   Fits regression models assessing predictors of initial treatment choice before and after the price rise.

5. **`tables.R`**  
   Creates manuscript-ready tables for main and supplementary analyses.

---

**Note:** All analyses were conducted using R. Scripts assume access to the underlying study dataset, which is not included in this repository.