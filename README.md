# lcp-health-heliosx-real-world-effectiveness-patterns
Analytical code used to support the manuscript Real-World Effectiveness and Affordability-Driven Treatment Patterns of GLP-1 Receptor Agonists in Adults with Overweight or Obesity: A Natural Experiment in One Million Self-Paying Adults in the United Kingdom 


# Order of running

1. First run the script 'data_prep.R'. This performs the exclusions and creates various flags for switching, discontinuation, persistence as well as gathering weight information at different time points
2. run 'summarise.R' next. This contains functions to perform the descriptive demographic statistics on various subsets of the cohort established in 'data_prep'
3. Run 'pct_weight_loss_summary.R' This calculates various averages (mean, median) for weights, BMIs and percentage weight lost at different time points for the cohort
4. Run 'logistic_regression_function_starters.R'; this creates the model output for predictive factors associated with selecting Mounjaro over Wegovy at first order in both the pre- and post- price rise periods
5. Run 'tables.R' to create tables which directly correlate to those included in the manuscript and/or supplementary materials.