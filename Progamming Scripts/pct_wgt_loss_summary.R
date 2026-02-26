# This script calculates the percentage weight loss at 3, 6, 9 and
# 12 months for each product and switching segment, and creates
# print-ready fields combining mean, CI and N. It also calculates
# the average BMI and weight at baseline for each product and
# switching segment.



# Subset the customers dataset to include only those with non-missing
# BMI values at each time point, as percentage weight loss can only be
# calculated for those with a baseline BMI and weight.
customers_w_3m_bmi <- customers[!is.na(customers$WEIGHT_3M), ]
customers_w_6m_bmi <- customers[!is.na(customers$WEIGHT_6M), ]
customers_w_9m_bmi <- customers[!is.na(customers$WEIGHT_9M), ]
customers_w_12m_bmi <- customers[!is.na(customers$WEIGHT_12M), ]

library(dplyr)
library(purrr)
## Function to get the median and IQR given a group_by and measurable attribute
summary_median <- function(source, gb, attribute, gbs, all_label) {
  r <- source %>%
    group_by({{ gb }}) %>%
    summarise(
      median_w = median({{ attribute }}),
      n = n(),
      q1 = quantile({{ attribute }}, 0.25, na.rm = TRUE),
      q3 = quantile({{ attribute }}, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(IQR = q3 - q1) %>%
    select({{ gb }}, median_w, IQR, n, q1, q3)
  t <- source
  t[[gbs]] <- all_label
  r2 <- t %>%
    group_by({{ gb }}) %>%
    summarise(
      median_w = median({{ attribute }}),
      n = n(),
      q1 = quantile({{ attribute }}, 0.25, na.rm = TRUE),
      q3 = quantile({{ attribute }}, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(IQR = q3 - q1) %>%
    select({{ gb }}, median_w, IQR, n, q1, q3)
  bind_rows(r, r2)
}
## Function to get the mean and 95% CI given a group_by and measurable attribute
sm <- function(source, gb, attribute, gbs, all_label) {
  r <- source %>%
    group_by({{ gb }}) %>%
    summarise(
      mean_pct_weight_loss = mean({{ attribute }}),
      n = n(),
      sd_pct_weight_lost = sd({{ attribute }}),
      .groups = "drop"
    ) %>%
    mutate(
      se = sd_pct_weight_lost / sqrt(n),
      ci_lower = mean_pct_weight_loss - qt(0.975, df = n - 1) * se,
      ci_upper = mean_pct_weight_loss + qt(0.975, df = n - 1) * se
    )
  t <- source
  t[[gbs]] <- all_label
  r2 <- t %>%
    group_by({{ gb }}) %>%
    summarise(
      mean_pct_weight_loss = mean({{ attribute }}),
      n = n(),
      sd_pct_weight_lost = sd({{ attribute }}),
      .groups = "drop"
    ) %>%
    mutate(
      se = sd_pct_weight_lost / sqrt(n),
      ci_lower = mean_pct_weight_loss - qt(0.975, df = n - 1) * se,
      ci_upper = mean_pct_weight_loss + qt(0.975, df = n - 1) * se
    )
  bind_rows(r, r2)
}

# Find the mean % weight lost at 3 months and create a print-ready field
pct_w_lost_3m <- sm(
  customers_w_3m_bmi, PRODUCT_BRAND, PCT_WGT_LOSS_3M,
  "PRODUCT_BRAND", "All GLP-1 RA Users"
)
col_3m <- "Percentage weight loss at 3 Months (mean, 95% CI, n)"
pct_w_lost_3m[[col_3m]] <- paste0(
  round(pct_w_lost_3m$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_3m$ci_upper, 2),
  " - ", round(pct_w_lost_3m$ci_lower, 2), ", ", pct_w_lost_3m$n
)
# Find the mean % weight lost at 6 months and create a print-ready field
pct_w_lost_6m <- sm(
  customers_w_6m_bmi, PRODUCT_BRAND, PCT_WGT_LOSS_6M,
  "PRODUCT_BRAND", "All GLP-1 RA Users"
)
col_6m <- "Percentage weight loss at 6 Months (mean, 95% CI, n)"
pct_w_lost_6m[[col_6m]] <- paste0(
  round(pct_w_lost_6m$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_6m$ci_upper, 2),
  " - ", round(pct_w_lost_6m$ci_lower, 2), ", ", pct_w_lost_6m$n
)
# Find the mean % weight lost at 9 months and create a print-ready field
pct_w_lost_9m <- sm(
  customers_w_9m_bmi, PRODUCT_BRAND, PCT_WGT_LOSS_9M,
  "PRODUCT_BRAND", "All GLP-1 RA Users"
)
col_9m <- "Percentage weight loss at 9 Months (mean, 95% CI, n)"
pct_w_lost_9m[[col_9m]] <- paste0(
  round(pct_w_lost_9m$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_9m$ci_upper, 2),
  " - ", round(pct_w_lost_9m$ci_lower, 2), ", ", pct_w_lost_9m$n
)
# Find the mean % weight lost at 12 months and create a print-ready field
pct_w_lost_12m <- sm(
  customers_w_12m_bmi, PRODUCT_BRAND, PCT_WGT_LOSS_12M,
  "PRODUCT_BRAND", "All GLP-1 RA Users"
)
col_12m <- "Percentage weight loss at 12 Months (mean, 95% CI, n)"
pct_w_lost_12m[[col_12m]] <- paste0(
  round(pct_w_lost_12m$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_12m$ci_upper, 2),
  " - ", round(pct_w_lost_12m$ci_lower, 2), ", ", pct_w_lost_12m$n
)

# Join all months together
tables <- list(pct_w_lost_12m, pct_w_lost_3m, pct_w_lost_6m, pct_w_lost_9m)
pct_w_lost_all <- reduce(tables, full_join, by = "PRODUCT_BRAND")[, c(
  "PRODUCT_BRAND",
  "Percentage weight loss at 3 Months (mean, 95% CI, n)",
  "Percentage weight loss at 6 Months (mean, 95% CI, n)",
  "Percentage weight loss at 9 Months (mean, 95% CI, n)",
  "Percentage weight loss at 12 Months (mean, 95% CI, n)"
)]
# Renaming fields to print
pct_w_lost_all <- pct_w_lost_all %>%
  rename(Group = PRODUCT_BRAND)

pct_w_lost_all$Group <- ifelse(
  pct_w_lost_all$Group == "Wegovy", "Started Wegovy",
  ifelse(
    pct_w_lost_all$Group == "Mounjaro", "Started Mounjaro",
    pct_w_lost_all$Group
  )
)


#################################################################
###### Switchers ################################################
#################################################################
pct_w_lost_3m_switchers <- sm(
  customers_w_3m_bmi[
    !is.na(customers_w_3m_bmi$CURRENT_SWITCHING_SEGMENT),
  ],
  CURRENT_SWITCHING_SEGMENT, PCT_WGT_LOSS_3M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
col_sw_3m <- "Percentage weight loss at 3 Months (mean, 95% CI, n)"
pct_w_lost_3m_switchers[[col_sw_3m]] <- paste0(
  round(pct_w_lost_3m_switchers$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_3m_switchers$ci_upper, 2),
  " - ", round(pct_w_lost_3m_switchers$ci_lower, 2),
  ", ", pct_w_lost_3m_switchers$n
)

pct_w_lost_6m_switchers <- sm(
  customers_w_6m_bmi[
    !is.na(customers_w_6m_bmi$CURRENT_SWITCHING_SEGMENT),
  ],
  CURRENT_SWITCHING_SEGMENT, PCT_WGT_LOSS_6M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
col_sw_6m <- "Percentage weight loss at 6 Months (mean, 95% CI, n)"
pct_w_lost_6m_switchers[[col_sw_6m]] <- paste0(
  round(pct_w_lost_6m_switchers$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_6m_switchers$ci_upper, 2),
  " - ", round(pct_w_lost_6m_switchers$ci_lower, 2),
  ", ", pct_w_lost_6m_switchers$n
)
pct_w_lost_9m_switchers <- sm(
  customers_w_9m_bmi[
    !is.na(customers_w_9m_bmi$CURRENT_SWITCHING_SEGMENT),
  ],
  CURRENT_SWITCHING_SEGMENT, PCT_WGT_LOSS_9M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
col_sw_9m <- "Percentage weight loss at 9 Months (mean, 95% CI, n)"
pct_w_lost_9m_switchers[[col_sw_9m]] <- paste0(
  round(pct_w_lost_9m_switchers$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_9m_switchers$ci_upper, 2),
  " - ", round(pct_w_lost_9m_switchers$ci_lower, 2),
  ", ", pct_w_lost_9m_switchers$n
)
pct_w_lost_12m_switchers <- sm(
  customers_w_12m_bmi[
    !is.na(customers_w_12m_bmi$CURRENT_SWITCHING_SEGMENT),
  ],
  CURRENT_SWITCHING_SEGMENT, PCT_WGT_LOSS_12M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
col_sw_12m <- "Percentage weight loss at 12 Months (mean, 95% CI, n)"
pct_w_lost_12m_switchers[[col_sw_12m]] <- paste0(
  round(pct_w_lost_12m_switchers$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_12m_switchers$ci_upper, 2),
  " - ", round(pct_w_lost_12m_switchers$ci_lower, 2),
  ", ", pct_w_lost_12m_switchers$n
)
tables <- list(
  pct_w_lost_12m_switchers, pct_w_lost_3m_switchers,
  pct_w_lost_6m_switchers, pct_w_lost_9m_switchers
)
pct_w_lost_all_switchers <- reduce(
  tables, full_join, by = "CURRENT_SWITCHING_SEGMENT"
)[, c(
  "CURRENT_SWITCHING_SEGMENT",
  "Percentage weight loss at 3 Months (mean, 95% CI, n)",
  "Percentage weight loss at 6 Months (mean, 95% CI, n)",
  "Percentage weight loss at 9 Months (mean, 95% CI, n)",
  "Percentage weight loss at 12 Months (mean, 95% CI, n)"
)]

pct_w_lost_all_switchers <- pct_w_lost_all_switchers %>%
  rename(Group = CURRENT_SWITCHING_SEGMENT)
pct_w_lost_all_switchers$Group <- ifelse(
  pct_w_lost_all_switchers$Group == "switched_wegovy_to_mounjaro",
  "Switched Wegovy to Mounjaro",
  ifelse(
    pct_w_lost_all_switchers$Group == "switched_mounjaro_to_wegovy",
    "Switched Mounjaro to Wegovy",
    pct_w_lost_all_switchers$Group
  )
)

#################################################################
###### Remainers ################################################
#################################################################
pct_w_lost_3m_remainers <- sm(
  customers_w_3m_bmi[customers_w_3m_bmi$SWITCHED_BRANDS_FLAG == 0, ],
  PRODUCT_BRAND, PCT_WGT_LOSS_3M,
  "PRODUCT_BRAND", "All Retainers"
)
pct_w_lost_3m_remainers[[col_3m]] <- paste0(
  round(pct_w_lost_3m_remainers$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_3m_remainers$ci_upper, 2),
  " - ", round(pct_w_lost_3m_remainers$ci_lower, 2),
  ", ", pct_w_lost_3m_remainers$n
)

pct_w_lost_6m_remainers <- sm(
  customers_w_6m_bmi[customers_w_6m_bmi$SWITCHED_BRANDS_FLAG == 0, ],
  PRODUCT_BRAND, PCT_WGT_LOSS_6M,
  "PRODUCT_BRAND", "All Retainers"
)
pct_w_lost_6m_remainers[[col_6m]] <- paste0(
  round(pct_w_lost_6m_remainers$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_6m_remainers$ci_upper, 2),
  " - ", round(pct_w_lost_6m_remainers$ci_lower, 2),
  ", ", pct_w_lost_6m_remainers$n
)

pct_w_lost_9m_remainers <- sm(
  customers_w_9m_bmi[customers_w_9m_bmi$SWITCHED_BRANDS_FLAG == 0, ],
  PRODUCT_BRAND, PCT_WGT_LOSS_9M,
  "PRODUCT_BRAND", "All Retainers"
)
pct_w_lost_9m_remainers[[col_9m]] <- paste0(
  round(pct_w_lost_9m_remainers$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_9m_remainers$ci_upper, 2),
  " - ", round(pct_w_lost_9m_remainers$ci_lower, 2),
  ", ", pct_w_lost_9m_remainers$n
)

pct_w_lost_12m_remainers <- sm(
  customers_w_12m_bmi[
    customers_w_12m_bmi$SWITCHED_BRANDS_FLAG == 0,
  ],
  PRODUCT_BRAND, PCT_WGT_LOSS_12M,
  "PRODUCT_BRAND", "All Retainers"
)
pct_w_lost_12m_remainers[[col_12m]] <- paste0(
  round(pct_w_lost_12m_remainers$mean_pct_weight_loss, 2), ", ",
  round(pct_w_lost_12m_remainers$ci_upper, 2),
  " - ", round(pct_w_lost_12m_remainers$ci_lower, 2),
  ", ", pct_w_lost_12m_remainers$n
)
tables <- list(
  pct_w_lost_12m_remainers, pct_w_lost_3m_remainers,
  pct_w_lost_6m_remainers, pct_w_lost_9m_remainers
)
pct_w_lost_all_remainers <- reduce(
  tables, full_join, by = "PRODUCT_BRAND"
)[, c(
  "PRODUCT_BRAND",
  "Percentage weight loss at 3 Months (mean, 95% CI, n)",
  "Percentage weight loss at 6 Months (mean, 95% CI, n)",
  "Percentage weight loss at 9 Months (mean, 95% CI, n)",
  "Percentage weight loss at 12 Months (mean, 95% CI, n)"
)]
pct_w_lost_all_remainers <- pct_w_lost_all_remainers %>%
  rename(Group = PRODUCT_BRAND)

pct_w_lost_all_remainers$Group <- ifelse(
  pct_w_lost_all_remainers$Group == "Wegovy",
  "Retained Wegovy",
  ifelse(
    pct_w_lost_all_remainers$Group == "Mounjaro",
    "Retained Mounjaro",
    pct_w_lost_all_remainers$Group
  )
)


#################################################################
###### Average BMI and Weight ###########################################
#################################################################
av_weight <- summary_median(
  customers, PRODUCT_BRAND, WEIGHT_0M,
  "PRODUCT_BRAND", "All Products"
)
# SC Check 29/12/2025 - For median (IQR) I would paste q1 - q3
# between the brackets instead of the IQR value
av_weight$weight <- paste0(
  av_weight$median_w, " (", av_weight$q1, " - ", av_weight$q3, ")"
)
av_weight <- av_weight[, c("PRODUCT_BRAND", "weight")]
av_weight <- av_weight %>%
  pivot_longer(
    cols = c(weight),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = PRODUCT_BRAND, values_from = value)

av_bmi <- summary_median(
  customers, PRODUCT_BRAND, BMI_0M,
  "PRODUCT_BRAND", "All Products"
)
av_bmi$BMI <- paste0(
  av_bmi$median_w, " (", av_bmi$q1, " - ", av_bmi$q3, ")"
)
av_bmi <- av_bmi[, c("PRODUCT_BRAND", "BMI")]
av_bmi <- av_bmi %>%
  pivot_longer(
    cols = c(BMI),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = PRODUCT_BRAND, values_from = value)

############################
## Early Discontinuers
############################
av_weight_ed <- summary_median(
  customers[customers$early_disc == 1, ], PRODUCT_BRAND, WEIGHT_0M,
  "PRODUCT_BRAND", "All Products"
)
av_weight_ed$weight <- paste0(
  av_weight_ed$median_w, " (", av_weight_ed$q1,
  " - ", av_weight_ed$q3, ")"
)
av_weight_ed <- av_weight_ed[, c("PRODUCT_BRAND", "weight")]
av_weight_ed <- av_weight_ed %>%
  pivot_longer(
    cols = c(weight),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = PRODUCT_BRAND, values_from = value)

av_bmi_ed <- summary_median(
  customers[customers$early_disc == 1, ], PRODUCT_BRAND, BMI_0M,
  "PRODUCT_BRAND", "All Products"
)
av_bmi_ed$BMI <- paste0(
  av_bmi_ed$median_w, " (", av_bmi_ed$q1, " - ", av_bmi_ed$q3, ")"
)
av_bmi_ed <- av_bmi_ed[, c("PRODUCT_BRAND", "BMI")]
av_bmi_ed <- av_bmi_ed %>%
  pivot_longer(
    cols = c(BMI),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = PRODUCT_BRAND, values_from = value)


############################
##NOT Early Discontinuers
############################
av_weight_ned <- summary_median(
  customers[customers$early_disc != 1, ], PRODUCT_BRAND, WEIGHT_0M,
  "PRODUCT_BRAND", "All Products"
)
av_weight_ned$weight <- paste0(
  av_weight_ned$median_w, " (", av_weight_ned$q1,
  " - ", av_weight_ned$q3, ")"
)
av_weight_ned <- av_weight_ned[, c("PRODUCT_BRAND", "weight")]
av_weight_ned <- av_weight_ned %>%
  pivot_longer(
    cols = c(weight),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = PRODUCT_BRAND, values_from = value)

av_bmi_ned <- summary_median(
  customers[customers$early_disc != 1, ], PRODUCT_BRAND, BMI_0M,
  "PRODUCT_BRAND", "All Products"
)
av_bmi_ned$BMI <- paste0(
  av_bmi_ned$median_w, " (", av_bmi_ned$q1, " - ", av_bmi_ned$q3, ")"
)
av_bmi_ned <- av_bmi_ned[, c("PRODUCT_BRAND", "BMI")]
av_bmi_ned <- av_bmi_ned %>%
  pivot_longer(
    cols = c(BMI),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = PRODUCT_BRAND, values_from = value)


############################
##Switched post ph
############################
sw_post_filter <- customers$switch_state == "After Price Rise" &
  !is.na(customers$switch_state)
av_weight_switchers_post_ph <- summary_median(
  customers[sw_post_filter, ],
  CURRENT_SWITCHING_SEGMENT, WEIGHT_0M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_weight_switchers_post_ph$weight <- paste0(
  av_weight_switchers_post_ph$median_w, " (",
  av_weight_switchers_post_ph$q1, " - ",
  av_weight_switchers_post_ph$q3, ")"
)
av_weight_switchers_post_ph <- av_weight_switchers_post_ph[
  , c("CURRENT_SWITCHING_SEGMENT", "weight")
]
av_weight_switchers_post_ph <- av_weight_switchers_post_ph %>%
  pivot_longer(
    cols = c(weight),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)

av_bmi_switchers_post_ph <- summary_median(
  customers[sw_post_filter, ],
  CURRENT_SWITCHING_SEGMENT, BMI_0M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_bmi_switchers_post_ph$BMI <- paste0(
  av_bmi_switchers_post_ph$median_w, " (",
  av_bmi_switchers_post_ph$q1, " - ",
  av_bmi_switchers_post_ph$q3, ")"
)
av_bmi_switchers_post_ph <- av_bmi_switchers_post_ph[
  , c("CURRENT_SWITCHING_SEGMENT", "BMI")
]
av_bmi_switchers_post_ph <- av_bmi_switchers_post_ph %>%
  pivot_longer(
    cols = c(BMI),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)


############################
##Switched pre ph
############################
sw_pre_filter <- customers$switch_state == "Before Price Rise" &
  !is.na(customers$switch_state)
av_weight_switchers_pre_ph <- summary_median(
  customers[sw_pre_filter, ],
  CURRENT_SWITCHING_SEGMENT, WEIGHT_0M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_weight_switchers_pre_ph$weight <- paste0(
  av_weight_switchers_pre_ph$median_w, " (",
  av_weight_switchers_pre_ph$q1, " - ",
  av_weight_switchers_pre_ph$q3, ")"
)
av_weight_switchers_pre_ph <- av_weight_switchers_pre_ph[
  , c("CURRENT_SWITCHING_SEGMENT", "weight")
]
av_weight_switchers_pre_ph <- av_weight_switchers_pre_ph %>%
  pivot_longer(
    cols = c(weight),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)

av_bmi_switchers_pre_ph <- summary_median(
  customers[sw_pre_filter, ],
  CURRENT_SWITCHING_SEGMENT, BMI_0M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_bmi_switchers_pre_ph$BMI <- paste0(
  av_bmi_switchers_pre_ph$median_w, " (",
  av_bmi_switchers_pre_ph$q1, " - ",
  av_bmi_switchers_pre_ph$q3, ")"
)
av_bmi_switchers_pre_ph <- av_bmi_switchers_pre_ph[
  , c("CURRENT_SWITCHING_SEGMENT", "BMI")
]
av_bmi_switchers_pre_ph <- av_bmi_switchers_pre_ph %>%
  pivot_longer(
    cols = c(BMI),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)


############################
##Switched post ph -  at switch
############################
wl_col <- "Percentage weight loss from baseline at switch"
av_wt_sph_sw <- summary_median(
  customers[sw_post_filter, ],
  CURRENT_SWITCHING_SEGMENT, switching_weight,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_wt_sph_sw$weight <- paste0(
  av_wt_sph_sw$median_w, " (",
  av_wt_sph_sw$q1, " - ",
  av_wt_sph_sw$q3, ")"
)
av_wt_sph_sw <- av_wt_sph_sw[
  , c("CURRENT_SWITCHING_SEGMENT", "weight")
]
av_wt_sph_sw <- av_wt_sph_sw %>%
  pivot_longer(
    cols = c(weight),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)

av_bmi_sph_sw <- summary_median(
  customers[sw_post_filter, ],
  CURRENT_SWITCHING_SEGMENT, switching_BMI,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_bmi_sph_sw$BMI <- paste0(
  av_bmi_sph_sw$median_w, " (",
  av_bmi_sph_sw$q1, " - ",
  av_bmi_sph_sw$q3, ")"
)
av_bmi_sph_sw <- av_bmi_sph_sw[
  , c("CURRENT_SWITCHING_SEGMENT", "BMI")
]
av_bmi_sph_sw <- av_bmi_sph_sw %>%
  pivot_longer(
    cols = c(BMI),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)
pct_w_lost_post_ph_at_switch <- sm(
  customers[sw_post_filter, ],
  CURRENT_SWITCHING_SEGMENT, switching_pct_weight_loss,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
pct_w_lost_post_ph_at_switch[[wl_col]] <- paste0(
  round(pct_w_lost_post_ph_at_switch$mean_pct_weight_loss * 100, 2),
  ", ",
  round(pct_w_lost_post_ph_at_switch$ci_lower * 100, 2),
  " - ",
  round(pct_w_lost_post_ph_at_switch$ci_upper * 100, 2)
)
pct_w_lost_post_ph_at_switch <- pct_w_lost_post_ph_at_switch[
  , c("CURRENT_SWITCHING_SEGMENT", wl_col)
]
pct_w_lost_post_ph_at_switch <- pct_w_lost_post_ph_at_switch %>%
  pivot_longer(
    cols = c(wl_col),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)

av_days_sph_sw <- summary_median(
  customers[sw_post_filter, ],
  CURRENT_SWITCHING_SEGMENT, switching_days,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_days_sph_sw$days <- paste0(
  av_days_sph_sw$median_w, " (",
  av_days_sph_sw$q1, " - ",
  av_days_sph_sw$q3, ")"
)
av_days_sph_sw <- av_days_sph_sw[
  , c("CURRENT_SWITCHING_SEGMENT", "days")
]
av_days_sph_sw <- av_days_sph_sw %>%
  pivot_longer(
    cols = c(days),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)


############################
##Switched pre ph - at switch
############################
av_wt_spr_sw <- summary_median(
  customers[sw_pre_filter, ],
  CURRENT_SWITCHING_SEGMENT, switching_weight,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_wt_spr_sw$weight <- paste0(
  av_wt_spr_sw$median_w, " (",
  av_wt_spr_sw$q1, " - ",
  av_wt_spr_sw$q3, ")"
)
av_wt_spr_sw <- av_wt_spr_sw[
  , c("CURRENT_SWITCHING_SEGMENT", "weight")
]
av_wt_spr_sw <- av_wt_spr_sw %>%
  pivot_longer(
    cols = c(weight),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)

av_bmi_spr_sw <- summary_median(
  customers[sw_pre_filter, ],
  CURRENT_SWITCHING_SEGMENT, switching_BMI,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_bmi_spr_sw$BMI <- paste0(
  av_bmi_spr_sw$median_w, " (",
  av_bmi_spr_sw$q1, " - ",
  av_bmi_spr_sw$q3, ")"
)
av_bmi_spr_sw <- av_bmi_spr_sw[
  , c("CURRENT_SWITCHING_SEGMENT", "BMI")
]
av_bmi_spr_sw <- av_bmi_spr_sw %>%
  pivot_longer(
    cols = c(BMI),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)

pct_w_lost_pre_ph_at_switch <- sm(
  customers[sw_pre_filter, ],
  CURRENT_SWITCHING_SEGMENT, switching_pct_weight_loss,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
pct_w_lost_pre_ph_at_switch[[wl_col]] <- paste0(
  round(pct_w_lost_pre_ph_at_switch$mean_pct_weight_loss * 100, 2),
  ", ",
  round(pct_w_lost_pre_ph_at_switch$ci_lower * 100, 2),
  " - ",
  round(pct_w_lost_pre_ph_at_switch$ci_upper * 100, 2)
)
pct_w_lost_pre_ph_at_switch <- pct_w_lost_pre_ph_at_switch[
  , c("CURRENT_SWITCHING_SEGMENT", wl_col)
]
pct_w_lost_pre_ph_at_switch <- pct_w_lost_pre_ph_at_switch %>%
  pivot_longer(
    cols = c(wl_col),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)


av_days_spr_sw <- summary_median(
  customers[sw_pre_filter, ],
  CURRENT_SWITCHING_SEGMENT, switching_days,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
av_days_spr_sw$days <- paste0(
  av_days_spr_sw$median_w, " (",
  av_days_spr_sw$q1, " - ",
  av_days_spr_sw$q3, ")"
)
av_days_spr_sw <- av_days_spr_sw[
  , c("CURRENT_SWITCHING_SEGMENT", "days")
]
av_days_spr_sw <- av_days_spr_sw %>%
  pivot_longer(
    cols = c(days),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = CURRENT_SWITCHING_SEGMENT, values_from = value)

#####################
### Weight loss velocity
#############

customers_w_3m_vel <- customers[
  !is.na(customers$wl_vel_3M) & customers$wl_vel_3M > 0,
]
customers_w_6m_vel <- customers[
  !is.na(customers$wl_vel_6M) & customers$wl_vel_6M > 0,
]
customers_w_9m_vel <- customers[
  !is.na(customers$wl_vel_9M) & customers$wl_vel_9M > 0,
]
customers_w_12m_vel <- customers[
  !is.na(customers$wl_vel_12M) & customers$wl_vel_12M > 0,
]
# Find the mean % weight lost at 3 months and create a print-ready field
# SC Check - 29/12/2025: Why am I getting Inf and NaNs
wl_vel_3m <- sm(
  customers_w_3m_vel, PRODUCT_BRAND, wl_vel_3M,
  "PRODUCT_BRAND", "All GLP-1 RA Users"
)
wl_vel_3m[[col_3m]] <- paste0(
  round(wl_vel_3m$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_3m$ci_upper, 2),
  " - ", round(wl_vel_3m$ci_lower, 2), ", ", wl_vel_3m$n
)
# Find the mean % weight lost at 6 months and create a print-ready field
wl_vel_6m <- sm(
  customers_w_6m_vel, PRODUCT_BRAND, wl_vel_6M,
  "PRODUCT_BRAND", "All GLP-1 RA Users"
)
wl_vel_6m[[col_6m]] <- paste0(
  round(wl_vel_6m$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_6m$ci_upper, 2),
  " - ", round(wl_vel_6m$ci_lower, 2), ", ", wl_vel_6m$n
)
# Find the mean % weight lost at 9 months and create a print-ready field
wl_vel_9m <- sm(
  customers_w_9m_vel, PRODUCT_BRAND, wl_vel_9M,
  "PRODUCT_BRAND", "All GLP-1 RA Users"
)
wl_vel_9m[[col_9m]] <- paste0(
  round(wl_vel_9m$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_9m$ci_upper, 2),
  " - ", round(wl_vel_9m$ci_lower, 2), ", ", wl_vel_9m$n
)
# Find the mean % weight lost at 12 months and create a print-ready field
wl_vel_12m <- sm(
  customers_w_12m_vel, PRODUCT_BRAND, wl_vel_12M,
  "PRODUCT_BRAND", "All GLP-1 RA Users"
)
wl_vel_12m[[col_12m]] <- paste0(
  round(wl_vel_12m$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_12m$ci_upper, 2),
  " - ", round(wl_vel_12m$ci_lower, 2), ", ", wl_vel_12m$n
)

# Join all months together
tables <- list(wl_vel_12m, wl_vel_3m, wl_vel_6m, wl_vel_9m)
wl_vel_all <- reduce(tables, full_join, by = "PRODUCT_BRAND")[, c(
  "PRODUCT_BRAND",
  "Percentage weight loss at 3 Months (mean, 95% CI, n)",
  "Percentage weight loss at 6 Months (mean, 95% CI, n)",
  "Percentage weight loss at 9 Months (mean, 95% CI, n)",
  "Percentage weight loss at 12 Months (mean, 95% CI, n)"
)]
# Renaming fields to print
wl_vel_all <- wl_vel_all %>%
  rename(Group = PRODUCT_BRAND)

wl_vel_all$Group <- ifelse(
  wl_vel_all$Group == "Wegovy", "Started Wegovy",
  ifelse(
    wl_vel_all$Group == "Mounjaro", "Started Mounjaro",
    wl_vel_all$Group
  )
)


#################################################################
###### Switchers ################################################
#################################################################
wl_vel_3m_switchers <- sm(
  customers_w_3m_bmi[
    !is.na(customers_w_3m_bmi$CURRENT_SWITCHING_SEGMENT),
  ],
  CURRENT_SWITCHING_SEGMENT, wl_vel_3M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
wl_vel_3m_switchers[[col_3m]] <- paste0(
  round(wl_vel_3m_switchers$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_3m_switchers$ci_upper, 2),
  " - ", round(wl_vel_3m_switchers$ci_lower, 2),
  ", ", wl_vel_3m_switchers$n
)

wl_vel_6m_switchers <- sm(
  customers_w_6m_bmi[
    !is.na(customers_w_6m_bmi$CURRENT_SWITCHING_SEGMENT),
  ],
  CURRENT_SWITCHING_SEGMENT, wl_vel_6M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
wl_vel_6m_switchers[[col_6m]] <- paste0(
  round(wl_vel_6m_switchers$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_6m_switchers$ci_upper, 2),
  " - ", round(wl_vel_6m_switchers$ci_lower, 2),
  ", ", wl_vel_6m_switchers$n
)
wl_vel_9m_switchers <- sm(
  customers_w_9m_bmi[
    !is.na(customers_w_9m_bmi$CURRENT_SWITCHING_SEGMENT),
  ],
  CURRENT_SWITCHING_SEGMENT, wl_vel_9M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
wl_vel_9m_switchers[[col_9m]] <- paste0(
  round(wl_vel_9m_switchers$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_9m_switchers$ci_upper, 2),
  " - ", round(wl_vel_9m_switchers$ci_lower, 2),
  ", ", wl_vel_9m_switchers$n
)
wl_vel_12m_switchers <- sm(
  customers_w_12m_bmi[
    !is.na(customers_w_12m_bmi$CURRENT_SWITCHING_SEGMENT),
  ],
  CURRENT_SWITCHING_SEGMENT, wl_vel_12M,
  "CURRENT_SWITCHING_SEGMENT", "All Switchers"
)
wl_vel_12m_switchers[[col_12m]] <- paste0(
  round(wl_vel_12m_switchers$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_12m_switchers$ci_upper, 2),
  " - ", round(wl_vel_12m_switchers$ci_lower, 2),
  ", ", wl_vel_12m_switchers$n
)
tables <- list(
  wl_vel_12m_switchers, wl_vel_3m_switchers,
  wl_vel_6m_switchers, wl_vel_9m_switchers
)
wl_vel_all_switchers <- reduce(
  tables, full_join, by = "CURRENT_SWITCHING_SEGMENT"
)[, c(
  "CURRENT_SWITCHING_SEGMENT",
  "Percentage weight loss at 3 Months (mean, 95% CI, n)",
  "Percentage weight loss at 6 Months (mean, 95% CI, n)",
  "Percentage weight loss at 9 Months (mean, 95% CI, n)",
  "Percentage weight loss at 12 Months (mean, 95% CI, n)"
)]

wl_vel_all_switchers <- wl_vel_all_switchers %>%
  rename(Group = CURRENT_SWITCHING_SEGMENT)
wl_vel_all_switchers$Group <- ifelse(
  wl_vel_all_switchers$Group == "switched_wegovy_to_mounjaro",
  "Switched Wegovy to Mounjaro",
  ifelse(
    wl_vel_all_switchers$Group == "switched_mounjaro_to_wegovy",
    "Switched Mounjaro to Wegovy",
    wl_vel_all_switchers$Group
  )
)

#################################################################
###### Remainers ################################################
#################################################################
# SC Check - 29/12/2025: Same here - Why am I getting Inf and NaNs
wl_vel_3m_remainers <- sm(
  customers_w_3m_bmi[
    customers_w_3m_bmi$SWITCHED_BRANDS_FLAG == 0,
  ],
  PRODUCT_BRAND, wl_vel_3M,
  "PRODUCT_BRAND", "All Retainers"
)
wl_vel_3m_remainers[[col_3m]] <- paste0(
  round(wl_vel_3m_remainers$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_3m_remainers$ci_upper, 2),
  " - ", round(wl_vel_3m_remainers$ci_lower, 2),
  ", ", wl_vel_3m_remainers$n
)

wl_vel_6m_remainers <- sm(
  customers_w_6m_bmi[
    customers_w_6m_bmi$SWITCHED_BRANDS_FLAG == 0,
  ],
  PRODUCT_BRAND, wl_vel_6M,
  "PRODUCT_BRAND", "All Retainers"
)
wl_vel_6m_remainers[[col_6m]] <- paste0(
  round(wl_vel_6m_remainers$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_6m_remainers$ci_upper, 2),
  " - ", round(wl_vel_6m_remainers$ci_lower, 2),
  ", ", wl_vel_6m_remainers$n
)

wl_vel_9m_remainers <- sm(
  customers_w_9m_bmi[
    customers_w_9m_bmi$SWITCHED_BRANDS_FLAG == 0,
  ],
  PRODUCT_BRAND, wl_vel_9M,
  "PRODUCT_BRAND", "All Retainers"
)
wl_vel_9m_remainers[[col_9m]] <- paste0(
  round(wl_vel_9m_remainers$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_9m_remainers$ci_upper, 2),
  " - ", round(wl_vel_9m_remainers$ci_lower, 2),
  ", ", wl_vel_9m_remainers$n
)

wl_vel_12m_remainers <- sm(
  customers_w_12m_bmi[
    customers_w_12m_bmi$SWITCHED_BRANDS_FLAG == 0,
  ],
  PRODUCT_BRAND, wl_vel_12M,
  "PRODUCT_BRAND", "All Retainers"
)
wl_vel_12m_remainers[[col_12m]] <- paste0(
  round(wl_vel_12m_remainers$mean_pct_weight_loss, 2), ", ",
  round(wl_vel_12m_remainers$ci_upper, 2),
  " - ", round(wl_vel_12m_remainers$ci_lower, 2),
  ", ", wl_vel_12m_remainers$n
)
tables <- list(
  wl_vel_12m_remainers, wl_vel_3m_remainers,
  wl_vel_6m_remainers, wl_vel_9m_remainers
)
wl_vel_all_remainers <- reduce(
  tables, full_join, by = "PRODUCT_BRAND"
)[, c(
  "PRODUCT_BRAND",
  "Percentage weight loss at 3 Months (mean, 95% CI, n)",
  "Percentage weight loss at 6 Months (mean, 95% CI, n)",
  "Percentage weight loss at 9 Months (mean, 95% CI, n)",
  "Percentage weight loss at 12 Months (mean, 95% CI, n)"
)]
wl_vel_all_remainers <- wl_vel_all_remainers %>%
  rename(Group = PRODUCT_BRAND)

wl_vel_all_remainers$Group <- ifelse(
  wl_vel_all_remainers$Group == "Wegovy",
  "Retained Wegovy",
  ifelse(
    wl_vel_all_remainers$Group == "Mounjaro",
    "Retained Mounjaro",
    wl_vel_all_remainers$Group
  )
)


####
## distributions
###


ggplot(customers_w_3m_bmi, aes(x = PCT_WGT_LOSS_3M, fill = PRODUCT_BRAND)) +
  geom_histogram(
    aes(y = after_stat(density)), position = "identity",
    binwidth = 2, alpha = 0.4, color = "white"
  ) +
  labs(
    title = "Distribution of % Weight Lost at 3 months",
    x = "% Weight Lost",
    y = "Density"
  ) +
  xlim(-20, 50) + theme_minimal()

ggplot(customers_w_6m_bmi, aes(x = PCT_WGT_LOSS_6M, fill = PRODUCT_BRAND)) +
  geom_histogram(
    aes(y = after_stat(density)), position = "identity",
    binwidth = 2, alpha = 0.4, color = "white"
  ) +
  labs(
    title = "Distribution of % Weight Lost at 6 months",
    x = "% Weight Lost",
    y = "Density"
  ) +
  xlim(-20, 50) + theme_minimal()

ggplot(customers_w_9m_bmi, aes(x = PCT_WGT_LOSS_9M, fill = PRODUCT_BRAND)) +
  geom_histogram(
    aes(y = after_stat(density)), position = "identity",
    binwidth = 2, alpha = 0.4, color = "white"
  ) +
  labs(
    title = "Distribution of % Weight Lost at 9 months",
    x = "% Weight Lost",
    y = "Density"
  ) +
  xlim(-20, 50) + theme_minimal()

ggplot(
  customers_w_12m_bmi, aes(x = PCT_WGT_LOSS_12M, fill = PRODUCT_BRAND)
) +
  geom_histogram(
    aes(y = after_stat(density)), position = "identity",
    binwidth = 2, alpha = 0.4, color = "white"
  ) +
  labs(
    title = "Distribution of % Weight Lost at 12 months",
    x = "% Weight Lost",
    y = "Density"
  ) +
  xlim(-20, 50) + theme_minimal()


customers_w_12m_bmi[customers_w_12m_bmi$PCT_WGT_LOSS_12M < -50, ]
