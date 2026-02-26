library(readr)
library(ggplot2)
library(dplyr)
set.seed(1)
# This function performs logistic regression analysis
# for a specified target product and switching state.
do_lr <- function(source, products, switch_state) {
  # Filter the dataset to include only relevant rows based on the specified
  # conditions. We exclude rows with missing values in ETHNICITY_GROUPED,
  # INCOME_BUCKET, and BMI_CAT_0M categories of Normal and Underweight
  model_df <- source[
    !(source$ETHNICITY_GROUPED == "MISSING") &
      !(source$INCOME_BUCKET == "(unknown)") &
      !(source$BMI_CAT_0M %in% c("Normal", "Underweight")),
  ]

  # Convert the relevant columns to factors and
  # set reference levels for the logistic regression model
  model_df$GENDER <- as.factor(model_df$GENDER)
  model_df$GENDER <- relevel(model_df$GENDER, ref = "male")

  model_df$AGE_BUCKET <- as.factor(model_df$AGE_BUCKET)
  model_df$AGE_BUCKET <- relevel(model_df$AGE_BUCKET, ref = "18-34")

  model_df$ETHNICITY_GROUPED <- as.factor(model_df$ETHNICITY_GROUPED)
  model_df$ETHNICITY_GROUPED <- relevel(
    model_df$ETHNICITY_GROUPED, ref = "WHITE"
  )

  model_df$INCOME_BUCKET <- as.factor(model_df$INCOME_BUCKET)
  model_df$INCOME_BUCKET <- relevel(model_df$INCOME_BUCKET, ref = "20k-35k")

  model_df$BMI_CAT_0M <- as.factor(model_df$BMI_CAT_0M)
  model_df$BMI_CAT_0M <- relevel(model_df$BMI_CAT_0M, ref = "Overweight")

  # Create the target variable for the logistic regression model based on
  # whether the product brand is specified as the target product or not.
  model_df$target <- ifelse(model_df$PRODUCT_BRAND %in% products, 1, 0)
  model_df$target <- as.factor(model_df$target)

  # Fit the logistic regression model using the glm function, with the target
  # variable as the response and the specified demographic and health-related
  # variables as predictors.
  # The family argument is set to binomial to indicate that
  # we are modeling a binary outcome.
  model <- glm(
    target ~ GENDER + AGE_BUCKET + ETHNICITY_GROUPED +
      INCOME_BUCKET + BMI_CAT_0M,
    data = model_df,
    family = binomial
  )

  summary(model)

  summary_table <- summary(model)$coefficients

  or <- exp(coef(model))
  ci <- exp(confint(model))

  result_table <- cbind(
    Estimate = summary_table[, "Estimate"],
    `Std Error` = summary_table[, "Std. Error"],
    `z value` = summary_table[, "z value"],
    `p` = round(summary_table[, "Pr(>|z|)"], 4),
    OR = round(or, 4),
    CI_lower = round(ci[, 1], 4),
    CI_Upper = round(ci[, 2], 4)
  )
  result_table <- as.data.frame(result_table)
  result_table$OR <- paste0(
    result_table$OR,
    " (",
    result_table$CI_lower,
    " - ",
    result_table$CI_Upper,
    ")"
  )
  result_table$factors <- rownames(result_table)
  rownames(result_table) <- NULL
  n_or <- paste0(switch_state, " OR")
  n_p <- paste0(switch_state, " p")
  result_table <- result_table %>%
    rename(
      !!n_or := OR,
      !!n_p := p
    )
  result_table[, c(8, 5, 4)]

}

# Perform logistic regression analysis for Mounjaro target product
# separately for customers who started before and after the price rise, and
# store the results in separate data frames.
mounjaro_target_pre <- do_lr(
  customers[customers$started_state == "Before Price Rise", ],
  c("Mounjaro"),
  "Before Price Rise"
)

mounjaro_target_post <- do_lr(
  customers[customers$started_state == "After Price Rise", ],
  c("Mounjaro"),
  "After Price Rise"
)

table_2_5_m <- merge(
  mounjaro_target_pre, mounjaro_target_post, by = "factors"
)
export_location <- "N:\\For extract\\"
write.csv(
  table_2_5_m,
  paste0(export_location, "table_2_5_starters_mounjaro_target.csv"),
  row.names = FALSE
)