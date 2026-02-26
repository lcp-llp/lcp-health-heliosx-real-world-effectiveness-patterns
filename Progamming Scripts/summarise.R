library(dplyr)
library(tidyr)
library(rlang)

# Function to create counts broken down by some attribute
# and some grouping category
get_totals <- function(source, group_by, gbs, attribute, all_label) {
  # Perform the initial counts
  init <- source %>%
    count({{ attribute }}, {{ group_by }}) %>%
    pivot_wider(
      names_from = {{ attribute }},
      values_from = n,
      values_fill = 0
    ) %>%
    mutate(total = rowSums(across(-{{ group_by }}))) %>%
    arrange({{ group_by }})

  # Replicate the source table and assign a 'all groups' value to
  # all rows of the group by column
  s2 <- source
  s2[[gbs]] <- all_label

  # Perform the totals per attribute for all group by categories
  totals <- s2 %>%
    count({{ attribute }}, {{ group_by }}) %>%
    pivot_wider(
      names_from = {{ attribute }},
      values_from = n,
      values_fill = 0
    ) %>%
    mutate(total = rowSums(across(-{{ group_by }}))) %>%
    arrange({{ group_by }})

  # Combine the initial totals and the all categories totals
  bind_rows(init, totals) %>%
    rename(col1 = {{ group_by }})
}

# Summarising totals without including the 'all groups' row
get_totals2 <- function(source, group_by, gbs, attribute) {
  init <- source %>%
    count({{ attribute }}, {{ group_by }}) %>%
    pivot_wider(
      names_from = {{ attribute }},
      values_from = n,
      values_fill = 0
    ) %>%
    mutate(total = rowSums(across(-{{ group_by }}))) %>%
    arrange({{ group_by }})


  init
}

# Age breakdown by sex
mounjaro_cust <- customers[customers$PRODUCT_BRAND == "Mounjaro", ]
wegovy_cust <- customers[customers$PRODUCT_BRAND == "Wegovy", ]

mounjaro_age_sex <- get_totals(
  mounjaro_cust, AGE_BUCKET, "AGE_BUCKET", GENDER, "All Ages"
)
mounjaro_age_sex$total <- NULL
mounjaro_age_sex <- mounjaro_age_sex %>%
  rename(
    mounjaro_male = male,
    mounjaro_female = female
  )
wegovy_age_sex <- get_totals(
  wegovy_cust, AGE_BUCKET, "AGE_BUCKET", GENDER, "All Ages"
)
wegovy_age_sex <- wegovy_age_sex %>%
  rename(
    wegovy_male = male,
    wegovy_female = female
  )
wegovy_age_sex$total <- NULL

age_sex <- merge(mounjaro_age_sex, wegovy_age_sex, by = "col1")
age_sex <- age_sex[age_sex$col1 != "All Ages", ]

# Starting dosages
mounjaro_dosages <- get_totals2(
  mounjaro_cust, STRENGTH_AMOUNT, "STRENGTH_AMOUNT", GENDER
)
wegovy_dosages <- get_totals2(
  wegovy_cust, STRENGTH_AMOUNT, "STRENGTH_AMOUNT", GENDER
)


# Baseline characteristics for all customers by initial product
sex <- get_totals(
  customers, GENDER, "GENDER", PRODUCT_BRAND, "All Genders"
)
ethnicity <- get_totals(
  customers, ETHNICITY_GROUPED, "ETHNICITY_GROUPED",
  PRODUCT_BRAND, "All Ethnicities"
)
age_bucket <- get_totals(
  customers, AGE_BUCKET, "AGE_BUCKET", PRODUCT_BRAND, "All Ages"
)
income_bucket <- get_totals(
  customers, INCOME_BUCKET, "INCOME_BUCKET",
  PRODUCT_BRAND, "All Incomes"
)
bmi_class <- get_totals(
  customers, BMI_CAT_0M, "BMI_CAT_0M", PRODUCT_BRAND, "All Categories"
)

# Baseline characteristics for all customers by customer status
sex_lapsed <- get_totals(
  customers, GENDER, "GENDER", CUSTOMER_STATUS, "All Genders"
)
ethnicity_lapsed <- get_totals(
  customers, ETHNICITY_GROUPED, "ETHNICITY_GROUPED",
  CUSTOMER_STATUS, "All Ethnicities"
)
age_bucket_lapsed <- get_totals(
  customers, AGE_BUCKET, "AGE_BUCKET", CUSTOMER_STATUS, "All Ages"
)
income_bucket_lapsed <- get_totals(
  customers, INCOME_BUCKET, "INCOME_BUCKET",
  CUSTOMER_STATUS, "All Incomes"
)
bmi_class_lapsed <- get_totals(
  customers, BMI_CAT_0M, "BMI_CAT_0M",
  CUSTOMER_STATUS, "All Categories"
)

# Baseline characteristics for all early discontinuers by initial product
ed_cust <- customers[customers$early_disc == 1, ]
sex_ed <- get_totals(
  ed_cust, GENDER, "GENDER", PRODUCT_BRAND, "All Genders"
)
ethnicity_ed <- get_totals(
  ed_cust, ETHNICITY_GROUPED, "ETHNICITY_GROUPED",
  PRODUCT_BRAND, "All Ethnicities"
)
age_bucket_ed <- get_totals(
  ed_cust, AGE_BUCKET, "AGE_BUCKET", PRODUCT_BRAND, "All Ages"
)
income_bucket_ed <- get_totals(
  ed_cust, INCOME_BUCKET, "INCOME_BUCKET",
  PRODUCT_BRAND, "All Incomes"
)
bmi_class_ed <- get_totals(
  ed_cust, BMI_CAT_0M, "BMI_CAT_0M", PRODUCT_BRAND, "All Categories"
)

# Baseline characteristics for all non-early discontinuers by initial product
ned_cust <- customers[customers$early_disc != 1, ]
sex_ned <- get_totals(
  ned_cust, GENDER, "GENDER", PRODUCT_BRAND, "All Genders"
)
ethnicity_ned <- get_totals(
  ned_cust, ETHNICITY_GROUPED, "ETHNICITY_GROUPED",
  PRODUCT_BRAND, "All Ethnicities"
)
age_bucket_ned <- get_totals(
  ned_cust, AGE_BUCKET, "AGE_BUCKET", PRODUCT_BRAND, "All Ages"
)
income_bucket_ned <- get_totals(
  ned_cust, INCOME_BUCKET, "INCOME_BUCKET",
  PRODUCT_BRAND, "All Incomes"
)
bmi_class_ned <- get_totals(
  ned_cust, BMI_CAT_0M, "BMI_CAT_0M", PRODUCT_BRAND, "All Categories"
)

# Baseline characteristics for all customers who switched products
# before the price rise by switching direction
pre_ph_cust <- customers[
  customers$switch_state == "Before Price Rise" &
    !is.na(customers$switch_state),
]
sex_sw_pre_ph <- get_totals(
  pre_ph_cust, GENDER, "GENDER",
  CURRENT_SWITCHING_SEGMENT, "All Genders"
)
eth_sw_pre_ph <- get_totals(
  pre_ph_cust, ETHNICITY_GROUPED, "ETHNICITY_GROUPED",
  CURRENT_SWITCHING_SEGMENT, "All Ethnicities"
)
age_sw_pre_ph <- get_totals(
  pre_ph_cust, AGE_BUCKET, "AGE_BUCKET",
  CURRENT_SWITCHING_SEGMENT, "All Ages"
)
inc_sw_pre_ph <- get_totals(
  pre_ph_cust, INCOME_BUCKET, "INCOME_BUCKET",
  CURRENT_SWITCHING_SEGMENT, "All Incomes"
)
bmi_sw_pre_ph <- get_totals(
  pre_ph_cust, BMI_CAT_0M, "BMI_CAT_0M",
  CURRENT_SWITCHING_SEGMENT, "All Categories"
)

# Baseline characteristics for all customers who switched products
# after the price rise by switching direction
post_ph_cust <- customers[
  customers$switch_state == "After Price Rise" &
    !is.na(customers$switch_state),
]
sex_sw_post_ph <- get_totals(
  post_ph_cust, GENDER, "GENDER",
  CURRENT_SWITCHING_SEGMENT, "All Genders"
)
eth_sw_post_ph <- get_totals(
  post_ph_cust, ETHNICITY_GROUPED, "ETHNICITY_GROUPED",
  CURRENT_SWITCHING_SEGMENT, "All Ethnicities"
)
age_sw_post_ph <- get_totals(
  post_ph_cust, AGE_BUCKET, "AGE_BUCKET",
  CURRENT_SWITCHING_SEGMENT, "All Ages"
)
inc_sw_post_ph <- get_totals(
  post_ph_cust, INCOME_BUCKET, "INCOME_BUCKET",
  CURRENT_SWITCHING_SEGMENT, "All Incomes"
)
bmi_sw_post_ph <- get_totals(
  post_ph_cust, BMI_CAT_0M, "BMI_CAT_0M",
  CURRENT_SWITCHING_SEGMENT, "All Categories"
)

# BMI at switch date for customers who switched before the price rise
bmi_sw_pre_ph_at_sw <- get_totals(
  pre_ph_cust, switching_BMI_CAT, "switching_BMI_CAT",
  CURRENT_SWITCHING_SEGMENT, "All Categories"
)
# BMI at switch date for customers who switched after the price rise
bmi_sw_post_ph_at_sw <- get_totals(
  post_ph_cust, switching_BMI_CAT, "switching_BMI_CAT",
  CURRENT_SWITCHING_SEGMENT, "All Categories"
)




# Count of customers who started each product by whether they began
# before or after the price hike
start_states <- get_totals(
  customers, PRODUCT_BRAND, "PRODUCT_BRAND",
  started_state, "All PRODUCTS"
)
# Count of customers who switched products by whether they switched
# before or after the price hike
switch_states <- get_totals(
  customers, CURRENT_SWITCHING_SEGMENT, "CURRENT_SWITCHING_SEGMENT",
  switch_state, "All SWitches"
)
# Count of customers who discontinued each product by whether they
# discontinued before or after the price hike
lapsed_states <- get_totals(
  customers, PRODUCT_BRAND, "PRODUCT_BRAND",
  lapsed_state, "All PRODUCTS"
)
lapsed_by_switch <- get_totals(
  customers, CURRENT_SWITCHING_SEGMENT, "CURRENT_SWITCHING_SEGMENT",
  lapsed_state, "All SWitches"
)

# Number of starters, switchers and discontinuers by month
starters_by_month <- get_totals(
  customers, PRODUCT_BRAND, "PRODUCT_BRAND",
  START_MONTH, "All Starters"
)
switchers_by_month <- get_totals(
  customers, CURRENT_SWITCHING_SEGMENT, "CURRENT_SWITCHING_SEGMENT",
  SWITCH_MONTH, "All Switchers"
)
discontinuers_by_month <- get_totals(
  customers, PRODUCT_BRAND, "PRODUCT_BRAND",
  DISCONTINUATION_MONTH, "All Discontinuers"
)

# Adjusting fields for presentation
starters_by_month$col1 <- ifelse(
  starters_by_month$col1 == "Mounjaro", "Mounjaro Starter",
  ifelse(
    starters_by_month$col1 == "Wegovy", "Wegovy Starter",
    starters_by_month$col1
  )
)
discontinuers_by_month$col1 <- ifelse(
  discontinuers_by_month$col1 == "Mounjaro", "Mounjaro discontinuer",
  ifelse(
    discontinuers_by_month$col1 == "Wegovy", "Wegovy discontinuer",
    discontinuers_by_month$col1
  )
)


# Get total active customers per month

# Find all the unique months in the dataset to create
# a complete timeline for cumulative counts
all_dates <- unique(customers$START_MONTH)

# Create cumulative counts for starters, switchers,
# and discontinuers by month and product
cumulative <- data.frame(month = all_dates) %>%
  mutate(
    cumulative_starter = sapply(
      month, function(d) sum(customers$START_MONTH <= d)
    ),
    cumulative_wegovy_starter = sapply(
      month, function(d) {
        sum(
          customers$START_MONTH <= d &
            customers$PRODUCT_BRAND == "Wegovy"
        )
      }
    ),
    cumulative_mounjaro_starter = sapply(
      month, function(d) {
        sum(
          customers$START_MONTH <= d &
            customers$PRODUCT_BRAND == "Mounjaro"
        )
      }
    ),
    cumulative_wegovy_switcher = sapply(
      month, function(d) {
        sum(
          customers$SWITCH_MONTH <= d &
            !is.na(customers$SWITCH_MONTH) &
            customers$PRODUCT_BRAND == "Wegovy"
        )
      }
    ),
    cumulative_mounjaro_switcher = sapply(
      month, function(d) {
        sum(
          customers$SWITCH_MONTH <= d &
            !is.na(customers$SWITCH_MONTH) &
            customers$PRODUCT_BRAND == "Mounjaro"
        )
      }
    ),
    cumulative_wegovy_discontinuer = sapply(
      month,
      function(d) {
        sum(
          customers$DISCONTINUATION_MONTH <= d &
            (
              (
                customers$PRODUCT_BRAND == "Mounjaro" &
                  !is.na(customers$SWITCH_MONTH)
              ) |
                (
                  customers$PRODUCT_BRAND == "Wegovy" &
                    is.na(customers$SWITCH_MONTH)
                )
            )
        )
      }
    ),
    cumulative_mounjaro_discontinuer = sapply(
      month, function(d) {
        sum(
          customers$DISCONTINUATION_MONTH <= d &
            (
              (
                customers$PRODUCT_BRAND == "Mounjaro" &
                  is.na(customers$SWITCH_MONTH)
              ) |
                (
                  customers$PRODUCT_BRAND == "Wegovy" &
                    !is.na(customers$SWITCH_MONTH)
                )
            )
        )
      }
    ),
    cumulative_finishers = sapply(
      month, function(d) {
        sum(
          !is.na(customers$DISCONTINUATION_MONTH) &
            customers$DISCONTINUATION_MONTH <= d
        )
      }
    ),
    net_active = cumulative_starter - cumulative_finishers
  )

# In order to get net values for each month, we need to lag the
# cumulative counts of discontinuers and switchers
cumulative <- cumulative %>%
  arrange(month) %>%
  mutate(
    previous_discontinuers = lag(cumulative_finishers),
    previous_m_discontinuers = lag(cumulative_mounjaro_discontinuer),
    previous_w_discontinuers = lag(cumulative_wegovy_discontinuer),
    previous_m_switchers = lag(cumulative_mounjaro_switcher),
    previous_w_switchers = lag(cumulative_wegovy_switcher)
  )

# Replace NA values in the lagged columns with 0, as there are no discontinuers
# or switchers before the first month
cumulative$previous_discontinuers <- ifelse(
  is.na(cumulative$previous_discontinuers),
  0, cumulative$previous_discontinuers
)
cumulative$previous_m_discontinuers <- ifelse(
  is.na(cumulative$previous_m_discontinuers),
  0, cumulative$previous_m_discontinuers
)
cumulative$previous_w_discontinuers <- ifelse(
  is.na(cumulative$previous_w_discontinuers),
  0, cumulative$previous_w_discontinuers
)
cumulative$previous_m_switchers <- ifelse(
  is.na(cumulative$previous_m_switchers),
  0, cumulative$previous_m_switchers
)
cumulative$previous_w_switchers <- ifelse(
  is.na(cumulative$previous_w_switchers),
  0, cumulative$previous_w_switchers
)

# Calculate net active customers for each month by adjusting the cumulative
# starters with the lagged discontinuers and switchers
cumulative$net_active_2 <-
  cumulative$cumulative_starter -
  cumulative$previous_discontinuers
cumulative$net_active_mounjaro <-
  cumulative$cumulative_mounjaro_starter -
  cumulative$previous_m_discontinuers -
  cumulative$previous_m_switchers +
  cumulative$previous_w_switchers
cumulative$net_active_wegovy <-
  cumulative$cumulative_wegovy_starter -
  cumulative$previous_w_discontinuers -
  cumulative$previous_w_switchers +
  cumulative$previous_m_switchers

# Remove the intermediate lagged columns as
# they are no longer needed for the final presentation
cumulative <- cumulative %>%
  select(-c(
    previous_discontinuers, previous_m_discontinuers,
    previous_w_discontinuers, previous_m_switchers,
    previous_w_switchers, cumulative_wegovy_starter,
    cumulative_mounjaro_starter, cumulative_wegovy_switcher,
    cumulative_mounjaro_switcher,
    cumulative_wegovy_discontinuer, cumulative_mounjaro_discontinuer
  ))

# Pivot the cumulative data to have months as
# columns and the different cumulative and net active counts as rows
pivot_cumulative <- cumulative %>%
  arrange(month) %>%
  pivot_longer(
    cols = c(
      cumulative_starter, cumulative_finishers,
      net_active, net_active_2,
      net_active_mounjaro, net_active_wegovy
    ),
    names_to = "col1", values_to = "value"
  ) %>%
  pivot_wider(names_from = month, values_from = value)
