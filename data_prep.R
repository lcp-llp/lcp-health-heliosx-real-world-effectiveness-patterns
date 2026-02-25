# Load libraries
.libPaths("N:/R Packages")

library(dplyr)
library(tidyr)
library(rlang)

# Set Study end date
extract_date <- ""
# Set Price Hike start dates
ph_start <- "2025-09-01"
# Read in Raw data
customers <- read.csv("")
orders <- read.csv("")

# Remove non-useable order data rows; any order data with no reference
# for the date at which the order was placed
orders <- orders[
                 !is.na(orders$ANON_CUSTOMER_ID) &
                   !is.na(orders$DAYS_SINCE_ACQUISITION), ]

# Percentage weight lost recorded as negative in original data;
# converting to positive
customers$PCT_WGT_LOSS_12M <- -customers$PCT_WGT_LOSS_12M
customers$PCT_WGT_LOSS_3M <- -customers$PCT_WGT_LOSS_3M
customers$PCT_WGT_LOSS_6M <- -customers$PCT_WGT_LOSS_6M
customers$PCT_WGT_LOSS_9M <- -customers$PCT_WGT_LOSS_9M

# Convert START_MONTH to date -  middle of the month to reduce error
customers$START_DATE <- as.Date(paste0(customers$START_MONTH, "-15"))

# Join customer data to orders to derive approximate order date
orders_w_dates <- merge(
  orders,
  customers[, c("ANON_CUSTOMER_ID", "START_DATE")],
  by = "ANON_CUSTOMER_ID",
  all.x = TRUE
)

# Create approximate order date
orders_w_dates$APPROX_ORDER_DATE <-
  orders_w_dates$START_DATE + orders_w_dates$DAYS_SINCE_ACQUISITION

# Determine the date at which a customer is first considered to have
# discontinued. All records after this date to be disincluded
# removes orders which occur after a period of discontinuation of
# treatment exceeding 84 days

orders_w_dates <- orders_w_dates %>%
  arrange(ANON_CUSTOMER_ID, APPROX_ORDER_DATE) %>%
  group_by(ANON_CUSTOMER_ID) %>%
  mutate(previous_days = lag(DAYS_SINCE_ACQUISITION))

days_diff <- orders_w_dates$DAYS_SINCE_ACQUISITION -
  orders_w_dates$previous_days
orders_w_dates <- orders_w_dates[
                                 !(days_diff > 84 & !is.na(days_diff)), ]



# calculate percentage weight change between orders
orders_w_dates <- orders_w_dates %>%
  arrange(ANON_CUSTOMER_ID, APPROX_ORDER_DATE) %>%
  group_by(ANON_CUSTOMER_ID) %>%
  mutate(
    previous_weight = lag(WEIGHT),
    next_weight = lead(WEIGHT),
    previous_height = lag(HEIGHT),
    next_height = lead(HEIGHT)
  )

orders_w_dates$pct_weight_change <- abs(
  (orders_w_dates$previous_weight - orders_w_dates$WEIGHT) /
    orders_w_dates$previous_weight
)
orders_w_dates$pct_height_change <- abs(
  (orders_w_dates$previous_height - orders_w_dates$HEIGHT) /
    orders_w_dates$previous_weight
)

# Remove Implausible Customers
implausible_weight_differences <- orders_w_dates[
  orders_w_dates$pct_weight_change > 0.25 &
    !is.na(orders_w_dates$pct_weight_change), ]
implausible_height_differences <- orders_w_dates[
  orders_w_dates$pct_height_change > 0.012 &
    !is.na(orders_w_dates$pct_height_change), ]
implausible_weights <- orders_w_dates[
  (orders_w_dates$WEIGHT > 350 | orders_w_dates$WEIGHT < 25) &
    !is.na(orders_w_dates$WEIGHT), ]
implausible_heights <- orders_w_dates[
  (orders_w_dates$HEIGHT > 3 | orders_w_dates$HEIGHT < 1.25) &
    !is.na(orders_w_dates$HEIGHT), ]
implausible_bmi <- orders_w_dates[
  (orders_w_dates$BMI > 120 | orders_w_dates$BMI < 10) &
    !is.na(orders_w_dates$BMI), ]
missing_data <- orders_w_dates[
                               is.na(orders_w_dates$BMI) |
                                 is.na(orders_w_dates$HEIGHT) |
                                 is.na(orders_w_dates$WEIGHT), ]
excluded_ids <- unique(bind_rows(
  implausible_height_differences,
  implausible_weight_differences,
  implausible_heights,
  implausible_weights,
  implausible_bmi,
  missing_data
)[, c("ANON_CUSTOMER_ID")])

# clean up customers
customers <- anti_join(customers, excluded_ids, by = "ANON_CUSTOMER_ID")



# Determine the first product ordered by a customer with HeliosX
customers <- merge(
  customers,
  orders_w_dates[
    orders_w_dates$TREATMENT_ORDER_NUMBER == 1,
    c("ANON_CUSTOMER_ID", "PRODUCT_BRAND",
      "DAYS_SINCE_ACQUISITION", "STRENGTH_AMOUNT")
  ],
  by = "ANON_CUSTOMER_ID",
  all.x = TRUE
)
# remove customers whose first order date was before their start date
customers <- customers[!customers$DAYS_SINCE_ACQUISITION < 0, ]
customers$DAYS_SINCE_ACQUISITION <- NULL
# remove saxenda/other customers
customers <- customers[
                       !(customers$PRODUCT_BRAND %in% c("(none)", "Saxenda")), ]
# Condense income buckets
customers$INCOME_BUCKET <- ifelse(
  customers$INCOME_BUCKET %in% c("20k-30k", "30k-35k"), "20k-35k",
  ifelse(
    customers$INCOME_BUCKET %in% c("35k-40k", "40k-45k"), "35k-45k",
    ifelse(
      customers$INCOME_BUCKET %in% c("45k-50k", "50k-55k"), "45k-55k",
      ifelse(
        customers$INCOME_BUCKET %in% c("55k-60k", "60k-150k"),
        "55k-150k",
        customers$INCOME_BUCKET
      )
    )
  )
)
# Calculate BMI category 0m
customers$BMI_CAT_0M <- case_when(
  customers$BMI_0M >= 10 & customers$BMI_0M < 18.5 ~ "Underweight",
  customers$BMI_0M >= 18.5 & customers$BMI_0M < 25 ~ "Normal",
  customers$BMI_0M >= 25 & customers$BMI_0M < 30 ~ "Overweight",
  customers$BMI_0M >= 30 & customers$BMI_0M < 35 ~ "Obese -  Class I",
  customers$BMI_0M >= 35 & customers$BMI_0M < 40 ~ "Obese - Class II",
  customers$BMI_0M >= 40 & customers$BMI_0M <= 120 ~
    "Very Severely Obese -  Class III",
  TRUE ~ "Invalid BMI"
)

# get latest order date, Weight, height and BMI
orders_w_dates <- orders_w_dates %>%
  group_by(ANON_CUSTOMER_ID) %>%
  mutate(latest_order = max(TREATMENT_ORDER_NUMBER))

customers <- merge(
  customers,
  orders_w_dates[
    orders_w_dates$latest_order == orders_w_dates$TREATMENT_ORDER_NUMBER,
    c("ANON_CUSTOMER_ID", "APPROX_ORDER_DATE",
      "WEIGHT", "HEIGHT", "BMI")
  ],
  by = "ANON_CUSTOMER_ID",
  all.x = TRUE
)
customers$latest_order_date <- customers$APPROX_ORDER_DATE
customers$APPROX_ORDER_DATE <- NULL
customers <- customers %>%
  rename(
    latest_weight = WEIGHT,
    latest_height = HEIGHT,
    latest_BMI = BMI
  )
# Create a date 30 days after last order date. This is an estimate of
# the customer's last date actively on treatment
customers$last_effective_date <- customers$latest_order_date + 30

# Create a lapsed flag for customers whose latest order was more than
# 84 days before the extract date
customers$is_lapsed <- ifelse(
  as.Date(extract_date) - customers$latest_order_date > 84,
  "LAPSED",
  "CURRENT"
)
customers <- customers %>%
  rename(
    CUSTOMER_STATUS = is_lapsed,
    CUSTOMER_STATUS_OLD = CUSTOMER_STATUS
  )

# early discontinuer flag. Early discontinuer is defined as a customer
# who has lapsed within 90 days of beginning treatment
customers$early_disc <- ifelse(
  customers$last_effective_date - customers$START_DATE < 90 &
    customers$CUSTOMER_STATUS == "LAPSED",
  1, 0
)

# get switching date, Weight, height and BMI
seg_col <- "CURRENT_SWITCHING_SEGMENT"
switching_filter <-
  orders_w_dates$MEDICATION_ORDER_NUMBER == 1 &
  (orders_w_dates[[seg_col]] == "switched_mounjaro_to_wegovy" |
   orders_w_dates[[seg_col]] == "switched_wegovy_to_mounjaro")
customers <- merge(
  customers,
  orders_w_dates[
    switching_filter,
    c("ANON_CUSTOMER_ID", "APPROX_ORDER_DATE",
      "CURRENT_SWITCHING_SEGMENT", "WEIGHT", "HEIGHT",
      "BMI", "DAYS_SINCE_ACQUISITION")
  ],
  by = "ANON_CUSTOMER_ID",
  all.x = TRUE
)
customers$switching_date <- customers$APPROX_ORDER_DATE
customers$APPROX_ORDER_DATE <- NULL
customers <- customers %>%
  rename(
    switching_weight = WEIGHT,
    switching_height = HEIGHT,
    switching_BMI = BMI,
    switching_days = DAYS_SINCE_ACQUISITION
  )
customers$days <- as.numeric(
  customers$last_effective_date - customers$START_DATE
)
customers$lapsed <- ifelse(
  customers$CUSTOMER_STATUS == "LAPSED", 1, 0
)
# Determine BMI category at switch
customers$switching_BMI_CAT <- case_when(
  customers$switching_BMI >= 10 &
    customers$switching_BMI < 18.5 ~ "Underweight",
  customers$switching_BMI >= 18.5 &
    customers$switching_BMI < 25 ~ "Normal",
  customers$switching_BMI >= 25 &
    customers$switching_BMI < 30 ~ "Overweight",
  customers$switching_BMI >= 30 &
    customers$switching_BMI < 35 ~ "Obese -  Class I",
  customers$switching_BMI >= 35 &
    customers$switching_BMI < 40 ~ "Obese - Class II",
  customers$switching_BMI >= 40 &
    customers$switching_BMI <= 120 ~ "Very Severely Obese -  Class III",
  TRUE ~ "Invalid BMI"
)
customers$switching_pct_weight_loss <-
  (customers$WEIGHT_0M - customers$switching_weight) /
  customers$WEIGHT_0M


# Flag whether customer joined before or after price hike
customers$started_state <- ifelse(
  customers$START_DATE > ph_start,
  "After Price Rise",
  "Before Price Rise"
)
# Flag whether customer switched products before or after price hike
customers$switch_state <- ifelse(
  customers$switching_date > ph_start,
  "After Price Rise",
  "Before Price Rise"
)
# Flag whether customer discontinued before or after price hike
customers$lapsed_state <- ifelse(
  customers$CUSTOMER_STATUS == "LAPSED",
  ifelse(
    customers$last_effective_date > ph_start,
    "After Price Rise",
    "Before Price Rise"
  ),
  "Current"
)

# Derive the month the customer switched products
customers$SWITCH_MONTH <- format(customers$switching_date, "%Y-%m")
# Derive the month the customer discontinued
customers$DISCONTINUATION_MONTH <- ifelse(
  customers$CUSTOMER_STATUS == "LAPSED",
  format(customers$last_effective_date, "%Y-%m"),
  "CURRENT"
)
customers$DISCONTINUATION_DATE <- ifelse(
  customers$CUSTOMER_STATUS == "LAPSED",
  customers$last_effective_date,
  NaN
)
# Flag whether the customer was active before the price hike
customers$active_before_ph <- ifelse(
  customers$START_DATE <= ph_start, 1, 0
)
# Flag whether the customer was active after the price hike
customers$active_after_ph <- ifelse(
  customers$last_effective_date >= ph_start, 1, 0
)


# Get customer weights at 3rd, 6th, 9th and 12th orders
customers <- customers %>%
  left_join(
    orders_w_dates[
      orders_w_dates$TREATMENT_ORDER_NUMBER == 3,
      c("ANON_CUSTOMER_ID", "WEIGHT", "DAYS_SINCE_ACQUISITION")
    ],
    by = "ANON_CUSTOMER_ID"
  ) %>%
  rename(
    weight_at_3_orders = WEIGHT,
    days_at_3_orders = DAYS_SINCE_ACQUISITION
  )

customers <- customers %>%
  left_join(
    orders_w_dates[
      orders_w_dates$TREATMENT_ORDER_NUMBER == 6,
      c("ANON_CUSTOMER_ID", "WEIGHT", "DAYS_SINCE_ACQUISITION")
    ],
    by = "ANON_CUSTOMER_ID"
  ) %>%
  rename(
    weight_at_6_orders = WEIGHT,
    days_at_6_orders = DAYS_SINCE_ACQUISITION
  )

customers <- customers %>%
  left_join(
    orders_w_dates[
      orders_w_dates$TREATMENT_ORDER_NUMBER == 9,
      c("ANON_CUSTOMER_ID", "WEIGHT", "DAYS_SINCE_ACQUISITION")
    ],
    by = "ANON_CUSTOMER_ID"
  ) %>%
  rename(
    weight_at_9_orders = WEIGHT,
    days_at_9_orders = DAYS_SINCE_ACQUISITION
  )

customers <- customers %>%
  left_join(
    orders_w_dates[
      orders_w_dates$TREATMENT_ORDER_NUMBER == 12,
      c("ANON_CUSTOMER_ID", "WEIGHT", "DAYS_SINCE_ACQUISITION")
    ],
    by = "ANON_CUSTOMER_ID"
  ) %>%
  rename(
    weight_at_12_orders = WEIGHT,
    days_at_12_orders = DAYS_SINCE_ACQUISITION
  )

# Determine customers' weight loss velocity at 3rd, 6th, 9th and 12th
# orders (that is %weight loss per month over the quarter)
customers$wl_vel_3M <- round(
  ((customers$WEIGHT_0M - customers$weight_at_3_orders) /
     customers$WEIGHT_0M) / customers$days_at_3_orders * 30.4 * 100,
  2
)
customers$wl_vel_6M <- round(
  ((customers$WEIGHT_0M - customers$weight_at_6_orders) /
     customers$WEIGHT_0M) / customers$days_at_6_orders * 30.4 * 100,
  2
)
customers$wl_vel_9M <- round(
  ((customers$WEIGHT_0M - customers$weight_at_9_orders) /
     customers$WEIGHT_0M) / customers$days_at_9_orders * 30.4 * 100,
  2
)
customers$wl_vel_12M <- round(
  ((customers$WEIGHT_0M - customers$weight_at_12_orders) /
     customers$WEIGHT_0M) / customers$days_at_12_orders * 30.4 * 100,
  2
)

# Set flag to 1 if customer ever switched products from Wegovy to
# Mounjaro or vice versa in their order history
customers$SWITCHED_BRANDS_FLAG <- ifelse(
  !is.na(customers$CURRENT_SWITCHING_SEGMENT), 1, 0
)


# Determine the number of days a customer was active before lapsing
# (or the end of the study was reached)
customers$days_to_lapse <- ifelse(
  customers$CUSTOMER_STATUS == "LAPSED",
  customers$days,
  as.Date(extract_date) - customers$START_DATE
)
# Determine the number of days a customer was active before switching
# (or the end of the study was reached)
customers$days_to_switch <- ifelse(
  !is.na(customers$CURRENT_SWITCHING_SEGMENT),
  customers$switching_days,
  customers$days_to_lapse
)
# Determine the number of days a customer was active before switching
# if switching occured before September 2025, else number of days a
# customer was active before Sep 2025
customers$days_to_switch_pre_price_rise <- ifelse(
  customers$switch_state == "Before Price Rise" &
    !is.na(customers$switch_state),
  customers$switching_days,
  pmin(
    as.integer(as.Date(ph_start) - customers$START_DATE),
    as.integer(customers$days_to_lapse)
  )
)

customers$days_to_switch_post_price_rise <- ifelse(
  customers$switch_state == "After Price Rise" &
    !is.na(customers$switch_state),
  customers$switching_date - as.Date(ph_start),
  pmin(
    as.Date(extract_date) - as.Date(ph_start),
    as.Date(extract_date) - customers$START_DATE,
    customers$last_effective_date - as.Date(ph_start)
  )
)

# Flag if a customer switched before or after the price rise
customers$switched_pre <- ifelse(
  customers$switch_state == "Before Price Rise" &
    !is.na(customers$switch_state),
  1, 0
)
customers$switched_post <- ifelse(
  customers$switch_state == "After Price Rise" &
    !is.na(customers$switch_state),
  1, 0
)

# Remove customers with no start month or no first product
customers <- customers[
  !is.na(customers$START_MONTH) &
    !is.na(customers$PRODUCT_BRAND), ]