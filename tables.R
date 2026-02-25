library(dplyr)
library(tidyr)
library(rlang)

# This file contains functions to create the tables that are included in
# the manuscript and supplementary materials.


table_1_create <- function(tables) {
  table_1 <- bind_rows(tables)
  all_mounjaro <- table_1[table_1$col1 == "All Genders", ]$Mounjaro
  all_wegovy <- table_1[table_1$col1 == "All Genders", ]$Wegovy
  all_prods <- table_1[table_1$col1 == "All Genders", ]$total
  table_1$Mounjaro_pct <- table_1$Mounjaro / all_mounjaro
  table_1$Wegovy_pct <- table_1$Wegovy / all_wegovy
  table_1$Total_pct <- table_1$total / all_prods
  table_1$col1 <- ifelse(
    table_1$col1 == "ASIAN_OR_ASIAN_BRITISH", "Asian",
    ifelse(
      table_1$col1 == "BLACK_AFRICAN_CARIBBEAN_OR_BLACK_BRITISH", "Black",
      ifelse(table_1$col1 == "WHITE", "White", table_1$col1)
    )
  )
  table_1$Mounjaro <- paste0(
    table_1$Mounjaro, " (",
    round(table_1$Mounjaro_pct * 100, 2), "%)"
  )
  table_1$Wegovy <- paste0(
    table_1$Wegovy, " (",
    round(table_1$Wegovy_pct * 100, 2), "%)"
  )
  table_1$total <- paste0(
    table_1$total, " (",
    round(table_1$Total_pct * 100, 2), "%)"
  )
  table_1$Mounjaro_pct <- NULL
  table_1$Wegovy_pct <- NULL
  table_1$Total_pct <- NULL
  table_1 <- table_1 %>%
    filter(!grepl("All", col1))
  table_1
}
create_bmi_stats <- function(bmis) {
  all_mounjaro <- bmis[bmis$col1 == "All Categories", ]$Mounjaro
  all_wegovy <- bmis[bmis$col1 == "All Categories", ]$Wegovy
  all_prods <- bmis[bmis$col1 == "All Categories", ]$total
  table_2 <- bmis
  table_2$Mounjaro_pct <- table_2$Mounjaro / all_mounjaro
  table_2$Wegovy_pct <- table_2$Wegovy / all_wegovy
  table_2$Total_pct <- table_2$total / all_prods
  table_2$Mounjaro <- paste0(
    table_2$Mounjaro, " (",
    round(table_2$Mounjaro_pct * 100, 2), "%)"
  )
  table_2$Wegovy <- paste0(
    table_2$Wegovy, " (",
    round(table_2$Wegovy_pct * 100, 2), "%)"
  )
  table_2$total <- paste0(
    table_2$total, " (",
    round(table_2$Total_pct * 100, 2), "%)"
  )
  table_2$Mounjaro_pct <- NULL
  table_2$Wegovy_pct <- NULL
  table_2$Total_pct <- NULL
  table_2 <- table_2 %>% rename("All Products" = total)
  table_2
}

# Creating exports that inform Table 2 in the manuscript
table_1_1 <- table_1_create(list(sex, age_bucket, ethnicity, income_bucket))
table_1_2 <- create_bmi_stats(bmi_class)
table_1_2 <- bind_rows(av_weight, av_bmi, table_1_2)

# Exports for Table 1 in the manuscript 
write.csv(
  age_sex,
  paste0(export_location, "table_age_sex.csv"),
  row.names = FALSE
)

# Export for table 3 in the manuscript and supplmentary figure 2
table_2_5_m <- merge(
  mounjaro_target_pre, mounjaro_target_post, by = "factors"
)

# Export for Table 4 in the manuscript

table_3_1 <- bind_rows(
  pct_w_lost_all, pct_w_lost_all_switchers, pct_w_lost_all_remainers
)
table_3_1 <- table_3_1[!is.na(table_3_1$Group), ]

# Export informing Figure 1 in the manuscript
# and Supplementary tables 1.1, 1.2, and 1.3

table_2_2 <- bind_rows(
  starters_by_month, switchers_by_month,
  discontinuers_by_month, pivot_cumulative
)
table_2_2$total <- NULL
table_2_2$`2024-05` <- NULL

table_2_2$`2026-01` <- NULL
table_2_2$CURRENT <- NULL
table_2_2$`NA` <- NULL
table_2_2 <- table_2_2[!is.na(table_2_2$col1), ]
all_dates_small <- all_dates[
  sapply(all_dates, function(x) {
    !(x %in% c("2024-05", "2026-01"))
  })
]
table_2_2_pivot <- table_2_2 %>%
  pivot_longer(cols = all_dates_small,
               names_to = "Date", values_to = "value") %>%
  pivot_wider(names_from = col1, values_from = value)


table_2_2_pivot$switched_mounjaro_to_wegovy_pct <-
  table_2_2_pivot$switched_mounjaro_to_wegovy /
  table_2_2_pivot$net_active_mounjaro
table_2_2_pivot$switched_wegovy_to_mounjaro_pct <-
  table_2_2_pivot$switched_wegovy_to_mounjaro /
  table_2_2_pivot$net_active_wegovy
table_2_2_pivot$switched_mounjaro_to_wegovy <- paste0(
  table_2_2_pivot$switched_mounjaro_to_wegovy, " (",
  round(table_2_2_pivot$switched_mounjaro_to_wegovy_pct * 100, 2), "%)"
)
table_2_2_pivot$switched_wegovy_to_mounjaro <- paste0(
  table_2_2_pivot$switched_wegovy_to_mounjaro, " (",
  round(table_2_2_pivot$switched_wegovy_to_mounjaro_pct * 100, 2), "%)"
)
table_2_2_pivot$`Product Switching Rate (%)` <- round(
  table_2_2_pivot$`All Switchers` /
    table_2_2_pivot$`net_active_2` * 100, 2
)

table_2_2_pivot$started_mounjaro_pct <-
  table_2_2_pivot$`Mounjaro Starter` / table_2_2_pivot$`All Starters`
table_2_2_pivot$started_wegovy_pct <-
  table_2_2_pivot$`Wegovy Starter` / table_2_2_pivot$`All Starters`
table_2_2_pivot$`Mounjaro Starter` <- paste0(
  table_2_2_pivot$`Mounjaro Starter`, " (",
  round(table_2_2_pivot$started_mounjaro_pct * 100, 2), "%)"
)
table_2_2_pivot$`Wegovy Starter` <- paste0(
  table_2_2_pivot$`Wegovy Starter`, " (",
  round(table_2_2_pivot$started_wegovy_pct * 100, 2), "%)"
)

table_2_2_pivot$discontinued_mounjaro_pct <-
  table_2_2_pivot$`Mounjaro discontinuer` /
  table_2_2_pivot$net_active_mounjaro
table_2_2_pivot$discontinued_wegovy_pct <-
  table_2_2_pivot$`Wegovy discontinuer` /
  table_2_2_pivot$net_active_wegovy
table_2_2_pivot$`Mounjaro discontinuer` <- paste0(
  table_2_2_pivot$`Mounjaro discontinuer`, " (",
  round(table_2_2_pivot$discontinued_mounjaro_pct * 100, 2), "%)"
)
table_2_2_pivot$`Wegovy discontinuer` <- paste0(
  table_2_2_pivot$`Wegovy discontinuer`, " (",
  round(table_2_2_pivot$discontinued_wegovy_pct * 100, 2), "%)"
)

table_2_2_pivot$`Discontinuation Rate (%)` <- round(
  table_2_2_pivot$`All Discontinuers` /
    table_2_2_pivot$`net_active_2` * 100, 2
)

table_2_2_pivot <- table_2_2_pivot %>%
  rename(
    "Mounjaro to Wegovy n (%)" = switched_mounjaro_to_wegovy,
    "Wegovy to Mounjaro n (%)" = switched_wegovy_to_mounjaro,
    "All Active (n)" = net_active_2,
    "All Active Mounjaro (n)" = net_active_mounjaro,
    "All Active Wegovy (n)" = net_active_wegovy,
    "Total Discontinuations (n)" = `All Discontinuers`,
    "Total Product Switches (n)" = `All Switchers`,
    "Starting on Mounjaro n (%)" = `Mounjaro Starter`,
    "Starting on Wegovy n (%)" = `Wegovy Starter`,
    "Total New GLP-1 RA Starters (n)" = `All Starters`,
    "Mounjaro Discontinuations n (%)" = `Mounjaro discontinuer`,
    "Wegovy Discontinuations n (%)" = `Wegovy discontinuer`
  )
table_2_2_pivot <- table_2_2_pivot[, c(
  "Date", "All Active (n)", "All Active Mounjaro (n)",
  "All Active Wegovy (n)", "Total New GLP-1 RA Starters (n)",
  "Starting on Mounjaro n (%)", "Starting on Wegovy n (%)",
  "Total Product Switches (n)", "Product Switching Rate (%)",
  "Mounjaro to Wegovy n (%)", "Wegovy to Mounjaro n (%)",
  "Total Discontinuations (n)", "Discontinuation Rate (%)",
  "Mounjaro Discontinuations n (%)", "Wegovy Discontinuations n (%)"
)]

table_2_2_pivot <- table_2_2_pivot %>%
  arrange(Date)
table_2_2_pivot <- table_2_2_pivot[
  !(table_2_2_pivot$Date %in% c("2024-05", "2024-06")), ]