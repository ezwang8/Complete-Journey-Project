library(completejourney)
library(dplyr)
library(purrr)
library(tibble)

# ----------------------------------------
# 1. DATA INVENTORY
# ----------------------------------------

dataset_names <- c(
  "campaign_descriptions",
  "campaigns",
  "coupon_redemptions",
  "coupons",
  "demographics",
  "products",
  "promotions_sample",
  "transactions_sample"
)

inventory <- map_dfr(dataset_names, function(x) {
  
  df <- get(x)
  
  tibble(
    dataset = x,
    rows = nrow(df),
    columns = ncol(df)
  )
})

inventory

# ----------------------------------------
# 2. COLUMN INVENTORY
# ----------------------------------------

for (x in dataset_names) {
  
  cat("\n\n============================\n")
  cat(x)
  cat("\n============================\n")
  
  df <- get(x)
  
  print(names(df))
  print(head(df, 3))
}

# ----------------------------------------
# 3. STRUCTURE
# ----------------------------------------

glimpse(campaign_descriptions)
glimpse(campaigns)
glimpse(coupon_redemptions)
glimpse(coupons)
glimpse(demographics)
glimpse(products)
glimpse(promotions_sample)
glimpse(transactions_sample)

# ----------------------------------------
# 4. DATA QUALITY / KEY CHECKS
# ----------------------------------------

# How many unique entities?
campaigns %>% summarise(
  households = n_distinct(household_id),
  campaigns = n_distinct(campaign_id)
)

transactions_sample %>% summarise(
  households = n_distinct(household_id),
  stores = n_distinct(store_id),
  baskets = n_distinct(basket_id),
  products = n_distinct(product_id),
  weeks = n_distinct(week)
)

# Transaction date range
transactions_sample %>% summarise(
  first_transaction = min(transaction_timestamp),
  last_transaction = max(transaction_timestamp)
)

# Strange quantities
transactions_sample %>%
  count(quantity, sort = TRUE)

# Any negative / zero sales?
transactions_sample %>% summarise(
  zero_quantity = sum(quantity == 0, na.rm = TRUE),
  negative_quantity = sum(quantity < 0, na.rm = TRUE),
  zero_sales = sum(sales_value == 0, na.rm = TRUE),
  negative_sales = sum(sales_value < 0, na.rm = TRUE)
)

# Coupon usage frequency
transactions_sample %>% summarise(
  coupon_rows = sum(coupon_disc != 0, na.rm = TRUE),
  coupon_match_rows = sum(coupon_match_disc != 0, na.rm = TRUE),
  retail_discount_rows = sum(retail_disc != 0, na.rm = TRUE)
)

# Campaign household coverage
campaigns %>%
  count(household_id, name = "campaigns_received") %>%
  count(campaigns_received)

# Redemption frequency
coupon_redemptions %>%
  count(household_id, name = "redemptions") %>%
  arrange(desc(redemptions))

# Store ID sanity check
transactions_sample %>%
  distinct(store_id) %>%
  arrange(store_id)