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

# Selecting target household
target_household <- "256"

# Campaigns received
campaigns %>%
  filter(household_id == target_household) %>%
  left_join(campaign_descriptions, by = "campaign_id") %>%
  arrange(start_date)

# Coupons redeemed
coupon_redemptions %>%
  filter(household_id == target_household) %>%
  arrange(redemption_date)

# Transactions in sample
transactions_sample %>%
  filter(household_id == target_household) %>%
  arrange(transaction_timestamp)

# Demographics, if available
demographics %>%
  filter(household_id == target_household)

coupon_redemptions %>%
  filter(household_id == target_household) %>%
  left_join(
    coupons,
    by = c("coupon_upc", "campaign_id")
  ) %>%
  left_join(
    products,
    by = "product_id"
  )


# ----------------------------------------
# 5. COUPON ELIGIBILITY
# ----------------------------------------

coupon_eligibility <- coupons %>%
  count(
    campaign_id,
    coupon_upc,
    name = "eligible_products"
  )

# Overall coupon eligibility distribution
coupon_eligibility %>%
  summarise(
    coupons = n(),
    min_products = min(eligible_products),
    median_products = median(eligible_products),
    mean_products = mean(eligible_products),
    max_products = max(eligible_products)
  )

# Largest product sets
coupon_eligibility %>%
  arrange(desc(eligible_products)) %>%
  head(20)

# Household 256's redeemed coupons
coupon_redemptions %>%
  filter(household_id == target_household) %>%
  distinct(campaign_id, coupon_upc) %>%
  left_join(
    coupon_eligibility,
    by = c("campaign_id", "coupon_upc")
  ) %>%
  arrange(desc(eligible_products))


# ----------------------------------------
# 6. COUPON BREADTH VS REDEMPTION
# ----------------------------------------

coupon_redemption_counts <- coupon_redemptions %>%
  count(
    campaign_id,
    coupon_upc,
    name = "redemptions"
  )

coupon_performance <- coupon_eligibility %>%
  left_join(
    coupon_redemption_counts,
    by = c("campaign_id", "coupon_upc")
  ) %>%
  mutate(
    redemptions = coalesce(redemptions, 0)
  )

# Narrowest coupons
coupon_performance %>%
  arrange(eligible_products, desc(redemptions)) %>%
  head(20)

# Most redeemed coupons
coupon_performance %>%
  arrange(desc(redemptions)) %>%
  head(20)

# Broadest coupons
coupon_performance %>%
  arrange(desc(eligible_products)) %>%
  head(20)


coupon_performance <- coupon_performance %>%
  mutate(
    breadth_group = case_when(
      eligible_products == 1 ~ "1 product",
      eligible_products <= 5 ~ "2-5 products",
      eligible_products <= 20 ~ "6-20 products",
      eligible_products <= 100 ~ "21-100 products",
      eligible_products <= 1000 ~ "101-1000 products",
      TRUE ~ "1000+ products"
    )
  )

coupon_performance %>%
  group_by(breadth_group) %>%
  summarise(
    coupons = n(),
    total_redemptions = sum(redemptions),
    avg_redemptions = mean(redemptions),
    median_redemptions = median(redemptions)
  )