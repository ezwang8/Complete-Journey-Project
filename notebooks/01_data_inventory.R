library(completejourney)
library(dplyr)
library(purrr)
library(tibble)

# ----------------------------------------
# 1. DATA INVENTORY
# ----------------------------------------

# 1.1 Define available datasets
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

# 1.2 Count rows and columns in each dataset
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

# 2.1 Inspect column names and first rows
for (x in dataset_names) {
  
  cat("\n\n============================\n")
  cat(x)
  cat("\n============================\n")
  
  df <- get(x)
  
  print(names(df))
  print(head(df, 3))
}


# ----------------------------------------
# 3. DATA STRUCTURE
# ----------------------------------------

# 3.1 Inspect variable types and dataset structure
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

# 4.1 Count unique campaign entities
campaigns %>%
  summarise(
    households = n_distinct(household_id),
    campaigns = n_distinct(campaign_id)
  )

# 4.2 Count unique transaction entities
transactions_sample %>%
  summarise(
    households = n_distinct(household_id),
    stores = n_distinct(store_id),
    baskets = n_distinct(basket_id),
    products = n_distinct(product_id),
    weeks = n_distinct(week)
  )

# 4.3 Check transaction date range
transactions_sample %>%
  summarise(
    first_transaction = min(transaction_timestamp),
    last_transaction = max(transaction_timestamp)
  )

# 4.4 Inspect quantity distribution
transactions_sample %>%
  count(quantity, sort = TRUE)

# 4.5 Check zero and negative transaction values
transactions_sample %>%
  summarise(
    zero_quantity = sum(quantity == 0, na.rm = TRUE),
    negative_quantity = sum(quantity < 0, na.rm = TRUE),
    zero_sales = sum(sales_value == 0, na.rm = TRUE),
    negative_sales = sum(sales_value < 0, na.rm = TRUE)
  )

# 4.6 Check coupon and discount usage frequency
transactions_sample %>%
  summarise(
    coupon_rows = sum(coupon_disc != 0, na.rm = TRUE),
    coupon_match_rows = sum(coupon_match_disc != 0, na.rm = TRUE),
    retail_discount_rows = sum(retail_disc != 0, na.rm = TRUE)
  )

# 4.7 Check campaign exposure per household
campaigns %>%
  count(
    household_id,
    name = "campaigns_received"
  ) %>%
  count(campaigns_received)

# 4.8 Check coupon redemption frequency per household
coupon_redemptions %>%
  count(
    household_id,
    name = "redemptions"
  ) %>%
  arrange(desc(redemptions))

# 4.9 Inspect store IDs
transactions_sample %>%
  distinct(store_id) %>%
  arrange(store_id)

# 4.10 Select target household for case study
target_household <- "256"

# 4.11 Inspect campaigns received by target household
campaigns %>%
  filter(household_id == target_household) %>%
  left_join(
    campaign_descriptions,
    by = "campaign_id"
  ) %>%
  arrange(start_date)

# 4.12 Inspect coupons redeemed by target household
coupon_redemptions %>%
  filter(household_id == target_household) %>%
  arrange(redemption_date)

# 4.13 Inspect target household transactions
transactions_sample %>%
  filter(household_id == target_household) %>%
  arrange(transaction_timestamp)

# 4.14 Inspect target household demographics
demographics %>%
  filter(household_id == target_household)

# 4.15 Join target household redemptions to eligible products
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

# 5.1 Count eligible products per coupon
coupon_eligibility <- coupons %>%
  count(
    campaign_id,
    coupon_upc,
    name = "eligible_products"
  )

# 5.2 Summarize overall coupon eligibility distribution
coupon_eligibility %>%
  summarise(
    coupons = n(),
    min_products = min(eligible_products),
    median_products = median(eligible_products),
    mean_products = mean(eligible_products),
    max_products = max(eligible_products)
  )

# 5.3 Inspect coupons with largest eligible product sets
coupon_eligibility %>%
  arrange(desc(eligible_products)) %>%
  head(20)

# 5.4 Inspect coupon breadth for target household redemptions
coupon_redemptions %>%
  filter(household_id == target_household) %>%
  distinct(
    campaign_id,
    coupon_upc
  ) %>%
  left_join(
    coupon_eligibility,
    by = c("campaign_id", "coupon_upc")
  ) %>%
  arrange(desc(eligible_products))


# ----------------------------------------
# 6. COUPON BREADTH VS REDEMPTION
# ----------------------------------------

# 6.1 Count coupon redemptions
coupon_redemption_counts <- coupon_redemptions %>%
  count(
    campaign_id,
    coupon_upc,
    name = "redemptions"
  )

# 6.2 Build coupon performance table
coupon_performance <- coupon_eligibility %>%
  left_join(
    coupon_redemption_counts,
    by = c("campaign_id", "coupon_upc")
  ) %>%
  mutate(
    redemptions = coalesce(redemptions, 0)
  )

# 6.3 Inspect narrowest coupons
coupon_performance %>%
  arrange(
    eligible_products,
    desc(redemptions)
  ) %>%
  head(20)

# 6.4 Inspect most redeemed coupons
coupon_performance %>%
  arrange(desc(redemptions)) %>%
  head(20)

# 6.5 Inspect broadest coupons
coupon_performance %>%
  arrange(desc(eligible_products)) %>%
  head(20)

# 6.6 Create coupon breadth groups
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

# 6.7 Summarize redemption by coupon breadth
coupon_performance %>%
  group_by(breadth_group) %>%
  summarise(
    coupons = n(),
    total_redemptions = sum(redemptions),
    avg_redemptions = mean(redemptions),
    median_redemptions = median(redemptions)
  )

# 6.8 Calculate campaign reach
campaign_reach <- campaigns %>%
  count(
    campaign_id,
    name = "households_targeted"
  )

# 6.9 Add campaign reach and normalized redemption rate
coupon_performance <- coupon_performance %>%
  left_join(
    campaign_reach,
    by = "campaign_id"
  ) %>%
  mutate(
    redemptions_per_100_households =
      100 * redemptions / households_targeted
  )

# 6.10 Rank coupons by normalized redemption rate
coupon_performance %>%
  arrange(desc(redemptions_per_100_households)) %>%
  select(
    campaign_id,
    coupon_upc,
    eligible_products,
    households_targeted,
    redemptions,
    redemptions_per_100_households
  ) %>%
  head(30)

# 6.11 Rank normalized redemption rates
#      among campaigns reaching at least 100 households
coupon_performance %>%
  filter(households_targeted >= 100) %>%
  arrange(desc(redemptions_per_100_households)) %>%
  select(
    campaign_id,
    coupon_upc,
    eligible_products,
    households_targeted,
    redemptions,
    redemptions_per_100_households
  ) %>%
  head(30)

# 6.12 Compare coupon performance within campaigns
coupon_performance %>%
  filter(households_targeted >= 100) %>%
  group_by(campaign_id) %>%
  mutate(
    campaign_avg_rate =
      mean(redemptions_per_100_households),
    
    relative_performance =
      redemptions_per_100_households - campaign_avg_rate
  ) %>%
  ungroup() %>%
  arrange(desc(relative_performance)) %>%
  select(
    campaign_id,
    coupon_upc,
    eligible_products,
    households_targeted,
    redemptions,
    redemptions_per_100_households,
    campaign_avg_rate,
    relative_performance
  ) %>%
  head(30)