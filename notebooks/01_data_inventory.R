# ----------------------------------------
# 0. LIBRARIES
# ----------------------------------------

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

# 6.11 Rank normalized redemption rates among campaigns
# reaching at least 100 households
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


# ----------------------------------------
# 7. TOP COUPON PRODUCT PROFILES
# ----------------------------------------

# 7.1 Identify top-performing coupons within campaigns
top_coupons <- coupon_performance %>%
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
  slice_head(n = 10) %>%
  select(
    campaign_id,
    coupon_upc,
    eligible_products,
    households_targeted,
    redemptions,
    redemptions_per_100_households,
    relative_performance
  )

top_coupons

# 7.2 Join top coupons to their eligible products
top_coupon_products <- top_coupons %>%
  inner_join(
    coupons,
    by = c("campaign_id", "coupon_upc")
  ) %>%
  left_join(
    products,
    by = "product_id"
  )

glimpse(top_coupon_products)

# 7.3 Summarize product breadth for each coupon
coupon_content_summary <- top_coupon_products %>%
  group_by(
    campaign_id,
    coupon_upc
  ) %>%
  summarise(
    eligible_products = n_distinct(product_id),
    departments = n_distinct(department, na.rm = TRUE),
    brand_types = n_distinct(brand, na.rm = TRUE),
    categories = n_distinct(product_category, na.rm = TRUE),
    product_types = n_distinct(product_type, na.rm = TRUE),
    manufacturers = n_distinct(manufacturer_id, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    top_coupons,
    by = c(
      "campaign_id",
      "coupon_upc",
      "eligible_products"
    )
  ) %>%
  arrange(desc(relative_performance))

coupon_content_summary

# 7.4 Inspect most common departments for each coupon
top_coupon_products %>%
  count(
    campaign_id,
    coupon_upc,
    department,
    sort = TRUE
  ) %>%
  group_by(
    campaign_id,
    coupon_upc
  ) %>%
  slice_max(
    n,
    n = 5,
    with_ties = FALSE
  ) %>%
  ungroup()

# 7.5 Inspect national vs private-label mix
top_coupon_products %>%
  count(
    campaign_id,
    coupon_upc,
    brand,
    sort = TRUE
  ) %>%
  group_by(
    campaign_id,
    coupon_upc
  ) %>%
  mutate(
    share = n / sum(n)
  ) %>%
  ungroup()

# 7.6 Inspect most common product categories
top_coupon_products %>%
  count(
    campaign_id,
    coupon_upc,
    product_category,
    sort = TRUE
  ) %>%
  filter(!is.na(product_category)) %>%
  group_by(
    campaign_id,
    coupon_upc
  ) %>%
  slice_max(
    n,
    n = 5,
    with_ties = FALSE
  ) %>%
  ungroup()

# 7.7 Inspect most common product types
top_coupon_products %>%
  count(
    campaign_id,
    coupon_upc,
    product_type,
    sort = TRUE
  ) %>%
  filter(!is.na(product_type)) %>%
  group_by(
    campaign_id,
    coupon_upc
  ) %>%
  slice_max(
    n,
    n = 5,
    with_ties = FALSE
  ) %>%
  ungroup()


# ----------------------------------------
# 8. REDEMPTION BASKET MATCHING
# ----------------------------------------

# 8.1 Download full transaction history
transactions <- get_transactions(verbose = TRUE)

# 8.2 Confirm full transaction data loaded correctly
glimpse(transactions)

transactions %>%
  summarise(
    rows = n(),
    households = n_distinct(household_id),
    baskets = n_distinct(basket_id),
    first_date = min(transaction_timestamp),
    last_date = max(transaction_timestamp)
  )

# 8.3 Add transaction date for matching
transactions <- transactions %>%
  mutate(
    transaction_date = as.Date(transaction_timestamp)
  )

# 8.4 Select two coupons for initial testing
selected_coupons <- tibble(
  campaign_id = c("12", "18"),
  coupon_upc = c(
    "10000089073",  # Focused egg offer
    "10000085475"   # Broad 4,294-product offer
  )
)

# 8.5 Retrieve redemptions for selected coupons
selected_redemptions <- coupon_redemptions %>%
  semi_join(
    selected_coupons,
    by = c("campaign_id", "coupon_upc")
  )

selected_redemptions

# 8.6 Expand redemptions to eligible product sets
redemption_eligible_products <- selected_redemptions %>%
  inner_join(
    coupons,
    by = c("campaign_id", "coupon_upc"),
    relationship = "many-to-many"
  )

# 8.7 Match eligible products to same-household purchases
redemption_purchase_matches <- redemption_eligible_products %>%
  inner_join(
    transactions,
    by = c("household_id", "product_id")
  ) %>%
  filter(
    transaction_date == redemption_date
  )

# 8.8 Inspect candidate redemption-purchase matches
redemption_purchase_matches %>%
  select(
    household_id,
    campaign_id,
    coupon_upc,
    redemption_date,
    basket_id,
    product_id,
    sales_value,
    coupon_disc,
    coupon_match_disc
  ) %>%
  arrange(
    coupon_upc,
    household_id,
    redemption_date
  )

# 8.9 Count candidate transaction lines per redemption event
redemption_match_check <- redemption_purchase_matches %>%
  count(
    household_id,
    campaign_id,
    coupon_upc,
    redemption_date,
    name = "matching_transaction_lines"
  )

redemption_match_check %>%
  count(
    matching_transaction_lines,
    name = "redemption_events"
  )

# 8.10 Identify recorded redemptions with no transaction match
selected_redemptions %>%
  anti_join(
    redemption_match_check,
    by = c(
      "household_id",
      "campaign_id",
      "coupon_upc",
      "redemption_date"
    )
  )

# 8.11 Check coupon discount values among candidate matches
redemption_purchase_matches %>%
  summarise(
    candidate_lines = n(),
    
    lines_with_coupon_discount =
      sum(coupon_disc > 0, na.rm = TRUE),
    
    lines_with_coupon_match =
      sum(coupon_match_disc > 0, na.rm = TRUE),
    
    lines_with_any_coupon_discount =
      sum(
        coupon_disc > 0 |
          coupon_match_disc > 0,
        na.rm = TRUE
      )
  )

# 8.12 Inspect candidate lines containing coupon discounts
redemption_purchase_matches %>%
  filter(
    coupon_disc > 0 |
      coupon_match_disc > 0
  ) %>%
  select(
    household_id,
    campaign_id,
    coupon_upc,
    redemption_date,
    basket_id,
    product_id,
    sales_value,
    coupon_disc,
    coupon_match_disc
  ) %>%
  arrange(
    coupon_upc,
    household_id,
    redemption_date
  )

# 8.13 Compare match quality between test coupons
redemption_match_check %>%
  group_by(coupon_upc) %>%
  summarise(
    matched_redemptions = n(),
    avg_candidate_lines = mean(matching_transaction_lines),
    median_candidate_lines = median(matching_transaction_lines),
    max_candidate_lines = max(matching_transaction_lines)
  )

# 8.14 Count unmatched redemption events by coupon
selected_redemptions %>%
  anti_join(
    redemption_match_check,
    by = c(
      "household_id",
      "campaign_id",
      "coupon_upc",
      "redemption_date"
    )
  ) %>%
  count(coupon_upc)


# ----------------------------------------
# 9. BASKET HALO - FOCUSED COUPON
# ----------------------------------------

# 9.1 Keep redemption events with exactly one eligible match
unique_redemption_matches <- redemption_purchase_matches %>%
  group_by(
    household_id,
    campaign_id,
    coupon_upc,
    redemption_date
  ) %>%
  filter(n() == 1) %>%
  ungroup()

# 9.2 Isolate clean egg-coupon redemption matches
egg_redemption_matches <- unique_redemption_matches %>%
  filter(
    campaign_id == "12",
    coupon_upc == "10000089073"
  )

egg_redemption_matches %>%
  select(
    household_id,
    redemption_date,
    basket_id,
    product_id,
    sales_value
  )

# 9.3 Retrieve every transaction line from egg-redemption baskets
egg_redemption_baskets <- transactions %>%
  semi_join(
    egg_redemption_matches %>%
      distinct(basket_id),
    by = "basket_id"
  )

# 9.4 Inspect full egg-redemption baskets
egg_redemption_baskets %>%
  arrange(
    basket_id,
    transaction_timestamp
  )

# 9.5 Summarize egg-redemption basket size and value
egg_basket_summary <- egg_redemption_baskets %>%
  group_by(
    basket_id,
    household_id
  ) %>%
  summarise(
    basket_sales = sum(sales_value, na.rm = TRUE),
    basket_quantity = sum(quantity, na.rm = TRUE),
    product_lines = n(),
    unique_products = n_distinct(product_id),
    .groups = "drop"
  )

egg_basket_summary

# 9.6 Calculate spend outside the matched coupon item
egg_basket_halo <- egg_basket_summary %>%
  left_join(
    egg_redemption_matches %>%
      select(
        basket_id,
        coupon_item_sales = sales_value
      ),
    by = "basket_id"
  ) %>%
  mutate(
    non_coupon_basket_sales =
      basket_sales - coupon_item_sales
  )

egg_basket_halo

# 9.7 Summarize basket halo across clean egg redemptions
egg_basket_halo %>%
  summarise(
    redemption_baskets = n(),
    avg_basket_sales = mean(basket_sales),
    median_basket_sales = median(basket_sales),
    avg_coupon_item_sales = mean(coupon_item_sales),
    avg_non_coupon_sales = mean(non_coupon_basket_sales),
    median_non_coupon_sales = median(non_coupon_basket_sales)
  )

# 9.8 Identify unmatched egg redemptions
unmatched_egg_redemptions <- selected_redemptions %>%
  filter(
    campaign_id == "12",
    coupon_upc == "10000089073"
  ) %>%
  anti_join(
    redemption_match_check,
    by = c(
      "household_id",
      "campaign_id",
      "coupon_upc",
      "redemption_date"
    )
  )

unmatched_egg_redemptions

# 9.9 Search for eligible purchases within one day
# of unmatched egg redemption dates
unmatched_egg_candidates <- unmatched_egg_redemptions %>%
  inner_join(
    coupons,
    by = c("campaign_id", "coupon_upc"),
    relationship = "many-to-many"
  ) %>%
  inner_join(
    transactions,
    by = c("household_id", "product_id")
  ) %>%
  mutate(
    days_from_redemption =
      as.integer(transaction_date - redemption_date)
  ) %>%
  filter(
    abs(days_from_redemption) <= 1
  ) %>%
  arrange(
    household_id,
    redemption_date,
    abs(days_from_redemption)
  )

# 9.10 Inspect nearby candidates for unmatched egg redemptions
unmatched_egg_candidates %>%
  select(
    household_id,
    redemption_date,
    transaction_date,
    days_from_redemption,
    basket_id,
    product_id,
    sales_value
  )