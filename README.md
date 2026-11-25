# From Promotion to Loyalty
### A Grocery Retail Marketing Analytics Project

**Language:** R  
**Domain:** Marketing Analytics | Customer Analytics | Retail Strategy  
**Status:** In Active Development

## Project Overview

Retailers invest heavily in coupons, discounts, and promotional campaigns to encourage customer purchases. However, high coupon redemption does not necessarily translate into incremental revenue, stronger customer engagement, or long-term profitability.

This project uses the **Complete Journey** grocery retail dataset to investigate how promotional strategies influence household purchasing behavior, shopping basket composition, and customer engagement.

The central business question is:

**Which promotional strategies generate lasting customer value rather than merely encouraging temporary discounted purchases?**

The project approaches this question from both a statistical and a business perspective, emphasizing actionable marketing insights, responsible interpretation of observational data, and the economic implications of promotional decisions.

## Business Objectives

- Evaluate coupon performance across different levels of product eligibility and campaign reach.
- Identify characteristics of successful promotional offers.
- Connect recorded coupon redemptions to household purchasing behavior.
- Investigate whether promotional purchases are associated with larger shopping baskets.
- Examine repeat purchasing and post-promotion customer engagement.
- Assess the potential economic value of promotional strategies.

The ultimate objective is to develop evidence-based recommendations for improving promotional targeting and marketing resource allocation.

## Dataset

This project utilizes the Complete Journey dataset, distributed through the `completejourney` R package and originating from dunnhumby's retail analytics data.

The dataset contains household-level purchasing activity throughout 2017, alongside marketing campaigns, coupon eligibility, redemption records, product information, and promotional placements.

| Dataset characteristic | Count |
|---|---:|
| Transaction records | 1,469,307 |
| Unique households | 2,469 |
| Shopping baskets | 155,848 |
| Marketing campaigns | 27 |
| Household-campaign assignments | 6,589 |
| Distinct campaign-coupon offers | 1,197 |
| Recorded coupon redemptions | 2,102 |
| Product metadata records | 92,331 |
| Household demographic records | 801 |

**Data source:** [dunnhumby Source Files](https://www.dunnhumby.com/source-files/)

**R package documentation:** [Complete Journey](https://bradleyboehmke.github.io/completejourney/)

## Technologies

| Technology | Application |
|---|---|
| R | Data processing and statistical analysis |
| dplyr | Data transformation, relational joins, and aggregation |
| purrr | Programmatic dataset inspection |
| tibble | Structured data representation |
| completejourney | Retail data acquisition |
| Git and GitHub | Version control and project documentation |

## Analytical Methodology

### 1. Data Inventory and Quality Assessment

The initial analysis examines the structure, granularity, coverage, and relationships of the eight core datasets.

Data-quality investigations include:

- Unique household, store, product, and basket identifiers.
- Transaction date coverage.
- Zero and negative transaction values.
- Discount and coupon usage frequency.
- Household campaign exposure.
- Coupon redemption concentration.
- Relational join cardinality and potential row duplication.

Special attention is given to preserving the distinction between transaction lines, shopping baskets, household-level observations, and campaign-level records.

### 2. Coupon Eligibility and Breadth

Coupon breadth is measured by the number of products eligible for an individual campaign-coupon offer.

The analysis reveals a highly right-skewed distribution of coupon eligibility.

| Metric | Eligible products |
|---|---:|
| Minimum | 1 |
| Median | 13 |
| Mean | 97.1 |
| Maximum | 14,477 |

Coupons range from highly specific single-product offers to broad promotions covering thousands of eligible products.

This variation provides an opportunity to investigate whether greater consumer choice is associated with stronger promotional response.

### 3. Coupon Performance and Campaign Normalization

Raw redemption counts are compared across coupon breadth groups.

| Eligible products | Coupon offers | Average redemptions per offer |
|---|---:|---:|
| 1 | 47 | 0.106 |
| 2–5 | 233 | 0.481 |
| 6–20 | 475 | 1.04 |
| 21–100 | 374 | 2.46 |
| 101–1,000 | 53 | 3.19 |
| Over 1,000 | 15 | 26.9 |

Broader coupons exhibit substantially higher observed redemption counts. However, raw comparisons do not account for differences in campaign audience size.

To address this limitation, coupon performance is normalized using the number of households targeted by each campaign.

Additional comparisons include:

- Redemptions per 100 targeted households.
- Exploratory rankings restricted to campaigns targeting at least 100 households.
- Coupon performance relative to the average offer within the same campaign.

These measures improve comparability while acknowledging that individual coupon exposure may vary within campaigns.

### 4. Product-Level Coupon Profiling

The number of eligible products alone does not fully describe the nature of a promotional offer.

To develop more interpretable coupon profiles, eligible products are linked to their:

- Departments
- Product categories
- Product types
- Manufacturers
- National or private-label brand classifications

This distinguishes broad assortment promotions from focused offers covering variations of similar products.

One particularly interesting example is a coupon covering 44 eligible egg products, consisting of 35 large-egg SKUs and nine extra-large-egg SKUs.

Despite its relatively narrow eligibility, the offer achieved strong observed redemption performance.

### 5. Redemption-to-Purchase Matching

A prototype matching process was developed to connect recorded coupon redemptions with eligible transaction lines using:

- Household identifier
- Redemption date
- Product eligibility
- Shopping basket identifier

Two contrasting promotional offers were selected for initial testing.

| Metric | Focused egg offer | Broad assortment offer |
|---|---:|---:|
| Eligible products | 44 | 4,294 |
| Recorded redemptions | 10 | 63 |
| Redemptions with same-day eligible matches | 8 | 52 |
| Same-day match coverage | 80.0% | 82.5% |
| Average candidate lines per matched redemption | 1.00 | 6.29 |

Although both offers exhibited similar matching coverage, broad promotions produced considerably greater attribution ambiguity.

The matching process therefore distinguishes plausible purchase candidates from definitively verified coupon usage.

### 6. Basket Halo Analysis

The initial basket analysis investigates whether promotional purchases occur alongside substantial purchases of other merchandise.

Eight uniquely matched egg-coupon redemption events were selected for a preliminary case study.

| Metric | Observed result |
|---|---:|
| Analyzed baskets | 8 |
| Average basket sales | $133 |
| Median basket sales | $90.70 |
| Average matched egg-item sales | $0.92 |
| Average remaining basket sales | $132 |
| Approximate basket sales range | $81–$284 |

These observations demonstrate that the matched promotional items represented only a small portion of total basket sales.

However, the presence of substantial additional merchandise does not establish that the coupon caused those purchases.

The next stage compares redemption-associated baskets with the same households' historical purchasing behavior to investigate whether these shopping trips were unusually valuable.

## Preliminary Findings

### Coupon breadth is associated with higher raw redemption counts.

Offers covering thousands of eligible products received considerably more redemptions per offer than narrowly eligible coupons. Campaign reach and targeting must be considered before drawing conclusions about promotional effectiveness.

### Focused offers can compete with broad promotions.

The focused egg coupon achieved 5.88 redemptions per 100 targeted households, compared with 5.56 for a broad offer covering 4,294 products.

This suggests that product relevance and everyday consumer utility may be important alongside the number of eligible products.

### Broad coupons introduce additional attribution challenges.

A coupon covering thousands of products may correspond to multiple eligible purchases within the same shopping trip or across multiple trips on the same day.

Reliable redemption attribution therefore requires more than a direct coupon-to-product join.

### Promotional purchases can accompany substantial basket sales.

The preliminary egg-coupon case study identified shopping baskets averaging approximately $133, while the matched promotional item averaged less than $1 in recorded sales value.

This motivates further investigation into potential basket halo effects, shopping-trip behavior, and incremental promotional value.

## Limitations

This project uses observational retail data rather than a randomized marketing experiment.

Important limitations include:

- Campaign recipients may have been selected based on existing purchasing behavior.
- Campaign assignment does not necessarily establish exposure to every individual coupon.
- Product eligibility does not establish which product received a particular coupon discount.
- Demographic information is available for only a subset of households.
- Coupon discount fields do not consistently resolve redemption-to-purchase attribution.
- Some redemption dates do not align exactly with eligible transaction dates.
- Product-level costs and complete campaign expenses are unavailable.
- Observed purchasing behavior cannot establish purchases at competing retailers or word-of-mouth referrals.

Consequently, preliminary findings are interpreted as observed associations rather than established causal effects.

## Project Roadmap

- [x] Data acquisition and inventory
- [x] Relational data exploration
- [x] Preliminary data-quality assessment
- [x] Coupon eligibility analysis
- [x] Coupon breadth and redemption analysis
- [x] Campaign-normalized performance comparisons
- [x] Product-level coupon profiling
- [x] Redemption-to-purchase matching prototype
- [x] Initial focused-coupon basket case study
- [ ] Redemption date alignment validation
- [ ] Household-level basket baseline comparisons
- [ ] Expanded basket halo analysis
- [ ] Customer behavioral segmentation
- [ ] Repeat purchasing and retention analysis
- [ ] Promotional incrementality assessment
- [ ] Marketing economics and profitability sensitivity analysis
- [ ] Final visualizations and business recommendations

## Repository Structure

**`notebooks/01_data_inventory.R`**

The current analytical script contains the complete exploratory workflow, organized into numbered sections covering data inventory, quality assessment, coupon performance, product profiling, redemption matching, and preliminary basket analysis.

As the project develops, additional analytical modules, visualizations, and reporting materials will be incorporated into the repository.

## Reproducing the Analysis

Install the required R packages:

`install.packages(c("completejourney", "dplyr", "purrr", "tibble"))`

Open and execute `notebooks/01_data_inventory.R` in RStudio.

The initial datasets are provided through the Complete Journey package. The full transaction dataset is retrieved programmatically during the analysis using `get_transactions()`.

An internet connection is required for the full transaction download.

## Project Status

This project is actively being developed as part of a business analytics portfolio, with an emphasis on connecting statistical analysis to practical marketing decisions.

Findings, analytical methods, and business recommendations will be updated as additional stages are completed.
