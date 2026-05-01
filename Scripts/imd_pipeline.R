# ================================
# 1. SET PARAMETERS (EDIT THESE)
# ================================

year <- 2025

domain <- "income_score"  
# Try: "income_score", "health_score", "crime_score"

population_col <- "pop_2022"
# If you want population-weighted, change this to the most up-to-date population
# reference.
# Some IMD domains refer to smaller subsets of the population, so caution
# needs to be taken when analysing the data.

selected_wards <- unique(c(
  # extremes
  ward_z %>% arrange(desc(z_score)) %>% slice(1:2) %>% pull(ward),
  ward_z %>% arrange(z_score) %>% slice(1:2) %>% pull(ward),
  
  # middle
  ward_z %>%
    mutate(distance = abs(z_score)) %>%
    arrange(distance) %>%
    slice(1:1) %>%
    pull(ward)
))
# Automates which Wards to plot based on the most and least deprived.

file_path <- paste0("data/deprivation-in-leicester-", year, ".xlsx")

# ================================
# 2. LOAD PACKAGES
# ================================

library(tidyverse)
library(readxl)
library(janitor)

# ================================
# 3. LOAD & CLEAN DATA
# ================================

imd <- read_excel(file_path, col_names = TRUE) %>%
  clean_names()

# ================================
# 4. VALIDATE REQUIRED COLUMNS
# ================================

required_cols <- c("lsoa_name", "ward", domain)

missing_cols <- setdiff(required_cols, names(imd))

if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# ================================
# 5. SELECT & PREP DATA
# ================================

imd_selected <- imd %>%
  select(
    lsoa_name,
    ward,
    all_of(population_col),
    all_of(domain)
  ) %>%
  rename(
    score = all_of(domain),
    population = all_of(population_col)
  )

# ================================
# 6. AGGREGATE TO WARD LEVEL
# ================================

ward_data <- imd_selected %>%
  group_by(ward) %>%
  summarise(
    mean_score = weighted.mean(score, population, na.rm = TRUE),
    n_lsoas = n(),
    .groups = "drop"
  )

# ================================
# 7. STANDARDISE (Z-SCORE)
# ================================

ward_z <- ward_data %>%
  mutate(
    z_score = (mean_score - mean(mean_score)) / sd(mean_score)
  )

# ================================
# 8. FILTER SELECTED WARDS
# ================================

plot_data <- ward_z %>%
  filter(ward %in% selected_wards)

# ================================
# 9. PLOT (SINGLE DOMAIN)
# ================================

ggplot(plot_data, aes(x = z_score, y = reorder(ward, z_score))) +
  geom_col(fill = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = paste("Deprivation Analysis -", year),
    subtitle = paste("Domain:", domain),
    x = "Z-score (relative to Leicester average)",
    y = "Ward"
  ) +
  theme_minimal()

# ================================
# 10. MULTI-DOMAIN COMPARISON
# ================================

multi_domains <- c("income_score", "health_score")

# Check they exist
missing_multi <- setdiff(multi_domains, names(imd))
if (length(missing_multi) > 0) {
  stop(paste("Missing multi-domain columns:", paste(missing_multi, collapse = ", ")))
}

multi_domain <- imd %>%
  select(ward, all_of(multi_domains)) %>%
  group_by(ward) %>%
  summarise(
    across(everything(), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      all_of(multi_domains),
      ~ (.x - mean(.x)) / sd(.x),
      .names = "{.col}_z"
    )
  ) %>%
  select(ward, ends_with("_z")) %>%
  filter(ward %in% selected_wards) %>%
  pivot_longer(
    cols = -ward,
    names_to = "domain",
    values_to = "z_score"
  )

ggplot(multi_domain, aes(x = z_score, y = ward, fill = domain)) +
  geom_col(position = "dodge") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = paste("Multi-Domain Deprivation Comparison -", year),
    x = "Z-score (standardised)",
    y = "Ward"
  ) +
  theme_minimal()

# ================================
# 11. DATA VALIDATION CHECKS
# ================================

# Purpose:
# Ensure the dataset is valid before producing outputs.
# These checks prevent silent errors and improve reproducibility.

# ---- Check 1: Missing values in key variables ----
if (any(is.na(plot_data$z_score))) {
  stop("ERROR: Missing values detected in z_score. 
Check standardisation or input data.")
}

if (any(is.na(plot_data$ward))) {
  stop("ERROR: Missing values detected in ward names. 
Check data import and cleaning steps.")
}

# ---- Check 2: Ensure sufficient number of wards ----
if (nrow(plot_data) < 3) {
  stop("ERROR: Too few wards selected. 
Check filtering or selection logic.")
}

# ---- Check 3: Check z-score range (sanity check) ----
if (any(plot_data$z_score < -5 | plot_data$z_score > 5)) {
  warning("WARNING: Unusual z-score values detected. 
Check for data scaling issues.")
}

# ---- Check 4: Population weighting sanity (if used) ----
if ("population" %in% names(imd_selected)) {
  
  if (any(is.na(imd_selected$population))) {
    warning("WARNING: Missing population values detected. 
Weighted means may be inaccurate.")
  }
  
  if (any(imd_selected$population <= 0)) {
    stop("ERROR: Invalid population values (<= 0) detected.")
  }
}

# ---- Check 5: Domain column validation ----
if (!"score" %in% names(imd_selected)) {
  stop("ERROR: Score column not found. 
Check domain selection step.")
}

# ---- Final confirmation ----
message("Data validation passed: dataset is clean and ready for analysis.")