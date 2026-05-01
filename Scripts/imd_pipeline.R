# ================================
# 1. SET PARAMETERS (EDIT THESE)
# ================================

year <- 2025

domain <- "income_score"  
# Try: "income_score", "health_score", "crime_score"

selected_wards <- c(
  "Eyres Monsell",
  "Wycliffe",
  "Evington",
  "Western",
  "Knighton"
)

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
    all_of(domain)
  ) %>%
  rename(score = all_of(domain))

# ================================
# 6. AGGREGATE TO WARD LEVEL
# ================================

ward_data <- imd_selected %>%
  group_by(ward) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
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