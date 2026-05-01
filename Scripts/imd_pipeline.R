# ================================
# 1. SET PARAMETERS (EDIT THESE)
# ================================

year <- 2025

domain <- "Income_score"  
# Options (depending on dataset):
# "income_score", "health_score", "crime_score", etc.

selected_wards <- c(
  "Eyres Monsell",
  "Wycliffe",
  "Evington",
  "Western",
  "Knighton"
)

file_path <- paste0("data/deprivation-in-leicester-", year, ".xlsx", )

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

# Check columns if needed
# names(imd)

# ================================
# 4. VALIDATE DOMAIN COLUMN
# ================================

if (!(domain %in% names(imd))) {
  stop(paste("Column", domain, "not found in dataset"))
}

# ================================
# 5. SELECT REQUIRED VARIABLES
# ================================

imd_selected <- imd %>%
  select(
    'LSOA name',
    Ward,
    all_of(domain)
  )

# Rename domain column to generic name
imd_selected <- imd_selected %>%
  rename(score = all_of(domain))

# ================================
# 6. AGGREGATE TO WARD LEVEL
# ================================

ward_data <- imd_selected %>%
  group_by(ward) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
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
# 9. PLOT
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
# 10. OPTIONAL: MULTI-DOMAIN COMPARISON
# ================================

multi_domain <- imd %>%
  select(
    ward,
    income_score,
    health_score
  ) %>%
  group_by(ward) %>%
  summarise(
    income_mean = mean(income_score, na.rm = TRUE),
    health_mean = mean(health_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    income_z = (income_mean - mean(income_mean)) / sd(income_mean),
    health_z = (health_mean - mean(health_mean)) / sd(health_mean)
  ) %>%
  filter(ward %in% selected_wards) %>%
  pivot_longer(
    cols = c(income_z, health_z),
    names_to = "domain",
    values_to = "z_score"
  )

ggplot(multi_domain, aes(x = z_score, y = ward, fill = domain)) +
  geom_col(position = "dodge") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = paste("Income vs Health Deprivation -", year),
    x = "Z-score",
    y = "Ward"
  ) +
  theme_minimal()