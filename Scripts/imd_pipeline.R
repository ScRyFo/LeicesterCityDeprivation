# ================================
# 0. INSTALL PACKAGES
# ================================

install.packages("tidyverse","readxl","janitor","ggtext")
# Once downloaded, hashtag the line when running the script

# ================================
# 1. SET PARAMETERS (EDIT THESE)
# ================================
year <- 2025

domain <- "income_score"  
# Try: "income_score", "health_score", "crime_score"

#population_col <- "pop_2022"
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

library(tidyverse) # needed for the functions to manipulate and plot data
library(readxl) # needed to read Excel spreadsheet data
library(janitor)
library(ggtext)  # needed for coloured title text

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
    #all_of(population_col),
    all_of(domain)
  ) %>%
  rename(
    score = all_of(domain),
    #population = all_of(population_col)
  )

# ================================
# 6. AGGREGATE TO WARD LEVEL
# ================================

ward_data <- imd_selected %>%
  group_by(ward) %>%
  summarise(
    mean_score = weighted.mean(score, na.rm = TRUE),
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
# 9. FINAL PRESENTATION PLOT
# ================================

plot_data <- multi_domain %>%
  mutate(
    domain_label = case_when(
      domain == "income_z" ~ "Income Deprivation",
      domain == "health_z" ~ "Health Deprivation"
    )
  )

ggplot(plot_data, aes(x = z_score, y = reorder(ward, z_score), fill = domain_label)) +
  
  # Bars
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  
  # Value labels at end of bars
  geom_text(
    aes(label = round(z_score, 2)),
    position = position_dodge(width = 0.7),
    hjust = ifelse(plot_data$z_score > 0, -0.2, 1.2),
    size = 4
  ) +
  
  # Zero reference line
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  
  # Custom colours
  scale_fill_manual(
    values = c(
      "Income Deprivation" = "#2C7FB8",   # blue
      "Health Deprivation" = "#F28E2B"    # orange
    )
  ) +
  
  # Remove legend
  guides(fill = "none") +
  
  # Remove x-axis completely
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    
    # Improve spacing for labels outside bars
    plot.margin = margin(10, 40, 10, 10),
    
    # Title styling (allows coloured text)
    plot.title = element_markdown(size = 16, face = "bold")
  ) +
  
  # Expand limits so labels don’t get cut off
  expand_limits(x = max(plot_data$z_score) + 0.5) +
  
  # Title with embedded legend
  labs(
    title = paste0(
      "<span style='color:#2C7FB8;'>Income Deprivation</span> and ",
      "<span style='color:#F28E2B;'>Health Deprivation</span> by Ward in Leicester"
    ),
    y = "Ward"
  )

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
#if ("population" %in% names(imd_selected)) {
  
  #if (any(is.na(imd_selected$population))) {
   # warning("WARNING: Missing population values detected. 
#Weighted means may be inaccurate.")
#  }
  
 # if (any(imd_selected$population <= 0)) {
 #   stop("ERROR: Invalid population values (<= 0) detected.")
#  }
#}

# ---- Check 5: Domain column validation ----
if (!"score" %in% names(imd_selected)) {
  stop("ERROR: Score column not found. 
Check domain selection step.")
}

# ---- Final confirmation ----
message("Data validation passed: dataset is clean and ready for analysis.")