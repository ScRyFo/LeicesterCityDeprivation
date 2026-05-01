# ================================
# 0. INSTALL PACKAGES
# ================================

# install.packages("tidyverse","readxl","janitor","ggtext")
# Once downloaded, hashtag the line when running the script

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
# 9. MULTI-DOMAIN COMPARISON
# ================================

multi_domains <- c("income_score", "health_score")

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
  pivot_longer(
    cols = ends_with("_z"),
    names_to = "domain",
    values_to = "z_score"
  ) %>%
  mutate(
    domain_label = case_when(
      domain == "income_score_z" ~ "Income Deprivation",
      domain == "health_score_z" ~ "Health Deprivation"
    )
  )

# Optional: filter wards AFTER computing both domains
multi_domain <- multi_domain %>%
  filter(ward %in% selected_wards)

# Optional: sort order by income deprivation, highest to lowest
multi_domain <- multi_domain %>%
  group_by(ward) %>%
  mutate(order_income = z_score[domain_label == "Income Deprivation"]) %>%
  ungroup()

# ================================
# 10. FINAL PRESENTATION PLOT
# ================================

ggplot(multi_domain, aes(
  x = z_score,
  y = reorder(ward, order_income),
  fill = domain_label
)) +
  
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  
  geom_text(
    aes(
      label = sprintf("%.2f", z_score),
      hjust = ifelse(z_score > 0, -0.15, 1.15),
      group = domain_label
    ),
    position = position_dodge(width = 0.7),
    size = 4,
    fontface = "bold"
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  
  scale_fill_manual(
    values = c(
      "Income Deprivation" = "#1f4e79",
      "Health Deprivation" = "#e46c0a"
    )
  ) +
  
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  
  guides(fill = "none") +
  
  annotate(
    "segment",
    x = min(multi_domain$z_score),
    xend = max(multi_domain$z_score),
    y = Inf,
    yend = Inf,
    arrow = arrow(ends = "both", length = unit(0.2, "cm")),
    colour = "black",
    linewidth = 0.8
  ) +
  
  annotate(
    "text",
    x = min(multi_domain$z_score),
    y = Inf,
    label = "Better",
    hjust = 1.1,
    vjust = 0.5,
    colour = "#2ca25f",
    size = 3.5,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = max(multi_domain$z_score),
    y = Inf,
    label = "Worse",
    hjust = -0.1,
    vjust = 0.5,
    colour = "#de2d26",
    size = 3.5,
    fontface = "bold"
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.text.y = element_text(size = 11, face = "bold", colour = "black"),
    
    plot.margin = margin(10, 40, 10, 10),
    
    plot.title = element_markdown(
      size = 18,
      face = "bold",
      hjust = 0.5
    ),
    
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  
  labs(
    title = paste0(
      "<span style='color:#1f4e79;'>Income Deprivation</span> and ",
      "<span style='color:#e46c0a;'>Health Deprivation</span> by Ward in Leicester"
    ),
    y = NULL
  )

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