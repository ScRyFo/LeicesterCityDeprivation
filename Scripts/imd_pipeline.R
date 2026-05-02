# ================================
# 0. INSTALL PACKAGES (RUN ONCE)
# ================================

# install.packages(c("tidyverse","readxl","janitor","ggtext","httr"))

# ================================
# 1. SET PARAMETERS
# ================================

year <- 2025

domain <- "income_score"  
multi_domains <- c("income_score", "health_score")

population_col <- "pop_2022"

# Data source (Leicester open data)
data_url <- "https://data.leicester.gov.uk/explore/dataset/deprivation-in-leicester-2025/download/?format=xlsx"

# Local storage
dir.create("data", showWarnings = FALSE)
file_path <- paste0("data/deprivation-in-leicester-", year, ".xlsx")

# ================================
# 2. DOWNLOAD DATA (IF NOT EXISTING)
# ================================

if (!file.exists(file_path)) {
  message("Downloading IMD data...")
  download.file(data_url, destfile = file_path, mode = "wb")
} else {
  message("Using existing local data file.")
}

# ================================
# 3. LOAD PACKAGES
# ================================

library(tidyverse)
library(readxl)
library(janitor)
library(ggtext)

# ================================
# 4. LOAD & CLEAN DATA
# ================================

imd <- read_excel(file_path, col_names = TRUE) %>%
  clean_names()

# ================================
# 5. VALIDATE REQUIRED COLUMNS
# ================================

required_cols <- c("lsoa_name", "ward_name", domain, population_col)

missing_cols <- setdiff(required_cols, names(imd))

if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# ================================
# 6. SELECT & PREP DATA
# ================================

imd_selected <- imd %>%
  select(
    lsoa_name,
    ward_name,
    all_of(population_col),
    all_of(domain)
  ) %>%
  rename(
    score = all_of(domain),
    population = all_of(population_col)
  )

# ================================
# 7. AGGREGATE TO WARD LEVEL
# ================================

ward_data <- imd_selected %>%
  group_by(ward_name) %>%
  summarise(
    mean_score = weighted.mean(score, population, na.rm = TRUE),
    n_lsoas = n(),
    .groups = "drop"
  )

# ================================
# 8. STANDARDISE (Z-SCORE)
# ================================

ward_z <- ward_data %>%
  mutate(
    z_score = (mean_score - mean(mean_score)) / sd(mean_score)
  )

# ================================
# 9. AUTOMATIC WARD SELECTION
# ================================

selected_wards <- unique(c(
  # most deprived
  ward_z %>% arrange(desc(z_score)) %>% slice(1:2) %>% pull(ward_name),
  
  # least deprived
  ward_z %>% arrange(z_score) %>% slice(1:2) %>% pull(ward_name),
  
  # closest to average
  ward_z %>%
    mutate(distance = abs(z_score)) %>%
    arrange(distance) %>%
    slice(1:1) %>%
    pull(ward_name)
))

# ================================
# 10. MULTI-DOMAIN ANALYSIS
# ================================

multi_domain <- imd %>%
  select(ward_name, all_of(multi_domains)) %>%
  group_by(ward_name) %>%
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
  ) %>%
  filter(ward_name %in% selected_wards)

# Order by income deprivation
multi_domain <- multi_domain %>%
  group_by(ward_name) %>%
  mutate(order_income = z_score[domain_label == "Income Deprivation"]) %>%
  ungroup()

# ================================
# 11. FINAL PRESENTATION PLOT
# ================================

ggplot(multi_domain, aes(
  x = z_score,
  y = reorder(ward_name, order_income),
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
    size = 2,
    fontface = "bold"
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
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
    arrow = arrow(ends = "both", length = unit(0.2, "cm"))
  ) +
  
  annotate(
    "text",
    x = min(multi_domain$z_score),
    y = Inf,
    label = "Better",
    hjust = 1.1,
    colour = "#2ca25f",
    size = 3,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = max(multi_domain$z_score),
    y = Inf,
    label = "Worse",
    hjust = -0.1,
    colour = "#de2d26",
    size = 3,
    fontface = "bold"
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_text(size = 11, face = "bold"),
    plot.title   = element_markdown(size = 18, face = "bold", hjust = 0.5),
    plot.margin  = margin(10, 40, 10, 10)
  ) +
  
  labs(
    title = paste0(
      "<span style='color:#1f4e79;'>Income Deprivation</span> and ",
      "<span style='color:#e46c0a;'>Health Deprivation</span> by Ward in Leicester"
    ),
    y = NULL
  )

# ================================
# 12. DATA VALIDATION CHECKS
# ================================

if (any(is.na(multi_domain$z_score))) {
  stop("ERROR: Missing z-scores detected.")
}

if (any(is.na(multi_domain$ward_name))) {
  stop("ERROR: Missing ward names detected.")
}

if (nrow(multi_domain) < 3) {
  stop("ERROR: Too few wards selected.")
}

if (any(multi_domain$z_score < -5 | multi_domain$z_score > 5)) {
  warning("WARNING: Unusual z-score values detected.")
}

if (any(imd_selected$population <= 0, na.rm = TRUE)) {
  stop("ERROR: Invalid population values detected.")
}

message("Pipeline complete: data downloaded, processed, validated, and plotted.")