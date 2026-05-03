# ================================
# 0. INSTALL PACKAGES (RUN ONCE)
# ================================
# install.packages(c("tidyverse","readxl","janitor","ggtext"))

# ================================
# 1. PARAMETERS
# ================================

#year <- 2025
# Only needed if the raw data has different years within it

# CHANGE THESE FREELY
multi_domains <- c("income_score", "health_score")
# e.g. c("income_score", "crime_score")
# e.g. c("idaci_score", "health_score")

data_url <- paste0(
  "https://data.leicester.gov.uk/explore/dataset/",
  "deprivation-in-leicester-", year,
  "/download/?format=xlsx"
)

dir.create("data", showWarnings = FALSE)
file_path <- paste0("data/deprivation-in-leicester-", year, ".xlsx")

# ================================
# 2. DOWNLOAD DATA
# ================================

if (!file.exists(file_path)) {
  download.file(data_url, file_path, mode = "wb")
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

imd <- read_excel(file_path) %>%
  clean_names()

# ================================
# 5. VALIDATE INPUT
# ================================

required_cols <- c("ward_name", multi_domains)

missing <- setdiff(required_cols, names(imd))
if (length(missing) > 0) {
  stop(paste("Missing columns:", paste(missing, collapse = ", ")))
}

# ================================
# 6. MULTI-DOMAIN PIPELINE
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
    # 🔑 GENERAL LABELING (NO HARDCODING)
    domain_label = str_remove(domain, "_score_z") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )

# ================================
# 7. SELECT WARDS (ROBUST FIX)
# ================================

# Use FIRST domain as reference (not hardcoded to income)
reference_domain <- unique(multi_domain$domain_label)[1]

ward_rank <- multi_domain %>%
  filter(domain_label == reference_domain) %>%
  arrange(desc(z_score))

n_wards <- nrow(ward_rank)

selected_wards <- unique(c(
  head(ward_rank$ward_name, 2),              # top 2
  tail(ward_rank$ward_name, 2),              # bottom 2
  ward_rank$ward_name[round(n_wards / 2)]    # middle
))

multi_domain_plot <- multi_domain %>%
  filter(ward_name %in% selected_wards)

# ================================
# 8. ORDERING
# ================================

multi_domain_plot <- multi_domain_plot %>%
  group_by(ward_name) %>%
  mutate(order_ref = z_score[domain_label == reference_domain]) %>%
  ungroup()

# ================================
# 9. CORRELATION (ALL WARDS)
# ================================

cor_data <- multi_domain %>%
  select(ward_name, domain_label, z_score) %>%
  pivot_wider(names_from = domain_label, values_from = z_score)

# Only works for 2 domains
if (length(multi_domains) == 2) {
  
  cor_all <- cor(
    cor_data[[2]],
    cor_data[[3]],
    use = "complete.obs"
  )
  
  effect_size_label <- case_when(
    abs(cor_all) < 0.1 ~ "negligible",
    abs(cor_all) < 0.3 ~ "small",
    abs(cor_all) < 0.5 ~ "moderate",
    TRUE ~ "large"
  )
  
} else {
  cor_all <- NA
  r2 <- NA
  effect_size_label <- "N/A"
}

# ================================
# 10. PLOT
# ================================

ggplot(multi_domain_plot, aes(
  x = z_score,
  y = reorder(ward_name, order_ref),
  fill = domain_label
)) +
  
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  
  geom_text(
    aes(
      label = sprintf("%.2f", z_score),
      hjust = ifelse(z_score > 0, -0.15, 1.15)
    ),
    position = position_dodge(width = 0.7),
    size = 3
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  scale_fill_manual(
    values = scales::hue_pal()(length(unique(multi_domain_plot$domain_label)))
  ) +
  
  guides(fill = "none") +
  
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  
  coord_cartesian(clip = "off") +
  
  # Correlation annotation (only if 2 domains)
  annotate(
    "text",
    x = max(multi_domain_plot$z_score) + 0.3,
    y = -Inf,
    label = if (!is.na(cor_all)) {
      paste0(
        "r = ", round(cor_all, 2),
        " (", effect_size_label, ")\n",
        "All wards in Leicester"
      )
    } else {
      "Correlation shown only for 2 domains"
    },
    hjust = 1,
    vjust = -1,
    size = 2,
    fontface = "italic"
  ) +
  
  annotate(
    "segment",
    x = min(multi_domain_plot$z_score),
    xend = max(multi_domain_plot$z_score),
    y = Inf,
    yend = Inf,
    arrow = arrow(ends = "both", length = unit(0.25, "cm")),
    linewidth = 0.8,
    colour = "black"
  ) +
  
  annotate(
    "text",
    x = min(multi_domain_plot$z_score),
    y = Inf,
    label = "Better",
    hjust = 1.2,
    vjust = 0.5,
    colour = "#2ca25f",
    size = 4,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = max(multi_domain_plot$z_score),
    y = Inf,
    label = "Worse",
    hjust = -0.2,
    vjust = 0.5,
    colour = "#de2d26",
    size = 4,
    fontface = "bold"
  ) +
  
  annotate(
    "rect",
    xmin = min(multi_domain_plot$z_score),
    xmax = 0,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.05,
    fill = "#2ca25f"
  ) +
  
  annotate(
    "rect",
    xmin = 0,
    xmax = max(multi_domain_plot$z_score),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.05,
    fill = "#de2d26"
  ) +
  
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(30, 40, 25, 10)
  ) +
  
  labs(
    title = paste0(
      paste(
        paste0(
          "<span style='color:",
          scales::hue_pal()(length(multi_domains)),
          ";'>",
          str_to_title(str_replace_all(multi_domains, "_score", "")),
          "</span>"
        ),
        collapse = " and "
      ),
      " by Ward in Leicester"
    )
  )

# ================================
# 11. DATA VALIDATION & QA CHECKS
# ================================

message("Running validation checks...")

# ---- 1. Check dataset loaded correctly ----
if (!exists("imd") || nrow(imd) == 0) {
  stop("ERROR: IMD dataset not loaded or empty.")
}

# ---- 2. Required columns exist ----
required_cols <- c("ward_name", multi_domains)

missing_cols <- setdiff(required_cols, names(imd))

if (length(missing_cols) > 0) {
  stop(paste("ERROR: Missing required columns:",
             paste(missing_cols, collapse = ", ")))
}

# ---- 3. No missing ward names ----
if (any(is.na(imd$ward_name))) {
  stop("ERROR: Missing ward_name values detected.")
}

# ---- 4. Domain variables are numeric ----
non_numeric <- multi_domains[!sapply(imd[multi_domains], is.numeric)]

if (length(non_numeric) > 0) {
  stop(paste("ERROR: Non-numeric domain columns detected:",
             paste(non_numeric, collapse = ", ")))
}

# ---- 5. Check aggregation worked ----
if (!exists("multi_domain") || nrow(multi_domain) == 0) {
  stop("ERROR: multi_domain object not created correctly.")
}

# ---- 6. Check z-score calculation ----
if (any(is.na(multi_domain$z_score))) {
  stop("ERROR: Missing z_score values detected after standardisation.")
}

if (sd(multi_domain$z_score) == 0) {
  stop("ERROR: z_score has zero variance (standardisation failed).")
}

# ---- 7. Check ward selection ----
if (!exists("multi_domain_plot") || nrow(multi_domain_plot) == 0) {
  stop("ERROR: No wards selected for plotting.")
}

if (length(unique(multi_domain_plot$ward_name)) < 3) {
  stop("ERROR: Too few wards selected (minimum 3 required).")
}

# ---- 8. Check correlation validity ----
if (!exists("cor_all") || is.na(cor_all)) {
  stop("ERROR: Correlation could not be computed.")
}

if (abs(cor_all) > 1) {
  stop("ERROR: Invalid correlation value (outside -1 to 1).")
}

# ---- 9. Logical consistency check ----
# Income vs Health should typically be positively correlated
if (cor_all < -0.3) {
  warning("WARNING: Unexpected negative correlation between domains. Check data.")
}

# ---- 10. Extreme value sanity check ----
if (any(abs(multi_domain$z_score) > 5)) {
  warning("WARNING: Extreme z-scores detected (> |5|). Possible data issue.")
}

# ---- 11. Duplicate ward entries ----
dup_wards <- multi_domain %>%
  count(ward_name) %>%
  filter(n > length(multi_domains))

if (nrow(dup_wards) > 0) {
  warning("WARNING: Duplicate ward entries detected after aggregation.")
}

# ---- 12. Final confirmation ----
message("All validation checks passed ✅")