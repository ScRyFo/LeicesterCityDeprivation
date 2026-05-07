# Please only edit Sections 0 and 1. The rest of the sections
# should work without any editing

# ================================
# 0. INSTALL PACKAGES (RUN ONCE)
# ================================
# install.packages(c("tidyverse","readxl","janitor","ggtext"))
# Unhash the line above if this is the first time you are using this script

# ================================
# 1 USER-SELECTION PARAMETERS
# ================================

year <- 2025
# Only relevant if the raw data has multiple years of data in it

multi_domains_to_compare <- c("income_score", "health_score") # change these if necessary
# Examples:
# c("income_score", "crime_score")
# c("idaci_score", "health_score")

# The sub-section below allows flexible ward selection:
# - "auto": selects most/least/middle deprived wards
# - "manual": user-defined wards
# This improves reproducibility while enabling targeted analysis

ward_selection_mode <- "auto"  # "auto" or "manual"

user_selected_wards <- c( # select as many or as few wards as you would like to see
  "Eyres Monsell", 
  "Wycliffe",
  "Evington"
)

# ============================================
# 2 PARAMETER VALIDATION (!DO NOT EDIT!)
# ============================================

if (!ward_selection_mode %in% c("auto", "manual")) {
  stop("ward_selection_mode must be 'auto' or 'manual'")
}

if (!is.character(multi_domains_to_compare) || length(multi_domains_to_compare) < 1) {
  stop("multi_domains_to_compare must be a character vector")
}

# ================================
# 3. DOWNLOAD DATA
# ================================

data_url <- paste0(
  "https://data.leicester.gov.uk/explore/dataset/",
  "deprivation-in-leicester-", year,
  "/download/?format=xlsx"
)

dir.create("data", showWarnings = FALSE)

file_path <- paste0(
  "data/deprivation-in-leicester-", year, ".xlsx"
)

if (!file.exists(file_path)) {
  download.file(data_url, file_path, mode = "wb")
}

# ================================
# 4. LOAD PACKAGES
# ================================

library(tidyverse)
library(readxl)
library(janitor)
library(ggtext)

# ================================
# 5. LOAD & CLEAN DATA
# ================================

imd <- read_excel(file_path) %>%
  clean_names()

# ================================
# 6. VALIDATE INPUT
# ================================

required_cols <- c("ward_name", multi_domains_to_compare)

missing <- setdiff(required_cols, names(imd))
if (length(missing) > 0) {
  stop(paste("Missing columns:", paste(missing, collapse = ", ")))
}

# ================================
# 7. MULTI-DOMAIN PIPELINE
# ================================

multi_domain <- imd %>%
  select(ward_name, all_of(multi_domains_to_compare)) %>%
  group_by(ward_name) %>%
  summarise(
    across(everything(), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      all_of(multi_domains_to_compare),
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
    domain_label = str_remove(domain, "_score_z") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )

# ================================
# 8. SELECT WARDS (AUTO OR MANUAL)
# ================================

reference_domain <- str_to_title(
  str_replace_all(multi_domains_to_compare[1], "_score", "")
)

if (ward_selection_mode == "auto") {
  
  selected_wards <- multi_domain %>%
    filter(domain_label == reference_domain) %>%
    arrange(desc(z_score)) %>%
    summarise(
      top = list(head(ward_name, 2)),
      bottom = list(tail(ward_name, 2)),
      middle = list(ward_name[which.min(abs(z_score))])
    ) %>%
    unlist() %>%
    unique()
  
  if (length(selected_wards) == 0) {
    stop("ERROR: No wards selected in AUTO mode.")
  }
  
  message("AUTO mode | Reference domain: ", reference_domain)
  
} else if (user_selection_mode == "manual") {
  
  valid_wards <- unique(multi_domain$ward_name)
  
  invalid <- setdiff(user_selected_wards, valid_wards)
  
  if (length(invalid) > 0) {
    stop(paste("Invalid wards:", paste(invalid, collapse = ", ")))
  }
  
  selected_wards <- user_selected_wards
  
  message("MANUAL mode")
  
} else {
  stop("ward_mode must be 'auto' or 'manual'")
}

multi_domain_plot <- multi_domain %>%
  filter(ward_name %in% selected_wards)

# ================================
# 9. ORDERING
# ================================

multi_domain_plot <- multi_domain_plot %>%
  group_by(ward_name) %>%
  mutate(order_ref = z_score[domain_label == reference_domain]) %>%
  ungroup()

# ================================
# 10. CORRELATION
# ================================

if (length(multi_domains_to_compare) == 2) {
  
  cor_data <- multi_domain %>%
    select(ward_name, domain_label, z_score) %>%
    pivot_wider(names_from = domain_label, values_from = z_score)
  
  cor_all <- cor(cor_data[[2]], cor_data[[3]], use = "complete.obs")
  
  effect_size_label <- case_when(
    abs(cor_all) < 0.1 ~ "negligible",
    abs(cor_all) < 0.3 ~ "small",
    abs(cor_all) < 0.5 ~ "moderate",
    TRUE ~ "large"
  )
  
} else {
  cor_all <- NA
  effect_size_label <- "N/A"
}

# ================================
# 11. COLOUR SETUP
# ================================

domain_levels <- unique(multi_domain_plot$domain_label)

if (length(domain_levels) == 0) {
  stop("No domain labels available.")
}

domain_colours <- setNames(
  scales::hue_pal()(length(domain_levels)),
  domain_levels
)

# ================================
# 12. TITLE
# ================================

title_text <- paste0(
  mapply(function(dom, col) {
    paste0(
      "<span style='color:", col, ";'>",
      str_to_title(str_replace_all(dom, "_score", "")),
      "</span>"
    )
  }, multi_domains_to_compare, domain_colours),
  collapse = " and "
)

# ================================
# 13. PLOT
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
  
  scale_fill_manual(values = domain_colours) +
  
  guides(fill = "none") +
  
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  
  coord_cartesian(clip = "off") +
  
  # Better ↔ Worse
  annotate("segment",
           x = min(multi_domain_plot$z_score),
           xend = max(multi_domain_plot$z_score),
           y = Inf, yend = Inf,
           arrow = arrow(ends = "both", length = unit(0.25, "cm"))) +
  
  annotate("text",
           x = min(multi_domain_plot$z_score),
           y = Inf,
           label = "Better",
           hjust = 1.2,
           colour = "#2ca25f",
           size = 4,
           fontface = "bold") +
  
  annotate("text",
           x = max(multi_domain_plot$z_score),
           y = Inf,
           label = "Worse",
           hjust = -0.2,
           colour = "#de2d26",
           size = 4,
           fontface = "bold") +
  
  # Coloured background in the chart
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
  
  # Correlation
  annotate(
    "text",
    x = max(multi_domain_plot$z_score) + 0.3,
    y = -Inf,
    label = if (!is.na(cor_all)) {
      paste0("r = ", round(cor_all, 2),
             " (", effect_size_label, ")\nAll wards")
    } else {
      "Correlation only for 2 domains"
    },
    hjust = 1,
    vjust = -1,
    size = 2,
    fontface = "italic"
  ) +
  
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    
    plot.title = element_markdown(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.margin = margin(30, 40, 25, 10)
  ) +
  
  labs(
    title = paste0(title_text, " by Ward in Leicester")
  )

# ================================
# 14. VALIDATION
# ================================

# This validation block ensures the analytical pipeline is robust to:
# - Changes in IMD data structure
# - Missing or malformed variables
# - Statistical inconsistencies
# It enforces reproducibility and prevents silent analytical errors

message("Running checks...")

if (nrow(multi_domain_plot) == 0) stop("No data to plot")

if (any(is.na(multi_domain$z_score))) stop("Missing z-scores")

if (sd(multi_domain$z_score) == 0) stop("Zero variance")

if (!is.na(cor_all) && abs(cor_all) > 1) {
  stop("Invalid correlation")
}

message("All checks passed ✅")