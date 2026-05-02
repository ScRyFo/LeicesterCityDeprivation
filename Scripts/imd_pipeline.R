# ================================
# 0. INSTALL PACKAGES (RUN ONCE)
# ================================
# install.packages(c("tidyverse","readxl","janitor","ggtext"))

# ================================
# 1. PARAMETERS
# ================================

year <- 2025

multi_domains <- c("income_score", "health_score")
population_col <- "pop_2022"

data_url <- "https://data.leicester.gov.uk/explore/dataset/deprivation-in-leicester-2025/download/?format=xlsx"

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
# 5. VALIDATE COLUMNS
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
    domain_label = case_when(
      domain == "income_score_z" ~ "Income Deprivation",
      domain == "health_score_z" ~ "Health Deprivation"
    )
  )

# ================================
# 7. SELECT WARDS (BASED ON INCOME)
# ================================

selected_wards <- multi_domain %>%
  filter(domain_label == "Income Deprivation") %>%
  arrange(desc(z_score)) %>%
  slice(c(1:2, (n()-1):n(), round(n()/2))) %>%
  pull(ward_name)

multi_domain_plot <- multi_domain %>%
  filter(ward_name %in% selected_wards)

# ================================
# 8. ORDERING
# ================================

multi_domain_plot <- multi_domain_plot %>%
  group_by(ward_name) %>%
  mutate(order_income = z_score[domain_label == "Income Deprivation"]) %>%
  ungroup()

# ================================
# 9. CORRELATION (ALL WARDS)
# ================================

cor_data <- multi_domain %>%
  select(ward_name, domain_label, z_score) %>%
  pivot_wider(names_from = domain_label, values_from = z_score)

cor_all <- cor(
  cor_data$`Income Deprivation`,
  cor_data$`Health Deprivation`,
  use = "complete.obs"
)

# Optional: correlation of selected wards only
cor_selected <- cor(
  multi_domain_plot %>%
    select(ward_name, domain_label, z_score) %>%
    pivot_wider(names_from = domain_label, values_from = z_score) %>%
    pull(`Income Deprivation`),
  
  multi_domain_plot %>%
    select(ward_name, domain_label, z_score) %>%
    pivot_wider(names_from = domain_label, values_from = z_score) %>%
    pull(`Health Deprivation`)
)

# ================================
# 10. PLOT
# ================================

ggplot(multi_domain_plot, aes(
  x = z_score,
  y = reorder(ward_name, order_income),
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
    values = c(
      "Income Deprivation" = "#1f4e79",
      "Health Deprivation" = "#e46c0a"
    )
  ) +
  
  guides(fill = "none") +
  
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  
  coord_cartesian(clip = "off") +
  
  annotate(
    "text",
    x = max(multi_domain_plot$z_score) + 0.3,
    y = -Inf,
    label = paste0(
      "r = ", round(cor_all, 2),
      " (All wards in Leicester)"
    ),
    hjust = 1,
    vjust = -1,
    size = 2,
    fontface = "italic"
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
    
    plot.margin = margin(10, 40, 20, 10)
  ) +
  
  labs(
    title = paste0(
      "<span style='color:#1f4e79;'>Income Deprivation</span> and ",
      "<span style='color:#e46c0a;'>Health Deprivation</span> by Ward in Leicester"
    )
  )