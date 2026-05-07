# ================================
# 0. INSTALL PACKAGES (RUN ONCE)
# ================================
install.packages(c("sf", "geojsonsf", "jsonlite", "readxl", "tidyverse"))
# Unhash the line above if this is the first time you are using this script

# ================================
# 1. LOAD PACKAGES
# ================================
library(readxl)
library(tidyverse)
library(sf)
library(jsonlite)

# ================================
# 2. IMPORT DATA
# ================================
wards <- read_excel("county_wards.xlsx") %>%
  clean_names()

# ================================
# 2. CONVERT DATA TO GEOJSON
# ================================
wards_sf <- wards %>%
  rowwise() %>%
  mutate(
    geometry = st_as_sfc(
      geo_shape,
      GeoJSON = TRUE,
      crs = 4326
    )
  ) %>%
  st_as_sf()

# ================================
# 2. EXPORT TO GEOJSON
# ================================
st_write(
  wards_sf,
  "leicester_wards.geojson",
  delete_dsn = TRUE
)