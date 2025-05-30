# Set up session for running maps.R

# 1. Load packages
# 2. Load functions
# 3. Set directories
# 4. Load map layer parameters
# 5. Load city parameters
# 6. Read AOI & wards

# 1. Load packages -------------------------------------------------------------
# Install packages from CRAN using librarian
if (!"librarian" %in% installed.packages()) install.packages("librarian")
librarian::shelf(quiet = T,
  # Read-in
  readxl,
  readr,
  yaml, 
  
  # Plots
  ggplot2, # 3.5 or higher
  ggrepel,
  directlabels,
  ggh4x,
  ggtext,
  plotly, 
  cowplot,
  # Spatial
  sf,
  rstudio/terra, # Only the github version of leaflet supports terra, in place of raster, which is now required as sp (on which raster depends) is being deprecated
  tidyterra, 
  leaflet, 
  ggspatial, 

  # Web
  curl,
  rvest,

  # Basic
  stringr,
  glue,
  tidyr,
  purrr,
  forcats,
  units,
  dplyr)

librarian::stock(
  ggnewscale, # 4.10 or higher
  prettymapr
)

# 2. Load functions ------------------------------------------------------------
source("R/fns.R")

# 3. Set directories -----------------------------------------------------------
city_dir <- readLines("city-dir.txt")[1]
user_input_dir <- file.path(city_dir, "01-user-input")
process_output_dir <- file.path(city_dir, "02-process-output")
spatial_dir <- file.path(process_output_dir, "spatial")
tabular_dir <- file.path(process_output_dir, "tabular")
output_dir <- file.path(city_dir, "03-render-output")
styled_maps_dir <- file.path(output_dir, "maps")
charts_dir <- file.path(output_dir, "charts")

dir.create(styled_maps_dir, recursive = T)
dir.create(charts_dir, recursive = T)

# 4. Load map layer parameters -------------------------------------------------
layer_params_file <- 'source/layers.yml' # Also used by fns.R
layer_params <- read_yaml(layer_params_file)

# 5. Load city parameters ------------------------------------------------------
city_params <- read_yaml(file.path(user_input_dir, "city_inputs.yml"))
city <- city_params$city_name
city_string <- tolower(city) %>% stringr::str_replace_all(" ", "-")
country <- city_params$country_name

# 6. Read AOI & wards ----------------------------------------------------------
aoi <- fuzzy_read(user_input_dir, "AOI") %>% project("epsg:4326")
wards <- tryCatch(fuzzy_read(user_input_dir, "wards") %>% project("epsg:4326"), error = \(e) NULL)
