library(shiny)
library(shinydashboard)
library(plotly)
library(openxlsx)
library(sf)
library(dplyr)
library(tidyr)
library(DT)
library(leaflet)
library(classInt)
library(shinyWidgets)
library(shinyjs)
# library(mapview)
# library(webshot)
library(jsonlite)
library(tibble)
library(purrr)
library(echarts4r)
library(highcharter)
library(htmltools)
library(httr2)
library(promises)
library(future)
library(later)

source("modules/map_module.R")
source("modules/hc_line_module.R")
source("fx/get_jstree_data.R")
source("modules/camera_module_v2.R")
# source("camera_module.R")
source("modules/about_spp_module.R")
source("modules/spp_list_module.R")

# Define the data fetcher
if (!dir.exists("data_cache")) dir.create("data_cache")
if (!dir.exists("public_assets")) dir.create("public_assets")

# 2. Map the public folder to the /docs prefix
# This makes your .webp files and .pdf available to the UI
shiny::addResourcePath(prefix = "docs", directoryPath = "public_assets")

# 3. Data loading helper
get_data <- function(file_name) {
  # Logic: Look in data_cache first (already there in Docker)
  # If missing (local dev), download it
  path <- file.path("data_cache", file_name)
  if (!file.exists(path)) {
    url <- paste0("https://raw.githubusercontent.com/fsotoj/spp-data/main/data/", file_name)
    download.file(url, path, mode = "wb")
  }
  return(path)
}


ui_assets <- c(
  "agustina.webp", "francisco.webp", "guadalupe.webp",
  "felipe.webp", "sergio.webp", "magdalena.webp",
  "EscuelaCienciasSocialesyGobierno_Horizontal_Blanco.webp",
  "EscuelaCienciasSocialesyGobierno_Horizontal_Negro.webp"
)

for (asset in ui_assets) {
  local_path <- file.path("public_assets", asset)
  if (!file.exists(local_path)) {
    # Note: ensure these are in the 'public' folder of your spp-data repo
    url <- paste0("https://raw.githubusercontent.com/fsotoj/spp-data/main/public/", asset)
    download.file(url, local_path, mode = "wb", quiet = TRUE)
  }
}


# get data ----------------------------------------------------------------


data_info <- read.xlsx(get_data("dict_new.xlsx")) %>%
  select(Category = category, Variable = variable, Description = description_for_ui)

dict <- read.xlsx(get_data("dict_new.xlsx")) %>%
  filter(scope == "subnational")

sled_names <- dict %>%
  filter(dataset == "Legislative Elections", viewable_map == 1) %>%
  pull(variable)


NED <- read.xlsx(get_data("NED (v.0.1).xlsx")) %>%
  mutate(
    ideo_party_nat_exe = as.double(ideo_party_nat_exe),
    start_date_head_nat_exe = as.Date(start_date_head_nat_exe - 2, origin = "1900-01-01"),
    end_date_head_nat_exe = as.Date(end_date_head_nat_exe - 2, origin = "1900-01-01")
  )

SEED <- read.xlsx(get_data("SEED SHINY (v.0.1).xlsx"))
SED <- read.xlsx(get_data("SED (v.0.1).xlsx"))
SLED <- read.xlsx(get_data("SLED_plus_mex.xlsx"))
SLED_ARG <- read.xlsx(get_data("SLED_ARG.xlsx"))
SDI <- read.xlsx(get_data("SDI (v.1).xlsx"))

cols_to_fill <- c("chamber_sub_leg", as.vector(outer(setdiff(sled_names, c("chamber_sub_leg", "concurrent_election_with_nat_sub_leg")), c("_1", "_2"), paste0)))

SLED_wide <- SLED %>%
  select(country_state_code, year, chamber_election_sub_leg, all_of(sled_names)) %>%
  distinct() %>%
  pivot_wider(
    id_cols     = c(country_state_code, year, chamber_sub_leg),
    names_from  = chamber_election_sub_leg,
    values_from = c(all_of(sled_names), -chamber_sub_leg),
    names_glue  = "{.value}_{chamber_election_sub_leg}",
    # values_fn   = ~ dplyr::last(., na_rm = TRUE),   # collapse duplicates
    values_fill = NA # fill absent cells with NA
  ) %>%
  arrange(country_state_code, year) %>%
  # carry-forward within each chamber separately
  group_by(country_state_code) %>%
  complete(year = seq(min(year, na.rm = TRUE), max(year, na.rm = TRUE), 1)) %>%
  arrange(country_state_code, year) %>%
  mutate(across(
    starts_with("concurrent_election_with_nat_sub_leg_"),
    ~ replace(., is.na(.), 0)
  )) %>%
  fill(all_of(cols_to_fill), .direction = "down") %>%
  ungroup()


data <- left_join(NED, SED, c("country_name", "country_code", "year")) %>%
  left_join(., SEED, c("country_state_code", "year")) %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x")) %>%
  left_join(., SLED_wide, c("country_state_code", "year")) %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x")) %>%
  left_join(., SDI, c("country_state_code", "year")) %>%
  select(-matches("\\.y$")) %>%
  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x"))


geom <- st_read(get_data("geom_simple_maps.geojson"))

party_colors <- read.xlsx(get_data("party_colors.xlsx"))
party_colors_leg <- read.xlsx(get_data("party_colors_leg_v3.xlsx"))


country_bboxes <- list(
  ARGENTINA = list(lng1 = -73.5, lat1 = -59, lng2 = -56, lat2 = -21.8),
  BRAZIL    = list(lng1 = -73.9, lat1 = -33.7, lng2 = -44.5, lat2 = 5.3),
  MEXICO    = list(lng1 = -118.5, lat1 = 12.5, lng2 = -84.7, lat2 = 34.7)
  # ,
  # `Select a country`  = list(lng1 = -118.5, lat1 = -55.1, lng2 = -34.8, lat2 = 32.7)
)
