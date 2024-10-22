## code to prepare `DATASET` dataset goes here

library(arcpullr)
library(tidyverse)
library(sf)
library(janitor)


# california boundary -----------------------------------------------------

ca_boundary <-
  arcpullr::get_spatial_layer('https://services1.arcgis.com/qr14biwnHA6Vis6l/arcgis/rest/services/California_State_Boundary/FeatureServer/0') |>
  select(NAME) |>
  clean_names()


# nps boundaries ----------------------------------------------------------

nps_boundaries <-
  arcpullr::get_spatial_layer(
    url = 'https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/NPS_Land_Resources_Division_Boundary_and_Tract_Data_Service/FeatureServer/2',
    where = "STATE = 'CA'"
  ) |>
  select(UNIT_CODE, UNIT_NAME, PARKNAME, UNIT_TYPE) |>
  clean_names() |>
  rename(park_name = parkname) |>
  st_make_valid() |>
  nngeo::st_remove_holes()


# usfs boundaries ---------------------------------------------------------

# usfs_boundaries <-
#   arcpullr::get_spatial_layer(
#     url = 'https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_ForestSystemBoundaries_01/MapServer/0',
#     where = "Region = '05'"
#   ) |>
#   select(ADMINFORESTID:FORESTNAME) |>
#   clean_names() |>
#   rename(admin_forest_id = adminforestid, forest_number = forestnumber, forest_org_code = forestorgcode, forest_name = forestname) |>
#   st_make_valid()

usfs_boundaries <-
  st_read(
    here::here('data-raw/usfs_boundaries.shp'),
    quiet = TRUE
  ) |>
  st_make_valid() |>
  st_transform(4326)


# epa ecoregions ----------------------------------------------------------

# epa_ecoregions <-
#   arcpullr::get_spatial_layer(
#     url = 'https://edits.nationalmap.gov/arcgis/rest/services/landforms/landforms_epa_eco_regions/MapServer/0',
#     where = "US_L3NAME IN ('Sierra Nevada', 'Cascades')"
#   ) |>
#   select(NA_L3NAME) |>
#   clean_names()


# sierra hexes ------------------------------------------------------------

sierra_hexes <-
  st_read(
    here::here('data-raw/sierra_nevada_hexes.shp'),
    quiet = TRUE
  ) |>
  st_make_valid()


# sierra study area (based off of hexes) ----------------------------------

sierra_study_area <-
  sierra_hexes |>
  summarise() |>
  nngeo::st_remove_holes()


# coastal hexes -----------------------------------------------------------

mendocino_hexes <-
  st_read(
    here::here('data-raw/mendocino_hexes.shp'),
    quiet = TRUE
  ) |>
  st_make_valid() |>
  st_transform(4326)

sonoma_hexes <-
  st_read(
    here::here('data-raw/sonoma_hexes.shp'),
    quiet = TRUE
  ) |>
  st_make_valid() |>
  st_transform(4326)

# combine
coastal_hexes <-
  mendocino_hexes |>
  bind_rows(sonoma_hexes) |>
  select(cell_id = CELL_ID)


# fire perimeters ---------------------------------------------------------

# 2020-onward
# fire_perimeters <-
#   arcpullr::get_spatial_layer(
#     url = 'https://services1.arcgis.com/jUJYIo9tSA7EHvfZ/ArcGIS/rest/services/California_Fire_Perimeters/FeatureServer/2',
#     where = "YEAR_ >= 2020",
#     out_fields = c('YEAR_', 'FIRE_NAME', 'CAUSE', 'C_METHOD')
#   ) |>
#   clean_names() |>
#   st_make_valid()


# demographic study areas -------------------------------------------------

# eldorado <-
#   st_read("Y:/Data-GIS/Boundaries/Owl_demography/DSA_boundary_utm83/DSA_boundary_utm83.shp") |>
#   st_transform(4326) |>
#   summarise() |>
#   transmute(name = 'Eldorado') |>
#   nngeo::st_remove_holes()
#
# lassen <-
#   st_read("Y:/Data-GIS/Boundaries/Owl_demography/LNF_demo_utm27/Lassen_quints.shp") |>
#   st_transform(4326) |>
#   summarise() |>
#   transmute(name = 'Lassen') |>
#   nngeo::st_remove_holes()
#
# seki <-
#   st_read("Y:/Data-GIS/Boundaries/Owl_demography/SKC_subsites_utm83/SKC_subsites_utm83.shp") |>
#   st_transform(4326) |>
#   st_make_valid() |>
#   summarise() |>
#   transmute(name = 'Sequoia-Kings Canyon') |>
#   nngeo::st_remove_holes()
#
# snf <-
#   st_read("Y:/Data-GIS/Boundaries/Owl_demography/SNF_Demo_utm83/SNF_Demo_utm83.shp") |>
#   st_transform(4326) |>
#   summarise() |>
#   transmute(name = 'Sierra') |>
#   nngeo::st_remove_holes()
#
# demography_study_areas <-
#   bind_rows(eldorado, lassen, seki, snf)
#
# demography_study_areas |>
#   mapview::mapview()
#
# demography_study_areas |>
#   st_write(here::here('data-raw/demography_areas.shp'))

demography_study_areas <-
  st_read(
    here::here('data-raw/demography_areas.shp'),
    quiet = TRUE
  )


# save raw spatial data ---------------------------------------------------

cb_boundary_layers <-
  list(
    sierra_hexes = sierra_hexes,
    coastal_hexes = coastal_hexes,
    # epa_ecoregions = epa_ecoregions,
    usfs_boundaries = usfs_boundaries,
    nps_boundaries = nps_boundaries,
    ca_boundary = ca_boundary,
    # fire_perimeters = fire_perimeters,
    demography_study_areas = demography_study_areas,
    sierra_study_area = sierra_study_area
  )

# this updates the /data folder
usethis::use_data(cb_boundary_layers, overwrite = TRUE)


# birdnet thresholds/codes ------------------------------------------------

# species_threshold_df <- read_csv(here::here('data-raw/species_thresholds.csv'))
# usethis::use_data(species_threshold_df, overwrite = TRUE)

birdnet_species_codes <- read_csv(here::here('data-raw/species_codes.csv'))
usethis::use_data(birdnet_species_codes, overwrite = TRUE)


# forest order (by latitude) ----------------------------------------------

forests_north_south <-
  usfs_boundaries |>
  st_centroid() %>%
  mutate(lat = st_coordinates(.)[,2]) |>
  arrange(desc(lat)) |>
  pull(frst_nm)

# save
usethis::use_data(forests_north_south, overwrite = TRUE)


# owl_recording_times -----------------------------------------------------

owl_times <-
  # 8PM - 5:59 AM
  # starts at 8PM with last recording starting at 5AM
  hms::parse_hms(
    c(
      '20:00:00',
      '21:00:00',
      '22:00:00',
      '23:00:00',
      '00:00:00',
      '01:00:00',
      '02:00:00',
      '03:00:00',
      '04:00:00',
      '05:00:00'
    )
  )

# save
usethis::use_data(owl_times, overwrite = TRUE)


# minimum flac size -------------------------------------------------------

# file sizes below this are moved and later discarded
min_flac_size <- 92 # (megabytes)

# save
usethis::use_data(min_flac_size, overwrite = TRUE)


# minimum json size -------------------------------------------------------

# file sizes below this are moved and later discarded
# this seems to be a good threshold now that jsons contain embeddings
min_json_size <- 4 # (megabytes)

# save
usethis::use_data(min_json_size, overwrite = TRUE)


# buffer distance ARU and broadcast survey --------------------------------

hoot_buffer_distance <- 1500 # (meters)

# save
usethis::use_data(hoot_buffer_distance, overwrite = TRUE)


# recording times by species group ----------------------------------------

# these are start times
csow_bdow_forest_owl_hours <-
  hms::parse_hms(
    c(
      '20:00:00',
      '21:00:00',
      '22:00:00',
      '23:00:00',
      '00:00:00',
      '01:00:00',
      '02:00:00',
      '03:00:00'
    )
  )

usethis::use_data(csow_bdow_forest_owl_hours, overwrite = TRUE)

all_bird_hours <-
  hms::parse_hms(
    c(
      '04:00:00',
      '05:00:00'
    )
  )

usethis::use_data(all_bird_hours, overwrite = TRUE)

diurnal_bird_hours <-
  hms::parse_hms(
    c(
      '18:00:00',
      '19:00:00',
      '06:00:00',
      '07:00:00',
      '08:00:00'
    )
  )

usethis::use_data(diurnal_bird_hours, overwrite = TRUE)


# minimum json predictions ------------------------------------------------

# this is equivalent to a 30 minute recording with predictions for each
# 3-second chunk
min_json_predictions <- 144600

usethis::use_data(min_json_predictions, overwrite = TRUE)

