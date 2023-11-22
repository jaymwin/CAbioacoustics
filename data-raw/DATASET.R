## code to prepare `DATASET` dataset goes here


library(arcpullr)
library(tidyverse)
library(sf)
library(janitor)
library(terra)


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
  rename(park_name = parkname)


# usfs boundaries ---------------------------------------------------------

usfs_boundaries <-
  arcpullr::get_spatial_layer(
    url = 'https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_ForestSystemBoundaries_01/MapServer/0',
    where = "Region = '05'"
  ) |>
  select(ADMINFORESTID:FORESTNAME) |>
  clean_names() |>
  rename(admin_forest_id = adminforestid, forest_number = forestnumber, forest_org_code = forestorgcode, forest_name = forestname) |>
  st_make_valid()


# epa ecoregions ----------------------------------------------------------

epa_ecoregions <-
  arcpullr::get_spatial_layer(
    url = 'https://edits.nationalmap.gov/arcgis/rest/services/landforms/landforms_epa_eco_regions/MapServer/0',
    where = "US_L3NAME IN ('Sierra Nevada', 'Cascades')"
  ) |>
  select(NA_L3NAME) |>
  clean_names()


# hexes -------------------------------------------------------------------

hexes <-
  st_read(
    here::here('data-raw/hexes_elev_xy.shp'),
    quiet = TRUE
  )


# fire perimeters ---------------------------------------------------------

# 2020-onward
fire_perimeters <-
  arcpullr::get_spatial_layer(
    url = 'https://services1.arcgis.com/jUJYIo9tSA7EHvfZ/ArcGIS/rest/services/California_Fire_Perimeters/FeatureServer/2',
    where = "YEAR_ >= 2020",
    out_fields = c('YEAR_', 'FIRE_NAME', 'CAUSE', 'C_METHOD')
  ) |>
  clean_names() |>
  st_make_valid()


# save raw spatial data ---------------------------------------------------

cb_boundary_layers <-
  list(
    hexes = hexes,
    epa_ecoregions = epa_ecoregions,
    usfs_boundaries = usfs_boundaries,
    nps_boundaries = nps_boundaries,
    ca_boundary = ca_boundary,
    fire_perimeters = fire_perimeters
  )

# this updates the /data folder
usethis::use_data(cb_boundary_layers, overwrite = TRUE)
