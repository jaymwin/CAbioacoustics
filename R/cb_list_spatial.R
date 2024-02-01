
#' Title
#'
#' @return
#' @export
#'
#' @examples

cb_list_spatial <- function() {

  tibble::tribble(
    ~layer_name, ~description, ~type,
    'hexes', 'hex sampling grid for Sierra Nevada region', 'shapefile',
    'epa_ecoregions', 'EPA ecoregions (level III)', 'shapefile',
    'usfs_boundaries', 'USFS national forest boundaries', 'shapefile',
    'nps_boundaries', 'NPS boundaries', 'shapefile',
    'ca_boundary', 'California border', 'shapefile',
    # 'fire_perimeters', 'Recent (2020-onward) fire perimeters', 'shapefile',
    'demography_study_areas', 'CSOW demographic study areas', 'shapefile',
    'sierra_study_area', 'Sierra study area (based on hex boundary)', 'shapefile'
  ) %>%
    dplyr::arrange(type, layer_name) %>%
    print(n = Inf)

}
