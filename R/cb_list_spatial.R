
#' Get a list of spatial layers available in `CAbioacoustics`
#'
#' @return data frame of available shapefiles
#' @export
#'
#' @examples
#' \dontrun{
#' # print list of shapefiles
#' cb_list_spatial()
#' }

cb_list_spatial <- function() {

  tibble::tribble(
    ~layer_name, ~description, ~type,
    'sierra_hexes', 'hex sampling grid for Sierra Nevada region', 'shapefile',
    'coastal_hexes', 'hex sampling grid for Coastal Barred Owl region', 'shapefile',
    'usfs_boundaries', 'USFS national forest boundaries', 'shapefile',
    'nps_boundaries', 'NPS boundaries', 'shapefile',
    'ca_boundary', 'California border', 'shapefile',
    'demography_study_areas', 'CSOW demographic study areas', 'shapefile',
    'sierra_study_area', 'Sierra study area (based on hex boundary)', 'shapefile'
  ) %>%
    dplyr::arrange(type, layer_name) %>%
    print(n = Inf)

}
