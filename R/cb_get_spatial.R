
#' Retrieve a project vector layer
#'
#' @param layer_name Shapefile name
#'
#' @return `sf` polygon object
#' @export
#'
#' @examples
#' \dontrun{
#' # retrieve layer of Sierra Nevada hex cells
#' hexes_sf <- cb_get_spatial('hexes')
#' }

cb_get_spatial <- function(layer_name = NULL) {

  if (layer_name == 'ca_boundary') {

    layer <- cb_boundary_layers$ca_boundary

  } else if (layer_name == 'hexes') {

    layer <- cb_boundary_layers$hexes

  } else if (layer_name == 'nps_boundaries') {

    layer <- cb_boundary_layers$nps_boundaries

  } else if (layer_name == 'usfs_boundaries') {

    layer <- cb_boundary_layers$usfs_boundaries

  # } else if (layer_name == 'fire_perimeters') {
  #
  #   layer <- cb_boundary_layers$fire_perimeters

  } else if (layer_name == 'demography_study_areas') {

    layers <- cb_boundary_layers$demography_study_areas

  } else if (layer_name == 'sierra_study_area') {

    layers <- cb_boundary_layers$sierra_study_area

  }

}
