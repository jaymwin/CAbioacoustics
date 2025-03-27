
#' Read ARU deployment/recovery data from ArcGIS Field Maps
#'
#' @param feature_layer HTML to hosted Field Maps feature layer
#'
#' @return
#' @export
#'
#' @examples

cb_read_field_maps <- function(feature_layer) {

  # make sure there's arcgis pro on your computer and it connects
  print(arcgisbinding::arc.check_product())

  # read now as sf object
  arcgisbinding::arc.data2sf(
    arcgisbinding::arc.select(
      arcgisbinding::arc.open(
        feature_layer
      )
    )
  ) |>
    janitor::clean_names()

}
