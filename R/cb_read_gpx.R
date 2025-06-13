
#' Read GPX files from Garmin GPS units
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

cb_read_gpx <- function(x) {

  sf::st_read(x, layer = "waypoints") |>
    dplyr::select(name, time, geometry)

}
