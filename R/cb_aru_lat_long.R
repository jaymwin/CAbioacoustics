
#' Extract lat/long coordinates from an ARU sf point object
#'
#' @param arus A sf point object of ARU deployments in WGS 84 coordinate system
#'
#' @return
#' @export
#'
#' @examples

cb_aru_lat_long <- function(arus) {

  arus |>
    dplyr::mutate(
      lon = sf::st_coordinates(arus)[,1],
      lat = sf::st_coordinates(arus)[,2]
    )

}

