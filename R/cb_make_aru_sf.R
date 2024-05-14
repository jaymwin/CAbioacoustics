
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

cb_make_aru_sf <- function(df) {

  # get the utm zone
  zone <- unique(df$utm_zone)

  if (zone == 10) {

    crs = 26910

  } else {

    crs = 26911

  }

  # convert to sf and WGS 84
  df <-
    df |>
    sf::st_as_sf(coords = c('utme', 'utmn'), crs = crs) |>
    sf::st_transform(4326) |>
    dplyr::select(-utm_zone)

  return(df)

}
