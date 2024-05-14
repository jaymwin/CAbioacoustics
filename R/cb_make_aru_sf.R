
#' Make a data frame containing ARUs and UTMs into a simple feature object
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # example ARU data frame
#' aru_df <-
#' tibble::tribble(
#'   ~cell_id, ~utm_zone, ~utme, ~utmn,
#'   'C0001', 10, 670282, 4390557,
#'   'C0001', 10, 670972, 4390553,
#'   'C0001', 10, 669829, 4390388
#' )
#' # convert to simple feature point geometry
#' aru_df |>
#' dplyr::group_split(utm_zone) |>
#'   purrr::map_dfr(cb_make_aru_sf)
#' }

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
