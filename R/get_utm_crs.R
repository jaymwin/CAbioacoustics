
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

get_utm_crs <- function(df) {

  zone <- unique(df$utm_zone)

  if (zone == 10) {

    crs = 26910

  } else {

    crs = 26911

  }

  return(crs)

}
