
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples

st_intersects_any <- function(x, y) {

  sf::st_intersects(x, y) %>%
    purrr::map_lgl(~length(.x) > 0)

}
