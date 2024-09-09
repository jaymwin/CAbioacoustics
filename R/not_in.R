
#' Title
#'
#' @param x
#' @param table
#'
#' @return
#' @export
#'
#' @examples

not_in <- function(x, table) {

  !(match(x, table, nomatch = 0) > 0)

}
