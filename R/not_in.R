
#' Title
#'
#' @param x
#' @param table
#'
#' @return
#' @export
#'
#' @examples

"%nin%" <- function(x, table) {

  !(match(x, table, nomatch = 0) > 0)

}
