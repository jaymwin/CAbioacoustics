
#' Title
#'
#' @param x
#' @param table
#'
#' @return
#' @export
#'
#' @examples

"%ni%" <- function(x, table) {

  !(match(x, table, nomatch = 0) > 0)

}
