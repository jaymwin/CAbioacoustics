

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

logit_to_probability <- function(x) {

  1 / (1 + exp(-x))

}
