
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

logit_to_probability <- function(x, reverse = FALSE) {

  if (reverse == FALSE) {

    # birdnet logit to 0-1 probability
    1 / (1 + exp(-x))

  } else if (reverse == TRUE) {

    # 0-1 probability to birdnet logit
    log(x /(1 - x))

  }

}
