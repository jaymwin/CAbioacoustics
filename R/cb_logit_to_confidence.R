
#' Title
#'
#' @param x
#' @param reverse
#'
#' @return
#' @export
#'
#' @examples

cb_logit_to_confidence <- function(x, reverse = FALSE) {

  if (reverse == FALSE) {

    # birdnet logit to 0-1 'confidence' score
    1 / (1 + exp(-x))

  } else if (reverse == TRUE) {

    # 0-1 confidence to birdnet logit score
    log(x / (1 - x))

  }

}
