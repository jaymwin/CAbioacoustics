
#' Convert BirdNET logit scores to confidence scores (or do the opposite)
#'
#' @param x Either BirdNET logit or confidence score
#' @param reverse Go from logit to confidence (reverse == TRUE) or confidence to logit (reverse == FALSE)
#'
#' @return BirdNET logit or confidence score
#' @export
#'
#' @examples
#' \dontrun{
#' # convert from BirdNET logit to confidence score
#' birdnet_logit_score <- 1.49850
#'
#' birdnet_confidence_score <- cb_logit_to_confidence(birdnet_logit_score, reverse = FALSE)
#'
#' # convert from BirdNET confidence to logit score
#' cb_logit_to_confidence(birdnet_confidence_score, reverse = TRUE)
#' }

cb_logit_to_confidence <- function(x, reverse = FALSE) {

  if (reverse == FALSE) {

    # birdnet logit to 0-1 'confidence' score
    1 / (1 + exp(-x))

  } else if (reverse == TRUE) {

    # 0-1 confidence to birdnet logit score
    log(x / (1 - x))

  }

}
