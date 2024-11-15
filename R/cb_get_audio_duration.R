
#' Get duration of audio file (in minutes)
#'
#' @param path Path to audio file
#'
#' @return Duration of audio file in minutes
#' @export
#'
#' @examples

cb_get_audio_duration <- function(path) {

  tibble::tibble(
    path = path,
    duration = round((av::av_media_info(path)$duration) / 60, 1)
  )

}
