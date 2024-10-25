
#' Get duration of audio file (in seconds)
#'
#' @param path Path to audio file
#'
#' @return Duration of audio file in seconds
#' @export
#'
#' @examples

cb_get_audio_duration <- function(path) {

  av::av_media_info(path)$duration

}
