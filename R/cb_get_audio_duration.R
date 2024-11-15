
#' Get duration of audio file (in minutes)
#'
#' @param path Path to audio file
#'
#' @return Duration of audio file in minutes
#' @export
#'
#' @examples
#' \dontrun{
#' flac_duration_df <-
#'   flac_df |>
#'     # list of flac paths
#'     pull(path) |>
#'     # run in parallel
#'     future_map_dfr(get_duration)
#'     }

cb_get_audio_duration <- function(path) {

  tibble::tibble(
    path = path,
    duration = round((av::av_media_info(path)$duration) / 60, 1)
  )

}
