
#' Play a FLAC file
#'
#' @param flac_path
#'
#' @return
#' @export
#'
#' @examples

cb_play_flac <- function(flac_path, begin_time, end_time) {

  seewave::sox(
    command = stringr::str_glue("{flac_path} -t waveaudio trim {hms::as_hms(begin_time)} ={hms::as_hms(end_time)}"),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

}
