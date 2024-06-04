
#' Play a FLAC file
#'
#' @param flac_path
#'
#' @return
#' @export
#'
#' @examples

cb_play_flac <- function(flac_path) {

  seewave::sox(
    command = stringr::str_glue("{flac_path} -t waveaudio"),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

}
