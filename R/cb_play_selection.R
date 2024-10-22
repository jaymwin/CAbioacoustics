
#' Play a BirdNET selection from a FLAC file
#'
#' @param flac_path Path to FLAC file
#' @param begin_time When to start playing FLAC
#' @param end_time When to stop playing FLAC
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # play seconds 3-6
#' cb_play_flac(
#'  "Z:/Acoustic_Data/ARU_Data_Raw/ARU_Sierra_Monitoring/2024/North/G018_V1/G018_V1_C0263_U3/G018_V1_C0263_U3_20240616/G018_V1_C0263_U3_20240616_060104Z.flac",
#'  3,
#'  6
#' )
#' }

cb_play_flac <- function(flac_path, begin_time, end_time) {

  seewave::sox(
    command = stringr::str_glue("{flac_path} -t waveaudio trim {hms::as_hms(begin_time)} ={hms::as_hms(end_time)}"),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

}
