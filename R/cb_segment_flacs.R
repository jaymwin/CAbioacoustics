
#' Title
#'
#' @param selections_df
#' @param output_path
#' @param sox_path
#'
#' @return
#' @export
#'
#' @examples

cb_segment_flacs <- function(selections_df, output_path, sox_path) {

  # start/end of 3-second segment (begin/end time from BirdNET selection table)
  start_time <- hms::as_hms(selections_df$begin_time)
  end_time <- hms::as_hms(selections_df$end_time)

  # now trim flac
  seewave::sox(str_glue("{selections_df$begin_file} hoot.flac trim {start_time} ={end_time}"), path2exe = sox_path)

  # play trimmed, 3-second flac
  seewave::sox("hoot.flac -t waveaudio", path2exe = sox_path)

}
