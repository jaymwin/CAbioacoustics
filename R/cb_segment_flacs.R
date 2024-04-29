
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

cb_segment_flacs <- function(begin_file, begin_time, end_time) {

  # start/end of 3-second segment (begin/end time from BirdNET selection table)
  begin <- hms::as_hms(begin_time)
  end <- hms::as_hms(end_time)

  input_flac <- str_c(here::here('raw_flacs'), begin_file, sep = '/')
  output_flac <- str_c(here::here('processed_flacs'), '/', str_remove(begin_file, '.flac'), '_', begin_time, '_', end_time, '.flac')

  # now trim flac
  seewave::sox(str_glue("{input_flac} {output_flac} trim {begin} ={end}"), path2exe = sox_path)

}
