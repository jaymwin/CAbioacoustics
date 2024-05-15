
#' Segment a flac file using the begin/end time of a BirdNET selection
#'
#' @param begin_file Name of flac file
#' @param begin_time Time from start of recording and beginning of a selection
#' @param end_time Time from start of recording and end of selection
#'
#' @return
#' @export
#'
#' @examples

cb_segment_flac <- function(begin_file, begin_time, end_time) {

  # start/end of 3-second segment (begin/end time from BirdNET selection table)
  begin <- hms::as_hms(begin_time)
  end <- hms::as_hms(end_time)

  input_flac <- stringr::str_c(here::here('raw_flacs'), begin_file, sep = '/')
  # name output by begin file name with begin/end times tacked on the end
  output_flac <- stringr::str_c(here::here('processed_flacs'), '/', stringr::str_remove(begin_file, '.flac'), '_', begin_time, '_', end_time, '.flac')

  # now trim flac
  seewave::sox(stringr::str_glue("{input_flac} {output_flac} trim {begin} ={end}"), path2exe = "C:/Program Files (x86)/sox-14-4-2")

}
