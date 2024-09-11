
#' Title
#'
#' @param path
#' @param date_time
#'
#' @return
#' @export
#'
#' @examples

cb_check_bad_json <- function(path, date_time = date_time) {

  # read json and find any parsing errors
  possible_error <-
    tryCatch(
      # Get values from json.gz file
      json_list <- jsonlite::fromJSON(path),
      error = function(e) e
    )

  if (inherits(possible_error, "error")) {

    # if there is a parsing error, save it to log text file
    tibble::tibble(
      bad_json = path,
      error = 'json_parsing'
    ) |>
      write.table(
        stringr::str_glue(here::here('code_outputs/post_birdnet_{date_time}/bad_json_files.txt')),
        col.names = FALSE,
        row.names = FALSE,
        append = TRUE
      )

  } else if (length(json_list$detections) == 0) {

    # some can parse but have no detections
    tibble::tibble(
      bad_json = path,
      error = 'no_detections'
    ) |>
      write.table(
        stringr::str_glue(here::here('code_outputs/post_birdnet_{date_time}/bad_json_files.txt')),
        col.names = FALSE,
        row.names = FALSE,
        append = TRUE
      )

  }

}
