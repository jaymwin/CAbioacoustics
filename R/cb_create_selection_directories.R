
#' Create directories to store selection tables
#'
#' @param date_time
#' @param directory
#'
#' @return
#' @export
#'
#' @examples

cb_create_selection_directories <- function(date_time, directory) {

  fs::dir_create(stringr::str_glue(here::here('code_outputs/csow_bdow_selection_tables_{date_time}/{directory}')))

}
