
#' Create directories to store selection tables
#'
#' @param date_time
#' @param directory
#'
#' @return
#' @export
#'
#' @examples

cb_create_selection_directories <- function(date_time, directory, species_group) {

  if (species_group != 'csow_bdow' & species_group != 'forest_owl') stop('must be `csow_bdow` or `forest_owl`')

  if (species_group == 'csow_bdow') {

    fs::dir_create(stringr::str_glue(here::here('code_outputs/csow_bdow_selection_tables_{date_time}/{directory}')))

  } else if (species_group == 'forest_owl')

    fs::dir_create(stringr::str_glue(here::here('code_outputs/forest_owl_selection_tables_{date_time}/{directory}')))

}

