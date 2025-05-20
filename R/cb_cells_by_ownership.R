
#' Get a list of cell IDs depending on land ownership
#'
#' @param ownership US Forest Service (`usfs`), National Park Service (`nps`), other (`other`), or all cells (`any`)
#'
#' @return List of cell IDs
#' @export
#'
#' @examples
#' \dontrun{
#' cell_list <- cb_cells_by_ownership(c('usfs', 'nps'))
#' print(cell_list)
#' }

cb_cells_by_ownership <- function(ownership) {

  # define allowed values
  allowed_values <- c('any', 'other', 'usfs', 'nps')

  # check that all elements of ownership are valid
  if (!all(ownership %in% allowed_values)) {
    stop("Invalid ownership value. Must be one or more of: 'any', 'other', 'usfs', 'nps'")
  }

  # handle 'any' as a special case
  if (length(ownership) == 1 && ownership == 'any') {

    cells <-
      cb_get_spatial('sierra_hexes') |>
      dplyr::pull(cell_id)

  } else {

    ownership_pattern <- stringr::str_flatten(ownership, '|')

    cells <-
      cb_get_spatial('sierra_hexes') |>
      dplyr::filter(stringr::str_detect(unit_admin, ownership_pattern)) |>
      dplyr::pull(cell_id)

  }

  return(cells)

}
