
#' Get a list of cell IDs depending on land ownership
#'
#' @param ownership US Forest Service (`usfs`), National Park Service (`nps`), or all cells (`any`)
#'
#' @return List of cell IDs
#' @export
#'
#' @examples
#' \dontrun{
#' cell_list <- cb_cells_by_ownership('usfs')
#' }

cb_cells_by_ownership <- function(ownership) {

  if (ownership == 'usfs') {

    cells <-
      cb_get_spatial('hexes') |>
      dplyr::filter(stringr::str_detect(ownership, 'Forest')) |>
      dplyr::pull(cell_id)

  } else if (ownership == 'nps') {

    cells <-
      cb_get_spatial('hexes') |>
      dplyr::filter(!stringr::str_detect(ownership, 'Forest')) |>
      dplyr::pull(cell_id)

  } else if (ownership == 'any') {

    cells <-
      cb_get_spatial('hexes') |>
      dplyr::pull(cell_id)

  }

  return(cells)

}
