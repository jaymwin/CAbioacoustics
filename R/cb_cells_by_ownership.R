
#' Title
#'
#' @param ownership
#'
#' @return
#' @export
#'
#' @examples

cb_cells_by_ownership <- function(ownership) {

  if (ownership == 'usfs') {

    cells <-
      cb_get_spatial('hexes') |>
      filter(str_detect(ownership, 'Forest')) |>
      pull(cell_id)

  } else if (ownership == 'nps') {

    cells <-
      cb_get_spatial('hexes') |>
      filter(!str_detect(ownership, 'Forest')) |>
      pull(cell_id)

  } else if (ownership == 'any') {

    cells <-
      cb_get_spatial('hexes') |>
      pull(cell_id)

  }

  return(cells)

}
