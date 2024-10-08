
#' Get path info for JSONs on research drive
#'
#' @param path
#' @param year
#'
#' @return
#' @export
#'
#' @examples

cb_get_json_paths <- function(path, year) {

  df <-
    fs::dir_ls(
      path = path,
      recurse = 2,
      regex = as.character(year)
    ) |>
    tibble::as_tibble() |>
    dplyr::rowwise() |>
    dplyr::mutate(depth = length(strsplit(value, "/")[[1]])) |>
    dplyr::ungroup() |>
    dplyr::filter(depth == max(depth)) |>
    dplyr::select(-depth)

  subfolder_contents <-
    df |>
    dplyr::group_split(value) |>
    purrr::map(get_subdirectory_contents) |>
    purrr::list_c()

  raw_file_info_df <-
    subfolder_contents |>
    furrr::future_map(get_file_info) |>
    dplyr::bind_rows()

  return(raw_file_info_df)

}
