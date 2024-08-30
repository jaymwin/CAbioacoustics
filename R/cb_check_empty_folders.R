
#' Check for folders missing acoustic data on research drive
#'
#' @param path
#' @param year
#'
#' @return
#' @export
#'
#' @examples

cb_check_empty_folders <- function(path, year) {

  # get monitoring/project subfolders for a particular year
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

  # get group_visit folders within a region folder
  folders_0 <-
    df |>
    dplyr::pull(value) |>
    furrr::future_map_dfr(get_files_next_depth) |>
    dplyr::rename(region = upper_level) |>
    dplyr::select(region, group_visit = value)

  # get group_visit_cell_unit folders within a group_visit folder
  folders_1 <-
    folders_0 |>
    dplyr::drop_na() |>
    dplyr::pull(group_visit) |>
    furrr::future_map_dfr(get_files_next_depth) |>
    dplyr::rename(group_visit = upper_level) |>
    dplyr::select(group_visit, group_visit_cell_unit = value)

  # get group_visit_cell_unit_date folders within a group_visit_cell_unit folder
  folders_2 <-
    folders_1 |>
    dplyr::drop_na() |>
    dplyr::pull(group_visit_cell_unit) |>
    furrr::future_map_dfr(get_files_next_depth) |>
    dplyr::mutate(group_visit_cell_unit = upper_level) |>
    dplyr::select(group_visit_cell_unit, group_visit_cell_unit_date = value)

  # get flacs within a group_visit_cell_unit_date folder
  folders_3 <-
    folders_2 |>
    dplyr::drop_na() |>
    dplyr::pull(group_visit_cell_unit_date) |>
    furrr::future_map_dfr(get_files_next_depth) |>
    dplyr::mutate(group_visit_cell_unit_date = upper_level) |>
    dplyr::select(group_visit_cell_unit_date, group_visit_cell_unit_date_hour = value)

  # combine all empty folders from above
  empty_df <-
    folders_0 |> dplyr::filter(is.na(group_visit)) |> dplyr::select(folder = region) |>
    dplyr::bind_rows(folders_1 |> dplyr::filter(is.na(group_visit_cell_unit)) |> dplyr::select(folder = group_visit)) |>
    dplyr::bind_rows(folders_2 |> dplyr::filter(is.na(group_visit_cell_unit_date)) |> dplyr::select(folder = group_visit_cell_unit)) |>
    dplyr::bind_rows(folders_3 |> dplyr::filter(is.na(group_visit_cell_unit_date_hour)) |> dplyr::select(folder = group_visit_cell_unit_date)) |>
    dplyr::rename(empty_folder = folder)

  return(empty_df)

}
