
#' Title
#'
#' @param path
#' @param species_thresholds
#'
#' @return
#' @export
#'
#' @examples

cb_extract_json_predictions <- function(path, species_thresholds = species_threshold_df, date_time = date_time) {

  # get name of file first for saving things later
  file_name <- stringr::str_extract(path, 'G(P|R|C|M|[0-9])[0-9]{2}_V[0-9]{1}_C[0-9]{4}_U[0-9]{1}_[0-9]{8}_[0-9]{6}')

  # read json (shouldn't be any errors at this point)
  jsonlite::fromJSON(path) |>
    # save the detections using species-specific thresholds
    tibble::enframe() |>
    # get detections
    dplyr::filter(name == 'detections') |>
    # do some unnesting/cleaning
    tidyr::unnest_longer(value) |>
    dplyr::select(-name) |>
    dplyr::rename(relative_time = value_id) |>
    tidyr::unnest(cols = c(value)) |>
    tidyr::unnest(cols = c(value)) |>
    # save dimensions of the JSON predictions df
    purrr::walk(\(x) get_dim(x, file_name, date_time)) |>
    dplyr::group_by(relative_time) |>
    # this allows for different duration files
    dplyr::mutate(
      species_code = dplyr::row_number() - 1,
      relative_time = as.numeric(relative_time)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(relative_time, species_code, value) |>
    dplyr::inner_join(species_threshold_df, by = dplyr::join_by('species_code')) |>
    dplyr::filter(value >= threshold) |>
    dplyr::select(-threshold) |>
    # and save
    readr::write_csv(stringr::str_glue(here::here('code_outputs/post_birdnet_{date_time}/species_predictions/{file_name}_filtered.csv')))

}
