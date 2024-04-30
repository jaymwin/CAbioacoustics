
#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples

cb_read_birdnet_json <- function(path) {

  # read json file
  json_list <- jsonlite::fromJSON(path)

  # species labels
  species_df <-
    tibble::enframe(json_list) |>
    dplyr::filter(name == 'labels') |>
    tidyr::unnest(cols = c(value)) |>
    tidyr::unnest(cols = c(value)) |>
    dplyr::mutate(
      species_code = dplyr::row_number() - 1
    ) |>
    tidyr::separate(value, c("scientific_name", "common_name"), sep = '_') |>
    dplyr::select(-name)

  # detections and birdnet scores
  df <-
    tibble::enframe(json_list) |>
    dplyr::filter(name == 'detections') |>
    tidyr::unnest_longer(value) |>
    dplyr::rename(start_time = value_id) |>
    tidyr::unnest(cols = c(value)) |>
    tidyr::unnest(cols = c(value)) |>
    dplyr::rename(birdnet_logit = value) |>
    dplyr::select(-name) |>
    dplyr::group_by(start_time) |>
    dplyr::mutate(species_code = dplyr::row_number() - 1) |>
    dplyr::ungroup() |>
    dplyr::select(start_time, species_code, birdnet_logit) |>
    # join in species codes
    dplyr::left_join(species_df, by = 'species_code') |>
    dplyr::mutate(birdnet_prediction = round(logit_to_probability(birdnet_logit), 3)) |>
    dplyr::arrange(start_time, common_name) |>
    dplyr::select(start_time, dplyr::matches('name'), dplyr::matches('birdnet')) |>
    dplyr::mutate(start_time = as.numeric(start_time)) |>
    dplyr::arrange(start_time)

  return(df)

}
