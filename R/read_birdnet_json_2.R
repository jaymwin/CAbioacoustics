
#' Title
#'
#' @param json_gz_path
#' @param species_threshold_df
#' @param p_true_positive
#'
#' @return
#' @export
#'
#' @examples

read_birdnet_json_2 <- function(json_gz_path, species_threshold_df, p_true_positive) {

  if (p_true_positive %ni% c(0.85, 0.9, 0.95, 0.975, 0.99)) stop("True positive probability must be either 0.85, 0.90, 0.95, 0.975, or 0.99")

  json_list <- jsonlite::fromJSON(json_gz_path)

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
    dplyr::mutate(
      species_code = dplyr::row_number() - 1,
      start_time = as.integer(start_time),
      birdnet_confidence = logit_to_probability(birdnet_logit)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(start_time, species_code, birdnet_logit, birdnet_confidence) |>
    # if species, probability not null then filter by those
    dplyr::inner_join(species_threshold_df |> filter(p_tp == p_true_positive), by = join_by('species_code')) |>
    dplyr::filter(birdnet_logit >= logit_threshold) |>
    dplyr::arrange(start_time, species) |>
    dplyr::select(start_time, dplyr::matches('species'), birdnet_logit, birdnet_confidence, p_tp) |>
    dplyr::arrange(start_time, species_code)

}
