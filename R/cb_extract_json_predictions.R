
#' Extract predictions from JSON files using species-specific threshold values
#'
#' @param path
#' @param species_thresholds
#' @param date_time
#'
#' @return
#' @export
#'
#' @examples

cb_extract_json_predictions <- function(path, species_thresholds = species_threshold_df, date_time = date_time) {

  # get name of file first for saving things later
  file_name <- stringr::str_extract(path, 'G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}_[0-9]{8}_[0-9]{6}')
  file_date_time <- lubridate::ymd_hms(stringr::str_extract(file_name, '[0-9]{8}_[0-9]{6}'))
  rounded_file_start_time <- hms::as_hms(lubridate::round_date(file_date_time, 'hour'))

  # read json (shouldn't be any errors at this point)
  json_df <-
    jsonlite::fromJSON(path) |>
    # turn into data frame where every row is a list
    tibble::enframe() |>
    # get detections part
    dplyr::filter(name == 'detections') |>
    # do some unnesting/cleaning
    tidyr::unnest_longer(value) |>
    dplyr::select(-name) |>
    dplyr::rename(relative_time = value_id) |>
    tidyr::unnest(cols = c(value)) |>
    tidyr::unnest(cols = c(value))

  # some can parse but have no detections
  tibble::tibble(
    path = basename(file_name),
    n_predictions = dim(json_df)[1]
  ) |>
    write.table(
      stringr::str_glue(here::here('code_outputs/post_birdnet_{date_time}/prediction_count_jsons.txt')),
      col.names = FALSE,
      row.names = FALSE,
      append = TRUE
    )

  if (rounded_file_start_time %in% csow_bdow_forest_owl_hours) {

    # owl outputs only
    json_df <-
      json_df |>
      dplyr::group_by(relative_time) |>
      # this allows for different duration files
      dplyr::mutate(
        species_code = dplyr::row_number() - 1,
        relative_time = as.numeric(relative_time)
      ) |>
      dplyr::ungroup() |>
      dplyr::inner_join(
        species_threshold_df |> dplyr::filter(species_type %in% c('forest_owl', 'csow_bdow')),
        by = dplyr::join_by('species_code')
      ) |>
      dplyr::filter(value >= logit_threshold)

    if (dim(json_df)[1] > 0) {

    json_df <-
      json_df |>
      dplyr::group_by(relative_time, scientific_name) |>
      dplyr::filter(species_code == max(species_code)) |>
      dplyr::ungroup()

    }

  } else if (rounded_file_start_time %in% all_bird_hours) {

    # all outputs
    json_df <-
      json_df |>
      dplyr::group_by(relative_time) |>
      # this allows for different duration files
      dplyr::mutate(
        species_code = dplyr::row_number() - 1,
        relative_time = as.numeric(relative_time)
      ) |>
      dplyr::ungroup() |>
      dplyr::inner_join(
        species_threshold_df,
        by = dplyr::join_by('species_code')
      ) |>
      dplyr::filter(value >= logit_threshold)

    if (dim(json_df)[1] > 0) {

      json_df <-
        json_df |>
        dplyr::group_by(relative_time, scientific_name) |>
        dplyr::filter(species_code == max(species_code)) |>
        dplyr::ungroup()

    }

  } else if (rounded_file_start_time %in% diurnal_bird_hours) {

    # diurnal outputs only
    json_df <-
      json_df |>
      dplyr::group_by(relative_time) |>
      # this allows for different duration files
      dplyr::mutate(
        species_code = dplyr::row_number() - 1,
        relative_time = as.numeric(relative_time)
      ) |>
      dplyr::ungroup() |>
      dplyr::inner_join(
        species_threshold_df |> dplyr::filter(species_type == 'diurnal'),
        by = dplyr::join_by('species_code')
      ) |>
      dplyr::filter(value >= logit_threshold)

  }

  if (dim(json_df)[1] > 0) {

    json_df |>
      dplyr::select(-logit_threshold) |>
      dplyr::mutate(json = file_name) |>
      dplyr::select(json, relative_time, species_code, birdnet_logit = value) |>
      # and save
      readr::write_csv(stringr::str_glue(here::here('code_outputs/post_birdnet_{date_time}/species_predictions/{file_name}_filtered.csv')))

  }

}
