
#' Title
#'
#' @param path
#' @param species_thresholds
#' @param date_time
#'
#' @return
#' @export
#'
#' @examples

cb_read_filter_json <- function(path, species_thresholds = species_threshold_df, date_time = date_time) {

  # get name of file first for saving things later
  file_name <- stringr::str_extract(path, 'G(P|R|C|M|[0-9])[0-9]{2}_V[0-9]{1}_C[0-9]{4}_U[0-9]{1}_[0-9]{8}_[0-9]{6}')

  # read json and find any parsing errors
  possible_json_error <-
    tryCatch(
      # Get values from json.gz file
      json_list <- jsonlite::fromJSON(path),
      error = function(e) e
    )

  if (inherits(possible_json_error, "error")) {

    # if there is an error, save it to log text file
    tibble::tibble(
      bad_json = file_name,
      error = 'json_parsing'
    ) |>
      write.table(
        stringr::str_glue(here::here('code_outputs/post_birdnet_{date_time}/bad_json_files.txt')),
        col.names = FALSE,
        row.names = FALSE,
        append = TRUE
      )

  } else if (length(json_list$detections) == 0) {

    # some can parse but have no detections
    tibble::tibble(
      bad_json = file_name,
      error = 'no_detections'
    ) |>
      write.table(
        stringr::str_glue(here::here('code_outputs/post_birdnet_{date_time}/bad_json_files.txt')),
        col.names = FALSE,
        row.names = FALSE,
        append = TRUE
        )

  } else {

    # otherwise save the detections using species-specific thresholds
    df_json <-
      json_list |>
      tibble::enframe() |>
      dplyr::filter(name == 'detections')

    df <-
      df_json |>
      tidyr::unnest_longer(value) |>
      dplyr::select(-name) |>
      dplyr::rename(relative_time = value_id) |>
      tidyr::unnest(cols = c(value)) |>
      tidyr::unnest(cols = c(value)) |>
      dplyr::group_by(start_time) |>
      dplyr::mutate(
        species_code = dplyr::row_number() - 1,
        relative_time = as.numeric(relative_time)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(relative_time, species_code, value) |>
      dplyr::inner_join(species_threshold_df, by = dplyr::join_by('species_code')) |>
      dplyr::filter(value >= threshold) |>
      dplyr::select(-threshold)

    df |>
      readr::write_csv(stringr::str_glue(here::here('code_outputs/post_birdnet_{date_time}/species_predictions/{file_name}_filtered.csv')))

  }

}
