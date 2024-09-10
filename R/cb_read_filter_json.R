
#' Title
#'
#' @param path
#' @param species_thresholds
#'
#' @return
#' @export
#'
#' @examples

cb_read_filter_json <- function(path, species_thresholds = species_threshold_df) {

  # read json and find any parsing errors
  possibleError <-
    tryCatch(
      # Get values from json.gz file
      json_list <- jsonlite::fromJSON(path),
      error = function(e) e
    )

  if (inherits(possibleError, "error")) {

    # if there is an error, save it to log text file
    tibble::tibble(
      bad_json = stringr::str_extract(path, 'G(P|R|C|M|[0-9])[0-9]{2}_V[0-9]{1}_C[0-9]{4}_U[0-9]{1}_[0-9]{8}_[0-9]{6}'),
      error = 'json_parsing'
    ) |>
      write.table(
        here::here('bad_json_files.txt'),
        col.names = FALSE,
        row.names = FALSE,
        append = TRUE
      )

  } else if (length(json_list$detections) == 0) {

    # some can parse but have no detections
    tibble::tibble(
      bad_json = stringr::str_extract(path, 'G(P|R|C|M|[0-9])[0-9]{2}_V[0-9]{1}_C[0-9]{4}_U[0-9]{1}_[0-9]{8}_[0-9]{6}'),
      error = 'no_detections'
    ) |>
      write.table(
        here::here('bad_json_files.txt'),
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

    file_name <- stringr::str_extract(path, 'G(P|R|C|M|[0-9])[0-9]{2}_V[0-9]{1}_C[0-9]{4}_U[0-9]{1}_[0-9]{8}_[0-9]{6}')

    df |>
      readr::write_csv(stringr::str_glue(here::here('compact_csvs/{file_name}_threshold_filtered.csv')))

  }

}
