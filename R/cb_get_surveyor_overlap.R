
#' Identify BirdNET selections that overlapped with CSOW broadcast surveys
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

cb_get_surveyor_overlap <- function(df, year, previous_date_time, new_date_time) {

  # get birdnet file path and filename (XXXX.flac)
  focal_birdnet_path <- unique(df$filename)

  # create data frame with deployment name and survey night for each birdnet file
  birdnet_selection_df <-
    focal_birdnet_path |>
    tibble::as_tibble() |>
    dplyr::rename(begin_file = value) |>
    dplyr::mutate(
      # pull out date (yyyymmdd)
      begin_file_date = stringr::str_extract(focal_birdnet_path, '(?<!\\d)[\\d]{8}(?!\\d)'),
      # pull out time (hhmmss)
      begin_file_time = stringr::str_extract(focal_birdnet_path, '(?<!\\d)[\\d]{6}(?!\\d)'),
      # create a date (yyyy-mm-dd hh:mm:ss)
      aru_date_time = lubridate::ymd_hms(stringr::str_c(begin_file_date, begin_file_time), tz = "US/Pacific"),
      # convert date to survey night depending on time
      aru_survey_night =
        dplyr::case_when(
          lubridate::hour(hms::as_hms(aru_date_time)) >= 0 ~ as.Date(aru_date_time) - 1,
          TRUE ~ as.Date(aru_date_time)
        )
    ) |>
    # just need deployment name and date for each birdnet file
    dplyr::transmute(
      deployment_name = stringr::str_extract(begin_file, 'G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}'),
      aru_survey_night = aru_survey_night
    )

  # pull out deployment name separately (for querying database)
  focal_deployment <- birdnet_selection_df$deployment_name

  # create focal date
  focal_date <- birdnet_selection_df$aru_survey_night

  # now connect to the database
  cb_connect_db()

  # get coordinates and convert to ARU for unit matching deployment name
  focal_deployment_df <-
    conn |>
    dplyr::tbl('acoustic_field_visits') |>
    dplyr::select(deployment_name, survey_year, dplyr::matches('utm')) |>
    dplyr::filter(deployment_name == focal_deployment & survey_year == year) |>
    dplyr::collect()

  # close database connection
  cb_disconnect_db()

  # convert ARU deployment to sf with utms, utm zone
  # and buffer by 1.5 km to determine if there's surveyor overlap
  focal_deployment_sf <-
    focal_deployment_df |>
    dplyr::group_split(utm_zone) |>
    purrr::map_dfr(cb_make_aru_sf) |>
    sf::st_transform(3310) |>
    sf::st_buffer(dist = 1500)

  # now determine if there are any surveyor hoots on the same night as the birdnet detections
  hoots <- CAbioacoustics:::hoots_same_night(human_hoots_sf, focal_date)

  # if there are, then see if any surveyor hoot locations intersect the ARU buffer;
  # otherwise set 'intersections' to FALSE
  if (hoots == TRUE) {

    focal_human_hoots_sf <-
      human_hoots_sf %>%
      # does it intersect ARU buffer?
      dplyr::filter(survey_night_date == as.Date(birdnet_selection_df$aru_survey_night))

    intersections <-
      human_hoots_sf %>%
      # does it intersect ARU buffer?
      dplyr::filter(survey_night_date == as.Date(birdnet_selection_df$aru_survey_night)) %>%
      dplyr::mutate(int = CAbioacoustics:::st_intersects_any(., focal_deployment_sf |> sf::st_transform(4326))) |>
      dplyr::filter(int == TRUE) |>
      dplyr::distinct(int) |>
      dplyr::pull()

  } else {

    intersections <- FALSE

  }

  # and now if there are surveyor hoots that overlap with the ARU buffer, set all of the
  # detections to 'surveyed' in overwrite column and save using that birdnet file's original path;
  # otherwise change overwrite to 'no' and save
  if (isTRUE(intersections) == TRUE) {

    focal_birdnet_df |>
      dplyr::select(-filename) |>
      dplyr::mutate(Overwrite = 'surveyed') |>
      dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~tidyr::replace_na(., ""))) |>
      write.table(
        file = stringr::str_replace(focal_birdnet_path, previous_date_time, new_date_time),
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )

  } else {

    focal_birdnet_df |>
      dplyr::select(-filename) |>
      dplyr::mutate(Overwrite = 'no') |>
      dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~tidyr::replace_na(., ""))) |>
      write.table(
        file = stringr::str_replace(focal_birdnet_path, previous_date_time, new_date_time),
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )

  }

}
