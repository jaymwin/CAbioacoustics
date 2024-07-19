
# function to fill in missing survey nights (i.e., nights with no effort)
complete_survey_nights <- function(df, start_date, end_date) {

  df |>
    # complete list of dates for each cell_unit by year
    tidyr::complete(
      group_id,
      cell_id,
      unit_number,
      cell_unit,
      study_type,
      project_area,
      survey_year,
      survey_night = seq.Date(
        as.Date(stringr::str_c(max(survey_year), start_date)),
        as.Date(stringr::str_c(max(survey_year), end_date)),
        by = "days"
      ),
      # fill in no effort
      fill = list(
        survey_hours = 0,
        detection = as.numeric(NA)
      )
    )

}


# create a date sequence given start/end dates
date_sequence <- function(df) {

  seq.Date(as.Date(df$start_date), as.Date(df$end_date), by = 'day')

}


# create a data frame of dates based off of start/end years and dates
create_season_dates <- function(start_year, end_year, start_date, end_date) {

  df <-
    tibble::tibble(
      survey_year = start_year:end_year,
      start_date = as.Date(stringr::str_c(survey_year, start_date)),
      end_date = as.Date(stringr::str_c(survey_year, end_date))
    )

  df <-
    df %>%
    dplyr::group_by(survey_year) %>%
    tidyr::nest() %>%
    dplyr::mutate(survey_night = purrr::map(data, date_sequence)) %>%
    tidyr::unnest(survey_night) %>%
    dplyr::ungroup() %>%
    dplyr::select(-data)

}


get_utm_crs <- function(df) {

  zone <- unique(df$utm_zone)

  if (zone == 10) {

    crs = 26910

  } else {

    crs = 26911

  }

  return(crs)

}


hoots_same_night <- function(df, focal_date) {

  n_rows <-
    nrow(
      df |>
        # does it intersect ARU buffer?
        dplyr::filter(survey_night_date == as.Date(focal_date))
    )

  if (n_rows > 0) {

    hoots <- TRUE

  } else {

    hoots <- FALSE

  }

  return(hoots)

}


st_intersects_any <- function(x, y) {

  sf::st_intersects(x, y) %>%
    purrr::map_lgl(~length(.x) > 0)

}

# copy sd wav folders to desktop
copy_sd_wav_folders_to_desktop <- function(x, desktop_wav_path, sd_wav_folders, sd_card_path) {

  fs::dir_copy(
    path <- x,
    new_path <- stringr::str_c(desktop_wav_path, stringr::str_remove(path, sd_card_path), sep = '/'),
    overwrite = TRUE
  )

}


# convert flacs
flac_conversion <- function(desktop_wav_path, desktop_flac_path) {

  # create sox command
  command <- paste(desktop_wav_path, desktop_flac_path)

  # sox will then send this command over to the terminal for execution.
  seewave::sox(command, path2exe = "C:/Program Files (x86)/sox-14-4-2")

}


# copy desktop flacs to external hard drive
copy_to_hd <- function(desktop_flac_path, hard_drive_path, group_visit) {

  fs::dir_copy(
    path <- stringr::str_c(desktop_flac_path, group_visit, sep = '/'),
    new_path <- stringr::str_c(hard_drive_path, group_visit, sep = '/'),
    overwrite = FALSE
  )

}


# currently not in use, but an updated version of read_birdnet_json
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

