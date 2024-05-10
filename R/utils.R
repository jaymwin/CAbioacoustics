
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
copy_sd_wav_folders_to_desktop <- function(x) {

  fs::dir_copy(
    path <- x,
    new_path <- stringr::str_c(desktop_wav_path, stringr::str_remove(sd_wav_folders, sd_card_path), sep = '/'),
    overwrite = TRUE
  )

}


# convert flacs
flac_conversion <- function(desktop_wav_path, desktop_flac_path) {

  # create sox command
  command <- paste(desktop_wav_path, desktop_flac_path)

  # sox will then send this command over to the terminal for execution.
  seewave::sox(command, path2exe = sox_path)

}


# copy desktop flacs to external hard drive
copy_to_hd <- function(desktop_flac_path, hard_drive_path, group_visit) {

  fs::dir_copy(
    path <- stringr::str_c(desktop_flac_path, group_visit, sep = '/'),
    new_path <- stringr::str_c(hard_drive_path, group_visit, sep = '/'),
    overwrite = FALSE
  )

}
