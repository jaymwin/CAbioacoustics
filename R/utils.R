
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


# get crs for utm zone
get_utm_crs <- function(df) {

  zone <- unique(df$utm_zone)

  if (zone == 10) {

    crs = 26910

  } else {

    crs = 26911

  }

  return(crs)

}


# hoots on same night as ARU data
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


# find polygons intersecting another polygon
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
  command <-
    paste(
      desktop_wav_path,
      desktop_flac_path
    )

  # sox will then send this command over to the terminal for execution.
  seewave::sox(command, path2exe = "C:/Program Files (x86)/sox-14-4-2")

}


# copy desktop flacs to external hard drive
copy_to_hd <- function(desktop_flac_path, hard_drive_path, group_visit) {

  fs::dir_copy(
    path <-
      stringr::str_c(
        desktop_flac_path,
        group_visit,
        sep = '/'
      ),
    new_path <-
      stringr::str_c(
        hard_drive_path,
        # group_visit,
        sep = '/'
      ),
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


# get file size
get_file_info <- function(path) {

  fs::file_info(
    fs::dir_ls(
      path
    )
  ) |>
    dplyr::select(path, size) |>
    dplyr::mutate(size = as.numeric(size))

}


# get all daily subfolders
get_subdirectory_contents <- function(df) {

  subfolders <-
    fs::dir_ls(
      path = df$value,
      recurse = 2
    )

  subfolder_length <- max(stringr::str_length(subfolders))

  daily_subfolders <-
    subfolders |>
    tibble::as_tibble() |>
    dplyr::filter(stringr::str_length(value) == subfolder_length) |>
    dplyr::pull(value)

  return(daily_subfolders)

}


# get folders/files for a directory
get_files_next_depth <- function(path) {

  df <-
    fs::dir_ls(
      path,
      recurse = 0
    ) |>
    tibble::as_tibble() |>
    dplyr::mutate(upper_level = path)

  if (dim(df)[1] == 0) {

    df <-
      tibble::tibble(
        value = NA,
        upper_level = path
      )

  }

  return(df)

}


# move flacs to delete later (data proofing)
move_flacs_delete <- function(x) {

  fs::file_move(
    path = x,
    new_path = stringr::str_glue(here::here('flacs_to_delete/{basename(x)}'))
  )

}


# move jsons to delete later (data proofing)
move_jsons_delete <- function(x) {

  fs::file_move(
    path = x,
    new_path = stringr::str_glue(here::here('jsons_to_delete/{basename(x)}'))
  )

}


# write pipeline files in digestible batches for the dtabase website
write_pipeline_batches <- function(df, date_time) {

  region <- unique(df$region)
  study <- unique(df$study_type)
  csv_num <- unique(df$csv_num)

  df |>
    dplyr::select(file_name) |>
    write.table(
      stringr::str_glue(here::here('code_outputs/effort_pipelines_{date_time}/{region}_{study}_{csv_num}_pipeline.csv')),
      row.names = FALSE,
      col.names = FALSE
    )

}


# delete a record from the database
delete_record <- function(table_name, record_id, con) {

  delete_query <- glue::glue_sql("DELETE FROM {table_name} WHERE (id = {record_id});", .con = con)
  print(delete_query)
  DBI::dbExecute(conn = con, statement = delete_query)

}


# go from WGS84 to utm zone-specific crs
wgs84_to_utm <- function(df) {

  # get the crs depending on utm zone
  zone <- unique(df$utm_zone)

  if (zone == 10) {

    aru_crs = 26910

  } else {

    aru_crs = 26911

  }

  # convert to utm zone-specific crs
  df <-
    df |>
    sf::st_transform(aru_crs)

  return(df)

}


# shiny app functions -----------------------------------------------------

get_deployment_info <- function(sd_card_path, year) {

  # wav count
  n_wavs <- length(fs::dir_ls(sd_card_path, recurse = TRUE, glob = '*.wav'))

  # recording dates
  min_date <-
    fs::dir_ls(sd_card_path, recurse = TRUE, glob = '*.wav') |>
    stringr::str_extract('[0-9]{8}') |>
    lubridate::ymd() |>
    min()

  max_date <-
    fs::dir_ls(sd_card_path, recurse = TRUE, glob = '*.wav') |>
    stringr::str_extract('[0-9]{8}') |>
    lubridate::ymd() |>
    max()

  # deployment_name
  swift <-
    fs::dir_ls(sd_card_path) |>
    stringr::str_subset('S[0-9]{4}') |>
    head(1) |>
    stringr::str_extract('S[0-9]{4}')

  # current_year <- lubridate::year(lubridate::today())

  cb_connect_db()
  focal_deployment <-
    conn |>
    dplyr::tbl('acoustic_field_visits') |>
    dplyr::filter(swift_id == swift & survey_year == year) |>
    dplyr::collect() |>
    dplyr::pull(deployment_name)
  cb_disconnect_db()

  tibble::tibble(
    n_wavs = n_wavs,
    min_date = format(min_date, "%B %d"),
    max_date = format(max_date, "%B %d"),
    deployment_name = focal_deployment,
    swift_id = swift
  )

}

create_subfolders <- function(x, deployment_name, hard_drive_path) {

  fs::dir_create(
    stringr::str_glue('{hard_drive_path}/{stringr::str_extract(deployment_name, "G(P|R|C|M|0)[0-9]{2}_V[1-5]")}/{deployment_name}/{deployment_name}_{x}')
  )

}

# convert SD wavs to SSD flacs
wav_to_flac <- function(wav_path, deployment_name, desktop_path, hard_drive_path) {

  wav_date_time <- stringr::str_extract(wav_path, '[0-9]{8}_[0-9]{6}')
  wav_desktop_path <- stringr::str_glue('{desktop_path}/{basename(wav_path)}')

  fs::file_copy(
    wav_path,
    wav_desktop_path
  )

  flac_ssd_path <- stringr::str_glue('{hard_drive_path}/{stringr::str_extract(deployment_name, "G(P|R|C|M|0)[0-9]{2}_V[1-5]")}/{deployment_name}/{deployment_name}_{stringr::str_extract(wav_date_time, "[0-9]{8}")}/{deployment_name}_{wav_date_time}Z.flac')

  # wav to flac compression
  seewave::sox(
    stringr::str_glue('"{wav_desktop_path}" "{flac_ssd_path}"'),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

  # delete temporary wav on desktop
  fs::file_delete(wav_desktop_path)

}
