
#' Convert WAV files stored on Swift SD card to FLAC files stored on external hard drive
#'
#' @param desktop_path Path to desktop
#' @param sd_card_path Path to SD card
#' @param hard_drive_path Path to external hard drive
#' @param deployment_df Data frame containing Swift ID
#'
#' @return WAV files from SD card transferred and converted to FLAC on external hard drive
#' @export
#' @include utils.R
#'
#' @examples
#' \dontrun{
#' # adjust for your desktop path
#' desktop_path <- "C:/Users/jmwin/OneDrive/Desktop"
#'
#' # set other paths; change accordingly
#' sd_card_path <- "E:/"
#' hard_drive_path <- "D:/"
#'
#' # deployment data
#' deployment_df <-
#'  tibble(
#'    swift_id = 'S1732',
#'    deployment_name = 'G030_V1_C1989_U1',
#'    survey_year = 2023
#'  )
#'
#' # transfer and convert flacs (SD -> hard drive)
#' cb_wav_to_flac(
#'   desktop_path = desktop_path,
#'   sd_card_path = sd_card_path,
#'   hard_drive_path = hard_drive_path,
#'   deployment_df = fake_owl_db
#' )
#'}

cb_wav_to_flac <- function(desktop_path, sd_card_path, hard_drive_path, deployment_df) {

  tictoc::tic('total runtime')
  # set up desktop folders
  desktop_wav_path <- stringr::str_c(desktop_path, 'wavs', sep = '/')
  desktop_flac_path <- stringr::str_c(desktop_path, 'flacs', sep = '/')

  # create these folders
  fs::dir_create(desktop_wav_path)
  fs::dir_create(desktop_flac_path)

  # locate wav folders on the SD card
  sd_wav_folders <-
    fs::dir_ls(sd_card_path) |>
    # should all be SXXXX_XXX where X is number
    stringr::str_subset("[S][0-9]{4}[_][0-9]{3}")

  # get swift ID from SD card
  card_swift_id <-
    sd_wav_folders |>
    stringr::str_extract("[S][0-9]{4}") |>
    unique()

  # get year from SD card
  card_survey_year <-
    fs::dir_ls(sd_wav_folders, recurse = 1) |>
    # get dates
    stringr::str_extract("[0-9]{4}[-][0-9]{2}[-][0-9]{2}") |>
    # get year
    stringr::str_extract("[0-9]{4}") |>
    unique() |>
    as.numeric()

  # use this to get deployment info
  deployment <-
    deployment_df |>
    dplyr::filter(swift_id == card_swift_id & survey_year == card_survey_year)

  # create flac folder paths from deployment name associated with swift ID, year
  group <- stringr::str_sub(deployment$deployment_name, 1, 4)
  visit <- stringr::str_extract(deployment$deployment_name, '[V][1-9]{1}')
  cell <- stringr::str_extract(deployment$deployment_name, '[C][1-9]{4}')
  unit_number <- stringr::str_extract(deployment$deployment_name, '[U][1-9]{1}')
  group_visit <- stringr::str_c(group, visit, sep = '_')
  group_visit_cell_unit <- stringr::str_c(group_visit, cell, unit_number, sep = '_')

  # check this
  message('deployment name - ', group_visit_cell_unit, '...')

  # create all paths now
  file_paths_df <-
    fs::dir_ls(sd_wav_folders, recurse = TRUE, glob = '*.wav') |>
    tibble::as_tibble() |>
    dplyr::rename(sd_wav_path = value) |>
    dplyr::mutate(
      desktop_wav_path = stringr::str_c(desktop_wav_path, stringr::str_remove(sd_wav_path, sd_card_path), sep = '/'),
      date = stringr::str_split(basename(sd_wav_path), pattern = "_", n = Inf, simplify = TRUE)[,2],
      date = stringr::str_remove_all(date, '-'),
      time = stringr::str_remove(stringr::str_split(basename(sd_wav_path), pattern = "_", n = Inf, simplify = TRUE)[,3], '.wav'),
      desktop_flac_path = stringr::str_c(
        desktop_flac_path,
        group_visit,
        group_visit_cell_unit,
        stringr::str_c(group_visit_cell_unit, '_', date),
        stringr::str_c(group_visit_cell_unit, '_', date, '_', time, 'Z.flac'),
        sep = '/'
      )
    ) |>
    # don't need these now
    dplyr::select(-date, -time)

  # create desktop flac subfolders
  file_paths_df |>
    dplyr::pull(desktop_flac_path) %>%
    stringr::str_remove(basename(.)) |>
    # subfolder for each day
    unique() |>
    # get rid of / on end
    stringr::str_sub(end = -2) |>
    # create directory for each day
    purrr::walk(\(x) fs::dir_create(x))

  print(desktop_wav_path)
  # copy SD wav directories to desktop
  message('copying wav directories on SD card to desktop...')
  start <- Sys.time()
  sd_wav_folders |>
    purrr::walk(\(x) copy_sd_wav_folders_to_desktop(x, desktop_wav_path, sd_wav_folders, sd_card_path))
  end <- Sys.time()
  time_diff <- round(as.numeric(end - start, units = "secs") / 60, 2)
  message(time_diff, ' minutes to complete task')

  # convert wav to flac on desktop
  message('converting wavs on desktop to flacs on desktop...')
  start <- Sys.time()
  file_paths_df |>
    dplyr::select(dplyr::matches('desktop')) |>
    furrr::future_pwalk(flac_conversion)
  end <- Sys.time()
  time_diff <- round(as.numeric(end - start, units = "secs") / 60, 2)
  message(time_diff, ' minutes to complete task')

  # delete desktop wavs
  fs::dir_ls(desktop_wav_path, regex = card_swift_id) |>
    purrr::walk(\(x) fs::dir_delete(x))

  # copy flacs to external hard drive
  message('copying desktop flacs to external hard drive...')
  start <- Sys.time()
  copy_to_hd(desktop_flac_path, hard_drive_path, group_visit)
  end <- Sys.time()
  time_diff <- round(as.numeric(end - start, units = "secs") / 60, 2)
  message(time_diff, ' minutes to complete task')

  # delete desktop flacs
  fs::dir_delete(stringr::str_c(desktop_flac_path, group_visit, sep = '/'))

  # eject SD card now?

  message('done!')

  # tally files transferred/converted
  # cont flacs that were transferred
  n_flacs <- length(fs::dir_ls(stringr::str_c(hard_drive_path, group_visit, sep = '/'), glob = '*.flac', recurse = TRUE))
  message(dim(file_paths_df)[1], ' wavs on SD; ', n_flacs, ' flacs on external hard drive')

  # make a noise when done
  beepr::beep('ping')

  # total time
  tictoc::toc()

}
