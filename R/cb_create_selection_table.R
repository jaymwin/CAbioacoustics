
#' Create selection tables for CSOW/BDOW or forest owls
#'
#' @param df
#' @param date_time
#' @param species_group
#'
#' @return
#' @export
#'
#' @examples

cb_create_selection_table <- function(df, date_time, species_group) {

  flac_name <- unique(df$json)
  group <- stringr::str_sub(flac_name, 1, 4)
  cell <- stringr::str_extract(flac_name, 'C[0-9]{4}')

  if (species_group == 'csow_bdow') {

  tibble::tibble(
    Selection = df$selection,
    View = 'Spectrogram 1',
    Channel = 1,
    `Begin File` = stringr::str_c(df$json, 'Z.flac'),
    `Begin Time (s)` = df$relative_time,
    `End Time (s)` = df$relative_time + 3,
    `Low Freq (Hz)` = 150,
    `High Freq (Hz)` = 12000,
    `Common Name` = df$common_name,
    Score = df$score,
    Detector = 'BirdNET',
    Species = df$species,
    `Call Type` = stringr::str_to_lower(df$call_type),
    Sex = '',
    Keep = '',
    Overwrite = 'no'
  ) |>
    write.table(
      file = stringr::str_glue(here::here('code_outputs/csow_bdow_selection_tables_{date_time}/{group}/{cell}/{flac_name}.BirdNET')),
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE
    )

  }

  if (species_group == 'forest_owl') {

    tibble::tibble(
      Selection = df$selection,
      View = 'Spectrogram 1',
      Channel = 1,
      `Begin File` = stringr::str_c(df$json, 'Z.flac'),
      `Begin Time (s)` = df$relative_time,
      `End Time (s)` = df$relative_time + 3,
      `Low Freq (Hz)` = 150,
      `High Freq (Hz)` = 12000,
      `Common Name` = df$common_name,
      Score = df$score,
      Detector = 'BirdNET',
      Species = df$species,
      Keep = ''
    ) |>
      write.table(
        file = stringr::str_glue(here::here('code_outputs/forest_owl_selection_tables_{date_time}/{group}/{cell}/{flac_name}.BirdNET')),
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )

  }

}
