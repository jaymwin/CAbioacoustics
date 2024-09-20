
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

cb_create_selection_table <- function(df, date_time, species_group) {

  flac_name <- unique(df$json)

  tibble::tibble(
    Selection = df$selection,
    View = 'Spectrogram 1',
    Channel = 1,
    `Begin File` = stringr::str_c(df$json, '.flac'),
    `Begin Time (s)` = df$relative_time,
    `End Time (s)` = df$relative_time + 3,
    `Low Freq (Hz)` = 150,
    `High Freq (Hz)` = 12000,
    `Common Name` = df$common_name,
    Score = df$score,
    Detector = 'BirdNET',
    Species = df$species,
    `Call Type` = df$call_type,
    Sex = NA,
    Keep = 'Y',
    Overwrite = 'no'
  ) |>
    write.table(
      file = stringr::str_glue(here::here('code_outputs/{species_group}_selection_tables_{date_time}/{flac_name}.BirdNET')),
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE
    )

}
