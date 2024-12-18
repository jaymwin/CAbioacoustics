
#' Read a BirdNET selection table
#'
#' @param birdnet_path Path to a BirdNET selection table
#'
#' @return Tibble version of a BirdNET selection table
#' @export
#'
#' @examples
#' \dontrun{
#' # set path to BirdNET selection file
#' birdnet_path <- here::here('G005_V1_C0125_U2_20230521_210001.BirdNET')
#'
#' # read in BirdNET file
#' birdnet_selection_df <- cb_read_birdnet_selections(birdnet_path)
#'
#' # or read and combine multiple BirdNET files at once
#' birdnet_selection_path <- "Z:/Acoustic_Data/ARU_Data_Processed/BirdNET_Postprocessing/Detection_Vetting/Sierra_Monitoring/2023_CSOW+BDOW/G001"
#' # get list of BirdNET files in a directory
#' birdnet_selection <- fs::dir_ls(birdnet_selections_2023_path, recurse = TRUE, glob = '*.BirdNET')
#'
#' # now combine in a single data frame
#' birdnet_selection_df <-
#'   birdnet_selection |>
#'   purrr::map_dfr(cb_read_birdnet_selections)
#' }

cb_read_birdnet_selection <- function(birdnet_path, type) {

  # if (type == 'csow_bdow') {
  #
  #   data.table::fread(birdnet_path) |>
  #     tibble::as_tibble() |>
  #     dplyr::mutate(dplyr::across(c(Selection, Channel), as.integer)) |>
  #     dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), as.character)) |>
  #     dplyr::mutate(dplyr::across(c(`Begin Time (s)`, `End Time (s)`, `Low Freq (Hz)`, `High Freq (Hz)`, Score), as.numeric)) |>
  #     # change blanks to NAs for now
  #     dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~dplyr::na_if(., ""))) # |>
  #   # dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~tidyr::replace_na(., "NA")))
  #
  # } else if (is.null(type) == TRUE) {
  #
  #   readr::read_delim(birdnet_path)
  #
  # }

  if (is.null(type) == TRUE) {

    readr::read_delim(
      birdnet_path,
      delim = '\t',
      col_types = 'icicddddcd'
    )

  } else if (type == 'csow_bdow') {

    readr::read_delim(
      birdnet_path,
      delim = '\t',
      col_types = 'icicddddcdccccc'
    ) |>
      # change blanks to NAs for now
      dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~dplyr::na_if(., "")))

  }

}
