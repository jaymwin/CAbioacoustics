
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
#' }

cb_read_birdnet_selections <- function(birdnet_path) {

  data.table::fread(birdnet_path) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c(Selection, Channel), as.integer)) |>
    dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), as.character)) |>
    dplyr::mutate(dplyr::across(c(`Begin Time (s)`, `End Time (s)`, `Low Freq (Hz)`, `High Freq (Hz)`, Score), as.numeric)) |>
    # change blanks to NAs for now
    dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~dplyr::na_if(., ""))) |>
    dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~tidyr::replace_na(., "NA")))

}
