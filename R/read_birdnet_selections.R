
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

read_birdnet_selections <- function(df) {

  data.table::fread(df) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c(Selection, Channel), as.integer)) |>
    dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), as.character)) |>
    dplyr::mutate(dplyr::across(c(`Begin Time (s)`, `End Time (s)`, `Low Freq (Hz)`, `High Freq (Hz)`, Score), as.numeric)) |>
    # change blanks to NAs for now
    dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~dplyr::na_if(., ""))) |>
    dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~tidyr::replace_na(., "NA")))

}
