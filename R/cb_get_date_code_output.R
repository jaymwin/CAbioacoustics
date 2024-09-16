
#' Get date of most recent code run
#'
#' @param folder A code output folder (e.g., pre_birdnet, post_birdnet).
#'
#' @return
#' @export
#'
#' @examples

cb_get_date_code_output <- function(folder) {

  # pick the folder (e.g., pre_birdnet, post_birdnet)
  fs::dir_ls(here::here('code_outputs'), regexp = folder) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      date_chr = stringr::str_extract(value, '[0-9]{14}'),
      date = lubridate::ymd_hms(date_chr)
    ) |>
    dplyr::filter(date == max(date)) |>
    dplyr::pull(date_chr)

}

