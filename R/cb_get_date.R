
#' Get datetime in character format for current or most recent code run
#'
#' @param type
#' @param folder
#'
#' @return
#' @export
#'
#' @examples

cb_get_date <- function(type, folder) {

  if (type == 'previous') {

    # pick the folder (e.g., pre_birdnet, post_birdnet)
    datetime <-
      fs::dir_ls(here::here('code_outputs'), regexp = folder) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        date_chr = stringr::str_extract(value, '[0-9]{14}'),
        date = lubridate::ymd_hms(date_chr)
      ) |>
      dplyr::filter(date == max(date)) |>
      dplyr::pull(date_chr)

  }

  if (type == 'current') {

    datetime <- stringr::str_flatten(stringr::str_extract_all(lubridate::ymd_hms(Sys.time()), '[0-9]+')[[1]][1:6], '')

  }

  return(datetime)

}
