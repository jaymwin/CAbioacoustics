
#' Get start and end dates for establishing primary sampling period and the number of secondary sampling occasions for occupancy analyses
#'
#' @param aru_dates_df A data frame of ...
#' @param occasion_length Length (in days) of secondary sampling occasions
#'
#' @return A data frame of start/end dates and the number of secondary sampling occasions
#' @export
#'
#' @examples

cb_occ_start_end_dates <- function(aru_dates_df, occasion_length) {

  # always use 2021; year doesn't really matter
  start_date <- as.Date(aru_dates_df |> dplyr::filter(date_type == 'min_date') |> dplyr::pull(date) |> stringr::str_replace(pattern = '[0-9]{4}', '2021'))
  end_date <- as.Date(aru_dates_df |> dplyr::filter(date_type == 'max_date') |> dplyr::pull(date) |> stringr::str_replace(pattern = '[0-9]{4}', '2021'))

  num_occasions <- length(seq.Date(start_date, end_date, by = 'days')) / occasion_length

  check_integer <- function(N) {

    !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))

  }

  if (check_integer(num_occasions) == TRUE) {

    message('secondary sampling occasions are even length')

  } else {

    message('secondary sampling occasions NOT even length; trying different primary occasion end dates...')

  }

  i <- 0

  while (check_integer(num_occasions) == FALSE) {

    i <- i + 1
    end_date <- as.Date(aru_dates_df |> dplyr::filter(date_type == 'max_date') |> dplyr::pull(date) |> stringr::str_replace(pattern = '[0-9]{4}', '2021')) + i
    num_occasions <- length(seq.Date(start_date, end_date, by = 'days')) / occasion_length

    if (check_integer(num_occasions) == TRUE) {

      break
      new_end_date <- end_date

    }

  }

  message(glue::glue('season start date = {format(start_date, "%b %d")}'))
  message(glue::glue('season end date = {format(end_date, "%b %d")}'))
  message(glue::glue('assuming {occasion_length}-day secondary sampling occasions, there are {num_occasions} occasions'))

  tibble::tibble(
    start_date = start_date,
    end_date = end_date,
    num_occasions = num_occasions
  )

}
