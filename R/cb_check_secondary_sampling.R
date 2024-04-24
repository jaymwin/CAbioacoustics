
#' Title
#'
#' @param start_date
#' @param end_date
#' @param occasion_length
#'
#' @return
#' @export
#'
#' @examples

cb_check_secondary_sampling <- function(start_date, end_date, occasion_length) {

  start_date <- as.Date(stringr::str_c('2021', '-', start_date))
  end_date <- as.Date(stringr::str_c('2021', '-', end_date))

  num_occasions <- length(seq.Date(start_date, end_date, by = 'days')) / occasion_length

  check_integer <- function(N) {

    !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))

  }

  if (check_integer(num_occasions) == TRUE) {

    message('secondary sampling occasions are even length')

  } else {

    message('secondary sampling occasions NOT even length; try different primary occasion dates')

  }

}
