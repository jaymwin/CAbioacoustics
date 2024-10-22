
#' Set number of cores to run functions in parallel with future/furrr
#'
#' @param n Number of cores to use. If blank, use all except leave one available for other tasks.
#'
#' @return Number of cores to use in parallel
#' @export
#'
#' @examples
#' \dontrun{
#' # use all cores - 1
#' cb_set_future_cores()
#'
#' # or a specified number of cores
#' cb_set_future_cores(n = 10)
#' }

cb_set_future_cores <- function(n) {

  if (missing(n)) {

    message(stringr::str_glue('setting cores to {parallelly::availableCores() - 1} (available cores minus one)'))
    future::plan(future::multisession, workers = parallelly::availableCores() - 1)

  } else {

    message(stringr::str_glue('setting cores to {n}'))
    future::plan(future::multisession, workers = n)

  }

}
