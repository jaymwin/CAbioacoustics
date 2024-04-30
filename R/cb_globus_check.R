
#' Check if you're set up with Globus from R
#'
#' @return
#' @export
#'
#' @examples

cb_globus_check <- function() {

  system("globus whoami")

}
