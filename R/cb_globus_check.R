
#' Check if you're set up with Globus from R
#'
#' @return Globus credentials (i.e., UW-Madison email)
#' @export
#'
#' @examples
#' \dontrun{
#' cb_globus_check()
#' }

cb_globus_check <- function() {

  system("globus whoami")

}
