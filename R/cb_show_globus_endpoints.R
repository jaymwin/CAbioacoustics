
#' Print Globus endpoint IDs
#'
#' @return
#' @export
#'
#' @examples

cb_show_globus_endpoints <- function() {

  message(stringr::str_glue("Amazon S3 endpoint = {keyring::key_get('s3_globus_endpoint')}"))
  message(stringr::str_glue("Research Drive endpoint = {keyring::key_get('research_drive_globus_endpoint')}"))
  message(stringr::str_glue("Jay's laptop = {keyring::key_get('jmwiniarski_laptop_globus_endpoint')}"))

}
