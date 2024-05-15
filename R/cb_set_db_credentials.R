
#' Establish credentials for the UW spotted owl database
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # this will prompt to enter 1) the host, 2) user, 3) password, and 4) database name
#' cb_set_db_credentials()
#' }

cb_set_db_credentials <- function() {

  keyring::key_set('spotted_owl_host')
  keyring::key_set('spotted_owl_user')
  keyring::key_set('spotted_owl_pwd')
  keyring::key_set('spottedowl_db')

}
