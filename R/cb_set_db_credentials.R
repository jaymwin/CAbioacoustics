
#' Title
#'
#' @return
#' @export
#'
#' @examples

cb_set_db_credentials <- function() {

  keyring::key_set('spotted_owl_host')
  keyring::key_set('spotted_owl_user')
  keyring::key_set('spotted_owl_pwd')
  keyring::key_set('spottedowl_db')

}
