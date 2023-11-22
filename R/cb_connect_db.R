
#' Title
#'
#' @return
#' @export
#'
#' @examples

cb_connect_db <- function() {

  conn <<-
    DBI::dbConnect(
      drv = RMariaDB::MariaDB(),
      host = keyring::key_get('spotted_owl_host'),
      port = "3306",
      user = keyring::key_get('spotted_owl_user'),
      password = keyring::key_get('spotted_owl_pwd'),
      dbname = keyring::key_get('spottedowl_db')
    )

}
