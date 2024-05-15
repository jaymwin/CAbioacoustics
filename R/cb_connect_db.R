
#' Connect to the UW spotted owl database
#'
#' @return `DBI` database connection
#' @export
#'
#' @examples
#' \dontrun{
#' # connect to the database
#' cb_connect_db()
#' }

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
