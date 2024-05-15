
#' Disconnect from spotted owl database when finished with queries
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cb_disconnect_db()
#' }

cb_disconnect_db <- function() {

  DBI::dbDisconnect(conn)

}
