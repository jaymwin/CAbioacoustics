
#' Title
#'
#' @return
#' @export
#'
#' @examples

cb_disconnect_db <- function() {

  DBI::dbDisconnect(conn)

}
