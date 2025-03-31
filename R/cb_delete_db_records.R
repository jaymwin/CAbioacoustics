
#' Delete a record from the database
#'
#' @param ids
#' @param table_name
#'
#' @return
#' @export
#'
#' @examples

cb_delete_db_records <- function(ids, table_name) {

  # Convert the list of IDs to a string for the SQL query
  ids_to_delete <- paste(ids, collapse = ", ")

  # Create the DELETE SQL statement
  delete_query <- stringr::str_glue("DELETE FROM {table_name} WHERE id IN (", ids_to_delete, ")", sep = "")

  # Execute the DELETE query
  DBI::dbExecute(conn, delete_query)

}
