
#' Connect to database containing S3 contents
#'
#' @return
#' @export
#'
#' @examples

cb_connect_s3_db <- function() {

  # connect to local s3 database
  conn_s3 <<- DBI::dbConnect(duckdb::duckdb(dbdir = "Z:/s3_bucket_contents/s3_files.duckdb"))

}
