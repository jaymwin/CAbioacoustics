
#' Create a database of S3 contents that is easy to query
#'
#' @return
#' @export
#'
#' @examples

cb_build_s3_db <- function(s3_df) {

  # create database
  conn <- DBI::dbConnect(duckdb::duckdb(dbdir = "Z:/s3_bucket_contents/s3_files.duckdb"))

  # create empty table of species detections
  s3_template <-
    tibble::tibble(
      Key = as.character(NA),
      LastModified = as.character(NA),
      ETag = as.character(NA),
      Size = as.character(NA),
      Owner_ID = as.character(NA),
      Owner_DisplayName = as.character(NA),
      StorageClass = as.character(NA),
      Bucket = as.character(NA)
    )

  # create these tables in the database
  DBI::dbCreateTable(conn, "s3_contents", s3_template)

  DBI::dbAppendTable(conn, "s3_contents", s3_df)

  DBI::dbDisconnect(conn)

}
