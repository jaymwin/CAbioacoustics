
#' Append new detections to a duckdb of all BirdNET species detections
#'
#' @return Updated duckdb
#' @export
#'
#' @examples

cb_update_birdnet_db <- function(spp_predictions_df) {

  # if 2024, create duckdb for the first time; otherwise don't and just append the new data
  if (!file.exists("Z:/sierra_birdnet_detections_effort.duckdb")) {

    message("database doesn't exist yet; building database...")

    # create database
    conn <- DBI::dbConnect(duckdb::duckdb(dbdir = "Z:/sierra_birdnet_detections_effort.duckdb"))

    # create empty table of species detections
    spp_detections_template <-
      tibble::tibble(
        json = as.character(NA),
        relative_time = as.numeric(NA),
        species_code = as.numeric(NA),
        birdnet_logit = as.numeric(NA)
      )

    # full set of thresholds
    thresholds_df <-
      readr::read_csv(here::here('birdnet_sierra_241_spp_thresholds.csv'), show_col_types = FALSE)

    # create these tables in the database
    DBI::dbCreateTable(conn, "species_detections", spp_detections_template)
    DBI::dbCreateTable(conn, "species_thresholds", thresholds_df)

    # then append to empty database
    message('appending new data to database...')

    conn <- DBI::dbConnect(duckdb::duckdb(dbdir = "Z:/sierra_birdnet_detections_effort.duckdb"))

    DBI::dbAppendTable(conn, "species_detections", spp_predictions_df)
    duckdb::duckdb_read_csv(conn, "species_thresholds", here::here('birdnet_sierra_241_spp_thresholds.csv'))

    DBI::dbDisconnect(conn)

  } else {

    # otherwise append new data
    message('appending new data to database...')

    conn <- DBI::dbConnect(duckdb::duckdb(dbdir = "Z:/sierra_birdnet_detections_effort.duckdb"))

    DBI::dbAppendTable(conn, "species_detections", spp_predictions_df)

    DBI::dbDisconnect(conn)

  }

}
