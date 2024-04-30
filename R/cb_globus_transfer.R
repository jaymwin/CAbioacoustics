
#' Execute a Globus file transfer between two collections from R
#'
#' @param collection_one
#' @param collection_two
#' @param destination_path
#' @param batch_file_path
#' @param label
#'
#' @return
#' @export
#'
#' @examples

cb_globus_transfer <- function(collection_one, collection_two, destination_path, batch_file_path, label) {

  globus_transfer <-
    stringr::str_glue(
      'globus transfer %{collection_one}%:/ %{collection_two}%:{destination_path} --label "{label}" --batch {batch_file_path}'
    )

  print(globus_transfer)

  # run the transfer
  system(globus_transfer)

}
