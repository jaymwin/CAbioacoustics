
#' Clear contents of an external hard drive
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples

cb_clear_hard_drive <- function(path) {

  # Ask for user confirmation
  confirmation <- readline(prompt = stringr::str_glue("Are you sure you want to delete all contents of {path}? Type 'yes' to confirm: "))

  if (path %in% c('Y:/', 'Z:/')) stop('Can not delete this drive!')

  # Check if user confirmed
  if (tolower(confirmation) == "yes") {

    # Proceed with the deletion
    fs::file_delete(
      fs::dir_ls(path) |>
        stringr::str_subset('RECYCLE', negate = TRUE)
    )
    cat("Contents of the hard drive have been deleted.\n")

  } else {

    cat("Operation canceled. No files were deleted.\n")

  }

}
