
# write script to save each file to flac
#' Title
#'
#' @param file
#' @param destination_folder
#'
#' @return
#' @export
#'
#' @examples

cb_save_s3_flac <- function(file, destination_folder) {

  aws.s3::save_object(
    object = file,
    bucket = "mpeery-archive",
    region = "",
    base_url = "s3.drive.wisc.edu",
    key = keyring::key_get('aws_access_key_id'),
    secret = keyring::key_get('aws_secret_access_key'),
    file = stringr::str_c(destination_folder, '/', basename(file))
  )

}
