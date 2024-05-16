
#' Save FLAC files on Amazon S3 locally
#'
#' @param file Path to FLAC file on Amazon S3
#' @param destination_folder Path to folder where FLAC will be downloaded
#'
#' @return FLAC file
#' @export
#'
#' @examples
#' \dontrun{
#' cb_save_s3_flac(amazon_s3_key, here::here())
#' }

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
