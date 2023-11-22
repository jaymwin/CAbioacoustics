
#' Title
#'
#' @return
#' @export
#'
#' @examples

cb_check_s3 <- function() {

  # check everything is working
  aws.s3::bucket_exists(
    bucket = "mpeery-archive",
    region = "",
    base_url = "s3.drive.wisc.edu",
    key = keyring::key_get('aws_access_key_id'),
    secret = keyring::key_get('aws_secret_access_key')
  )

}
