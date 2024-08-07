
#' Check that Amazon S3 bucket exists
#'
#' @return Output will contain `TRUE` if connection to S3 bucket is working
#' @export
#'
#' @examples
#' \dontrun{
#' cb_check_s3()
#' }

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
