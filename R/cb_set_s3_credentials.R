
#' Establish credentials to access Amazon S3 bucket
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # this will prompt to enter 1) the AWS access key ID, and 2) AWS secret access key
#' cb_set_s3_credentials()
#' }

cb_set_s3_credentials <- function() {

  keyring::key_set('aws_access_key_id')
  keyring::key_set('aws_secret_access_key')

}
