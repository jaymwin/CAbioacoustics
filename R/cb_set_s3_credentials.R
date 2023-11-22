
#' Title
#'
#' @return
#' @export
#'
#' @examples

cb_set_s3_credentials <- function() {

  keyring::key_set('aws_access_key_id')
  keyring::key_set('aws_secret_access_key')

}
