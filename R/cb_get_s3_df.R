

#' Title
#'
#' @return
#' @export
#'
#' @examples

cb_get_s3_df <- function() {

  aws.s3::get_bucket_df(
    bucket = "mpeery-archive",
    region = "",
    base_url = "s3.drive.wisc.edu",
    key = keyring::key_get('aws_access_key_id'),
    secret = keyring::key_get('aws_secret_access_key'),
    # it takes a while for this to run (~30 mins?); needs to be Inf to read in all files
    max = Inf
  ) |>
    tibble::as_tibble()

}
