
#' Create PDF of Swift barcodes or QR codes
#'
#' @param swift_df
#' @param type
#' @param file
#'
#' @return
#' @export
#'
#' @examples

cb_create_swift_labels <- function(swift_df, type, file) {

  num_row <- 10
  page_height <- 11
  height_margin = 0.5

  baRcodeR::custom_create_PDF(
    Labels = swift_df |> as.data.frame(),
    type = type,
    name = file,
    Fsz = 18,
    numrow = 10,
    numcol = 3,
    label_height = ((page_height - 2 * height_margin) / num_row) * 8/10,
    width_margin = 3/8,
    height_margin = 1/2
  )

}
