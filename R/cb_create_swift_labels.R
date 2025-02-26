
#' Create PDF of scannable QR codes for Swift recorders (geared toward Avery 5520 labels)
#'
#' @param swift_df
#' @param type
#' @param file
#' @param font_size
#' @param label_height_ratio
#'
#' @return
#' @export
#'
#' @examples

cb_create_swift_labels <- function(swift_df, type = 'matrix', file, font_size, label_height_ratio) {

  num_row <- 10
  page_height <- 11
  height_margin = 0.5

  baRcodeR::custom_create_PDF(
    Labels = swift_df |> as.data.frame(),
    type = type,
    # .pdf not necessary
    name = stringr::str_remove(file, '.pdf'),
    Fsz = font_size,
    numrow = 10,
    numcol = 3,
    label_height = ((page_height - 2 * height_margin) / num_row) * label_height_ratio,
    width_margin = 3/8,
    height_margin = 1/2
  )

}
