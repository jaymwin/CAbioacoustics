
#' Shiny app to convert WAVs to FLACs
#'
#' @return
#' @export
#'
#' @examples

cb_shiny_wav_to_flac <- function() {

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
