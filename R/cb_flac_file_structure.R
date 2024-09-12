
#' Check that flacs are properly nested on research drive
#'
#' @param network_drive
#' @param survey_year
#' @param regions
#'
#' @return
#' @export
#'
#' @examples

cb_flac_file_structure <- function(network_drive, survey_year, regions) {

  stringr::str_c(
    network_drive,
    '/Acoustic_Data/ARU_Data_Raw/(ARU_Sierra_Monitoring|ARU_Sierra_Projects|ARU_Coastal_Barred)/',
    survey_year,
    '/',
    '(',
    stringr::str_flatten(regions, '|'),
    ')/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}_[0-9]{8}/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}_[0-9]{8}_[0-9]{6}Z.flac'
  )

}
