
#' Title
#'
#' @param network_drive
#' @param survey_year
#' @param regions
#'
#' @return
#' @export
#'
#' @examples

cb_json_file_structure <- function(network_drive, survey_year, regions) {

  stringr::str_c(
    network_drive,
    '/Acoustic_Data/ARU_Data_Processed/BirdNET_Results_JSON/(JSON_Sierra_Monitoring|JSON_Sierra_Projects)/',
    survey_year,
    '/',
    '(',
    stringr::str_flatten(regions, '|'),
    ')/G(P|R|C|M|[0-9])[0-9]{2}_V[0-9]{1}/G(P|R|C|M|[0-9])[0-9]{2}_V[0-9]{1}_C[0-9]{4}_U[0-9]{1}/G(P|R|C|M|[0-9])[0-9]{2}_V[0-9]{1}_C[0-9]{4}_U[0-9]{1}_[0-9]{8}/G(P|R|C|M|[0-9])[0-9]{2}_V[0-9]{1}_C[0-9]{4}_U[0-9]{1}_[0-9]{8}_[0-9]{6}Z.json.gz'
  )

}
