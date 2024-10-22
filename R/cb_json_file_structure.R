
#' Create expected JSON file path based on drive location, survey year, and study regions
#'
#' @param network_drive
#' @param survey_year
#' @param regions
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # example ARU data frame
#' # set mapped drive to research path
#' network_drive <- 'Z:'
#' # path to data
#' path <- stringr::str_c(network_drive, "/Acoustic_Data/ARU_Data_Processed/BirdNET_Results_JSON")
#' # current year = survey year
#' data_year <- lubridate::year(lubridate::today())

#' # string with proper naming convention
#' json_file_structure <-
#'   cb_json_file_structure(
#'     network_drive = network_drive,
#'     survey_year = data_year,
#'     regions = regions
#'  )
#'  print(json_file_structure)
#' }

cb_json_file_structure <- function(network_drive, survey_year, regions) {

  stringr::str_c(
    network_drive,
    '/Acoustic_Data/ARU_Data_Processed/BirdNET_Results_JSON/(JSON_Sierra_Monitoring|JSON_Sierra_Projects|JSON_Coastal_Barred)/',
    survey_year,
    '/',
    '(',
    stringr::str_flatten(regions, '|'),
    ')/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}_[0-9]{8}/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}_[0-9]{8}_[0-9]{6}Z.json.gz'
  )

}
