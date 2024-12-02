
#' Read broadcast survey data
#'
#' @param path Path to surveyor data
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # path to data
#' path <- here::here('surveyor_data_example.csv')
#' surveyor_df <- cb_read_surveyor_data(path)
#' }

cb_read_surveyor_data <- function(path) {

  file_type <- stringr::str_extract(path, ".csv|.xlsx")

  if (file_type == ".csv") {

    readr::read_csv(path) |>
      dplyr::select(lat_WGS84, lon_WGS84, survey_night_year, survey_night_month, survey_night_day) |>
      dplyr::mutate_if(is.character, as.numeric)

  }

  else {

    readxl::read_excel(path) |>
      dplyr::select(lat_WGS84, lon_WGS84, survey_night_year, survey_night_month, survey_night_day) |>
      dplyr::mutate_if(is.character, as.numeric)

  }

}
