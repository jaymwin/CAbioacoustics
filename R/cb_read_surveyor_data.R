
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

  # depending on how the clean data is saved, use read_csv or read_excel
  file_type <- df |> stringr::str_extract('.csv|.xlsx')

  if (file_type == ".csv") {

    readr::read_csv(df) |>
      dplyr::select(lat_WGS84, lon_WGS84, survey_night_year, survey_night_month, survey_night_day)

  } else {

    readxl::read_excel(df) |>
      dplyr::select(lat_WGS84, lon_WGS84, survey_night_year, survey_night_month, survey_night_day)

  }

}
