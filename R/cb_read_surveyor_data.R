
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

cb_read_surveyor_data <- function(df) {

  file_type <- df |> stringr::str_extract('.csv|.xlsx')

  if (file_type == ".csv") {

    readr::read_csv(df) |>
      dplyr::select(lat_WGS84, lon_WGS84, survey_night_year, survey_night_month, survey_night_day)

  } else {

    readxl::read_excel(df) |>
      dplyr::select(lat_WGS84, lon_WGS84, survey_night_year, survey_night_month, survey_night_day)

  }

}
