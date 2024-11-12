
#' Title
#'
#' @param sf_object
#' @param unit
#' @param buffer
#'
#' @return
#' @export
#'
#' @examples

cb_get_elevation <- function(sf_object, unit, buffer = NULL) {

  if (unit == 'aru' & is.null(buffer) == TRUE) {

    sf_object <-
      elevatr::get_elev_point(sf_object, src = "epqs") |>
      dplyr::select(-elev_units)

  }

  if (unit == 'aru' & is.null(buffer) == FALSE) {

    # get data frame with elevation summarized at different buffer sizes
    elev_df <-
      buffer %>%
      # create column names
      stringr::str_c('elev', ., sep = '_') |>
      purrr::set_names() |>
      # iterate through buffer sizes
      purrr::map_dfr(
        \(x)
        exactextractr::exact_extract(
          ca_elev,
          # buffer points
          sf_object |> sf::st_transform(sf::st_crs(ca_elev)) |> sf::st_buffer(as.numeric(stringr::str_extract(x, "\\d"))),
          fun = 'mean'
        ),
        .id = 'buffer'
      )

    # bind to deployments and view
    sf_object <-
      sf_object |>
      dplyr::bind_cols(elev_df)

  }

  return(sf_object)

}
