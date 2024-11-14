
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

cb_extract_raster <- function(sf_object, scale, raster_layer, raster_name, buffer_size = NULL, fun = NULL) {

  if (scale == 'aru' & is.null(buffer_size) == TRUE) {

    extract_df <- terra::extract(raster_layer, sf_object |> sf::st_transform(sf::st_crs(raster_layer)))

    sf_object <-
      sf_object |>
      dplyr::bind_cols(extract_df |> dplyr::select(2) |> dplyr::rename_at(1, ~ raster_name))

  }

  if (scale == 'aru' & is.null(buffer_size) == FALSE) {

    # get data frame with elevation summarized at different buffer sizes
    extract_df <-
      buffer_size %>%
      # create column names
      stringr::str_c(raster_name, ., sep = '_') |>
      purrr::set_names() |>
      # iterate through buffer sizes
      purrr::map_dfr(
        \(x)
        exactextractr::exact_extract(
          raster_layer,
          # buffer points
          sf_object |> sf::st_transform(sf::st_crs(raster_layer)) |> sf::st_buffer(as.numeric(stringr::str_extract(x, "\\d+"))),
          fun = fun,
          progress = FALSE
        ),
        .id = 'buffer_size'
      )

    if (length(fun) == 1) {

      # use single function name
      extract_df <-
        extract_df |>
        dplyr::select_all(list(~ paste0(fun, '_', .)))

    } else {

      # otherwise combine function, buffers for naming columns
      extract_df <-
        extract_df |>
        tidyr::pivot_longer(!buffer_size, names_to = "statistic", values_to = "value") |>
        tidyr::pivot_wider(
          names_from = c(statistic, buffer_size),
          names_glue = "{statistic}_{buffer_size}",
          values_from = value
        ) |>
        tidyr::unnest() |>
        suppressWarnings()

    }

    # bind to deployments
    sf_object <-
      sf_object |>
      dplyr::bind_cols(extract_df)

  }

  if (scale == 'hex' & is.null(buffer_size) == TRUE) {

    extract_df <-
      exactextractr::exact_extract(
      raster_layer,
      # buffer points
      sf_object |> sf::st_transform(sf::st_crs(raster_layer)),
      fun = fun,
      progress = FALSE
    )

    # bind to deployments
    sf_object <-
      sf_object |>
      dplyr::bind_cols(extract_df)

  }

  return(sf_object)

}
