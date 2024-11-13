
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

cb_get_terrain <- function(sf_object, scale, terrain_layer, terrain_name, buffer_size = NULL, fun = NULL) {

  if (scale == 'aru' & is.null(buffer_size) == TRUE) {

    terrain_df <- terra::extract(terrain_layer, deployments_sf |> sf::st_transform(sf::st_crs(terrain_layer)))

    sf_object <-
      sf_object |>
      dplyr::bind_cols(terrain_df |> dplyr::select(2) |> dplyr::rename_at(1, ~ terrain_name))

  }

  if (scale == 'aru' & is.null(buffer_size) == FALSE) {

    # get data frame with elevation summarized at different buffer sizes
    terrain_df <-
      buffer_size %>%
      # create column names
      stringr::str_c(terrain_name, ., sep = '_') |>
      purrr::set_names() |>
      # iterate through buffer sizes
      purrr::map_dfr(
        \(x)
        exactextractr::exact_extract(
          terrain_layer,
          # buffer points
          sf_object |> sf::st_transform(sf::st_crs(terrain_layer)) |> sf::st_buffer(as.numeric(stringr::str_extract(x, "\\d+"))),
          fun = fun,
          progress = FALSE
        ),
        .id = 'buffer_size'
      )

    if (length(fun) == 1) {

      # use single function name
      terrain_df <-
        terrain_df |>
        dplyr::select_all(list(~ paste0(fun, '_', .)))

    } else {

      # otherwise combine function, buffers for naming columns
      terrain_df <-
        terrain_df |>
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
      dplyr::bind_cols(terrain_df)

  }

  if (scale == 'hex' & is.null(buffer_size) == TRUE) {

    print('do hex stuff, not done yet')

  }

  return(sf_object)

}
