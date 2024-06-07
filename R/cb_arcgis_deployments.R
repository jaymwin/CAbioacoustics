
#' Pull ARU deployment information entered into ArcGIS Field Maps app
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples

cb_arcgis_deployments <- function(object) {

  if (object == 'arcgis_deployments') {

    arcgis_sf <-
      arcgisbinding::arc.data2sf(
        arcgisbinding::arc.select(
          arcgisbinding::arc.open(
            keyring::key_get('arcgis_deployments')
          )
        )
      ) |>
      # clean up
      dplyr::select(
        OBJECTID,
        Creator,
        cell_id,
        deployer_1,
        deployer_2,
        unit_number,
        swift_id,
        sd_id,
        date_time,
        esrignss_latitude,
        esrignss_longitude,
        esrignss_positionsourcetype,
        esrignss_receiver,
        esrignss_fixdatetime,
        esrignss_avg_h_rms,
        esrignss_avg_positions,
        CreationDate
      )

  }

  return(arcgis_sf)

}
