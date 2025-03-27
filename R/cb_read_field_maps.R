
#' Read ARU deployment/recovery data from ArcGIS Field Maps
#'
#' @param feature_layer HTML to hosted Field Maps feature layer
#'
#' @return
#' @export
#'
#' @examples

cb_read_field_maps <- function(feature_layer) {

  # make sure there's arcgis pro on your computer and it connects
  print(arcgisbinding::arc.check_product())

  # read now as sf object
  arcgisbinding::arc.data2sf(
    arcgisbinding::arc.select(
      arcgisbinding::arc.open(
        feature_layer
      )
    )
  ) |>
    janitor::clean_names() |>
    dplry::select(
      creation_date,
      deployment_or_recovery,
      deployed_by,
      deployment_date_and_time,
      swift_id,
      sd_card_id,
      group_id,
      visit_id,
      cell_id,
      unit_number,
      deployment_notes,
      recovery_date_and_time,
      recovered_by,
      unit_missingnot_recovered,
      recover_recording_status,
      unit_broken,
      microphone_missingdamaged,
      microphone_windscreen_damaged,
      recover_notes,
      did_you_take_a_deployment_photo,
      dplyr::matches('esri')
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., " "))) |>
    sf::st_transform(4326)

}
