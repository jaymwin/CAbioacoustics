
#' Get summary of sampling effort
#'
#' @param species
#' @param template
#' @param study_type
#' @param cell_ids
#' @param start_year
#' @param end_year
#'
#' @return
#' @export
#'
#' @examples

cb_sampling_summary <- function(species, template, study_type, cell_ids, start_year, end_year) {

  focal_species <- species
  template_used <- template

  # query acoustic field deployments table
  deployments_sql_df <-
    conn |>
    dplyr::tbl('acoustic_field_visits') |>
    # only use valid records
    dplyr::filter(is_invalid == 0) |>
    # keep necessary deployment data
    dplyr::select(id:unit_number)

  # query acoustic efforts table
  efforts_sql_df <-
    conn |>
    dplyr::tbl('acoustic_efforts') |>
    dplyr::select(id:survey_hour) |>
    dplyr::rename(effort_id = id)

  # query acoustic detections table
  detections_sql_df <-
    conn |>
    dplyr::tbl('acoustic_detections') |>
    dplyr::filter(species %in% focal_species & template_used == template) |>
    dplyr::select(acoustic_effort_id, begin_file, template_used) |>
    # keep all detections for this summary
    dplyr::mutate(detection = 1)

  # sql dataframe with efforts, their deployment info, and any detections for that particular survey hour
  efforts_deployments_detections_sql_df <-
    efforts_sql_df |>
    # deployment acoustic_field_visit_id = effort id; rename to join
    dplyr::left_join(deployments_sql_df |> dplyr::rename(acoustic_field_visit_id = id), by = dplyr::join_by('acoustic_field_visit_id')) |>
    # detections effort id = effort id
    dplyr::left_join(detections_sql_df |> dplyr::rename(effort_id = acoustic_effort_id), by = dplyr::join_by('effort_id')) |>
    # clean up
    dplyr::select(survey_date:unit_number, detection)

  # keep relevant data for owls
  efforts_deployments_detections_sql_df <-
    efforts_deployments_detections_sql_df |>
    # make survey year an integer
    # mutate(survey_year = sql("cast(survey_year as signed)")) |>
    # filter to study type, years, and appropriate survey hours for owls; usfs cells only
    dplyr::filter(
      cell_id %in% cell_ids,
      study_type == study,
      survey_year >= start_year,
      survey_year <= end_year,
      # keep efforts from 8:00PM to 6:00AM (for owls)
      survey_hour >= '20:00' & survey_hour < '23:59' | survey_hour >= '00:00' & survey_hour < '05:59'
    ) |>
    # create a field for survey night (i.e., biologically-relevant night for an owl)
    dplyr::mutate(
      survey_night = dplyr::case_when(
        survey_hour >= '00:00' & survey_hour < '05:59' ~ dplyr::sql("date_add(survey_date, interval -1 day)"),
        TRUE ~ survey_date
      )
    ) |>
    # discard survey date, just using survey_night now
    dplyr::select(-survey_date)

  # now get survey effort by forest and year
  forest_effort_df <-
    efforts_deployments_detections_sql_df |>
    dplyr::mutate(cell_unit = stringr::str_c(cell_id, unit_number, sep = '_')) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::left_join(cb_get_spatial('sierra_hexes') |> dplyr::select(cell_id, forest_name = ownership) |> sf::st_drop_geometry(), by = dplyr::join_by('cell_id')) |>
    dplyr::group_by(survey_year, forest_name) |>
    dplyr::summarise(
      survey_hours = dplyr::n(),
      cells = dplyr::n_distinct(cell_id),
      arus = dplyr::n_distinct(cell_unit)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(forest_name = forcats::fct_relevel(forest_name, forests_north_south)) |>
    dplyr::arrange(forest_name, survey_year) |>
    dplyr::select(forest_name, survey_year, arus, cells, survey_hours)

  # summarize that for entire sierra nevada
  sierra_effort_df <-
    forest_effort_df |>
    dplyr::group_by(survey_year) |>
    dplyr::summarise(
      arus = sum(arus),
      cells = sum(cells),
      survey_hours = sum(survey_hours)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(forest_name = 'All forests') |>
    dplyr::select(forest_name, survey_year, arus, cells, survey_hours)

  # combine in effort df
  effort_summary_df <-
    forest_effort_df |>
    dplyr::bind_rows(sierra_effort_df)

  # get detection summary by forest and year
  forest_detections_df <-
    efforts_deployments_detections_sql_df |>
    # had to add this here as filter not working on a sql table
    dplyr::collect() |>
    dplyr::filter(detection == 1) |>
    # dplyr::collect() |>
    dplyr::left_join(cb_get_spatial('sierra_hexes') |> dplyr::select(cell_id, forest_name = ownership) |> sf::st_drop_geometry(), by = dplyr::join_by('cell_id')) |>
    dplyr::group_by(survey_year, forest_name) |>
    dplyr::summarise(
      n_detections = sum(detection)
    ) |>
    dplyr::mutate(n_detections = as.integer(n_detections)) |>
    dplyr::ungroup() |>
    dplyr::mutate(forest_name = forcats::fct_relevel(forest_name, forests_north_south)) |>
    dplyr::arrange(forest_name, survey_year) |>
    dplyr::select(forest_name, survey_year, n_detections)

  # summarize this across entire sierra nevada
  sierra_detections_df <-
    forest_detections_df |>
    dplyr::group_by(survey_year) |>
    dplyr::summarise(
      n_detections = sum(n_detections)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(forest_name = 'All forests') |>
    dplyr::select(forest_name, survey_year, n_detections)

  # combine detection summaries
  detections_summary_df <-
    forest_detections_df |>
    dplyr::bind_rows(sierra_detections_df)

  # combine effort and detection summaries
  sampling_summary_df <-
    effort_summary_df |>
    dplyr::left_join(detections_summary_df, by = dplyr::join_by('forest_name', 'survey_year')) |>
    # clean up
    dplyr::mutate(forest_name = stringr::str_remove(forest_name, ' National Forest'))

  return(sampling_summary_df)

}
