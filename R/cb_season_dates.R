
#' Get start and end dates of primary sampling period
#'
#' @param study_type Sierra Monitoring, Sierra Projects, etc.
#' @param cell_ids List of cell IDs
#' @param start_year Minimum survey year
#' @param end_year Maximum survey year
#'
#' @return A data frame containing earliest/latest date of passive acoustic monitoring
#' @export
#'
#' @examples

cb_season_dates <- function(study_type, cell_ids, start_year, end_year) {

  study <- study_type

  # query acoustic field deployments table
  deployments <-
    conn |>
    tbl('acoustic_field_visits') |>
    select(id:unit_number)

  # query acoustic efforts table
  efforts <-
    conn |>
    tbl('acoustic_efforts') |>
    select(id:survey_hour) |>
    rename(effort_id = id)

  # combine
  efforts_deployments <-
    efforts |>
    # deployment acoustic_field_visit_id = effort id; rename to join
    left_join(deployments |> rename(acoustic_field_visit_id = id), by = join_by('acoustic_field_visit_id')) |>
    filter(
      cell_id %in% cell_ids,
      study_type %in% study,
      survey_year >= start_year,
      survey_year <= end_year,
      # keep efforts from 8:00PM to 6:00AM (for owls)
      survey_hour >= '20:00' & survey_hour < '23:59' | survey_hour >= '00:00' & survey_hour < '05:59'
    ) |>
    # create a field for survey night (i.e., biologically-relevant night for an owl)
    mutate(
      survey_night = case_when(
        survey_hour >= '00:00' & survey_hour < '05:59' ~ sql("date_add(survey_date, interval -1 day)"),
        TRUE ~ survey_date
      )
    ) |>
    # discard survey date, just using survey_night now
    select(-survey_date) |>
    # find min/max date by year
    group_by(survey_year) |>
    summarise(
      min_date = min(survey_night),
      max_date = max(survey_night)
    ) |>
    # read into memory
    collect()

  # pick the earliest date and the latest date, regardless of year
  efforts_deployments <-
    efforts_deployments |>
    pivot_longer(
      cols = !survey_year,
      names_to = 'date_type',
      values_to = 'date'
    ) |>
    mutate(jday = yday(date)) |>
    filter(jday == min(jday) | jday == max(jday)) |>
    select(-jday)

  return(efforts_deployments)

}
