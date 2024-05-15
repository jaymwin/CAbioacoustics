
#' Query spotted owl database to format data for occupancy modeling (static, single-season or dynamic models)
#'
#' @param species Vector of species (CSOW or BDOW)
#' @param template BirdNET version used
#' @param study_type Vector of study types (e.g., Sierra Monitoring, Sierra Projects)
#' @param cell_ids Vector cell IDs
#' @param start_year Start year
#' @param end_year End year
#' @param start_date Start date of sampling
#' @param end_date End date of sampling
#' @param occasion_length Length (in days) of secondary sampling occasions
#' @param num_occasions Number of secondary sampling occasions
#' @param type Occupancy modeling framework, either single-season or dynamic
#'
#' @return A list of data frames
#' @export
#'
#' @examples

cb_query_effort_detections_occ <- function(species, template, study_type, cell_ids, start_year, end_year, start_date, end_date, occasion_length, num_occasions, type) {

  focal_species <- species
  template_used <- template

  # query acoustic field deployments table
  deployments_sql_df <-
    conn |>
    tbl('acoustic_field_visits') |>
    select(id:unit_number)

  # query acoustic efforts table
  efforts_sql_df <-
    conn |>
    tbl('acoustic_efforts') |>
    select(id:survey_hour) |>
    rename(effort_id = id)

  # query acoustic detections table
  detections_sql_df <-
    conn |>
    tbl('acoustic_detections') |>
    filter(species %in% focal_species & template_used == template) |>
    select(acoustic_effort_id, begin_file, template_used) |>
    # use distinct survey hours; can have both MAN or BirdNET classifications for a single survey hour (which inflates survey effort)
    # only need a single detection per survey hour
    distinct() |>
    mutate(detection = 1)

  # sql dataframe with efforts, their deployment info, and any detections for that particular survey hour
  efforts_deployments_detections_sql_df <-
    efforts_sql_df |>
    # deployment acoustic_field_visit_id = effort id; rename to join
    left_join(deployments_sql_df |> rename(acoustic_field_visit_id = id), by = join_by('acoustic_field_visit_id')) |>
    # detections effort id = effort id
    left_join(detections_sql_df |> rename(effort_id = acoustic_effort_id), by = join_by('effort_id')) |>
    # clean up
    select(survey_date:unit_number, detection)

  # keep relevant data for owls
  efforts_deployments_detections_sql_df <-
    efforts_deployments_detections_sql_df |>
    # make survey year an integer
    # mutate(survey_year = sql("cast(survey_year as signed)")) |>
    # filter to study type, years, and appropriate survey hours for owls; usfs cells only
    filter(
      cell_id %in% cell_ids,
      study_type == study,
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
    select(-survey_date)

  efforts_df <-
    efforts_deployments_detections_sql_df |>
    # now tally (count number of hours) per deployment, year, and survey night
    group_by(cell_id, unit_number, survey_year, survey_night) |>
    tally(name = 'survey_hours') |>
    # read into memory now
    collect() |>
    # convert these to integers from integer64
    mutate(
      # survey_year = as.integer(survey_year),
      survey_hours = as.integer(survey_hours)
    )

  detections_df <-
    efforts_deployments_detections_sql_df |>
    group_by(study_type, project_area, group_id, survey_year, cell_id, unit_number, survey_night) |>
    # calculate the maximum count for each survey night
    summarize(detection = max(detection, na.rm = TRUE)) |>
    suppressWarnings() |>
    # NAs here are 0 detection nights
    mutate(
      detection = case_when(
        is.na(detection) ~ 0,
        TRUE ~ detection
      )
    ) |>
    collect()

  efforts_detections_df <-
    efforts_df |>
    left_join(detections_df, by = join_by('cell_id', 'unit_number', 'survey_year', 'survey_night')) |>
    # create a cell_unit field; fix empty project area
    mutate(
      cell_unit = str_c(cell_id, '_', unit_number),
      project_area = case_when(
        project_area == "" ~ as.character(NA),
        TRUE ~ project_area
      )
    ) |>
    # organize
    select(
      group_id, cell_id, unit_number, cell_unit, study_type, project_area,
      survey_year, survey_night, survey_hours, detection
    )

  # fill in missing dates with complete_survey_nights(); do this in parallel with furrr/future
  future::plan(future::multisession, workers = parallelly::availableCores() - 1)
  efforts_detections_df <-
    efforts_detections_df |>
    # complete these by splitting into cell units and year
    group_split(cell_unit, survey_year) |>
    # fill in missing dates from season start to end date
    furrr::future_map_dfr(\(df) CAbioacoustics:::complete_survey_nights(df, start_date, end_date))

  occ_dates_df <-
    CAbioacoustics:::create_season_dates(
      start_year = start_year,
      end_year = end_year,
      start_date = start_date,
      end_date = end_date
    ) |>
    group_by(survey_year) |>
    # subtract 1 so first day is 0
    mutate(day_of_season = row_number() - 1) |>
    # split season into equal intervals (1-day, 3-day, ...1 week, etc)
    # ntile() assigns each day into a sampling occasion
    mutate(
      occasion = ntile(day_of_season, num_occasions)
    ) |>
    ungroup()

  # assign secondary sampling occasion to each date
  efforts_detections_df <-
    efforts_detections_df |>
    left_join(occ_dates_df, by = join_by('survey_year', 'survey_night'))

  # 1) summarize mean date over sampling occasion (days since start date)
  dates_df <-
    occ_dates_df |>
    group_by(survey_year, occasion) |>
    summarize(
      mean_date = mean(day_of_season)
    ) |>
    suppressMessages() |>
    ungroup()

  # 2) summarize maximum number of ARUs operational over a sampling occasion
  active_arus_df <-
    efforts_detections_df |>
    filter(survey_hours > 0) |>
    group_by(cell_id, survey_year, survey_night) |>
    tally(name = 'arus') |>
    left_join(occ_dates_df |> select(-day_of_season), by = join_by('survey_year', 'survey_night')) |>
    group_by(cell_id, survey_year, occasion) |>
    summarize(
      arus = max(arus)
    ) |>
    suppressMessages() |>
    ungroup()

  # 3) summarize total survey hours over a sampling occasion
  survey_hours_df <-
    efforts_detections_df |>
    group_by(cell_id, survey_year, occasion) |>
    summarize(
      survey_hours = sum(survey_hours)
    ) |>
    suppressMessages() |>
    ungroup() |>
    # 0s here are occasions where there were no arus deployed
    mutate(
      survey_hours = case_when(
        survey_hours == 0 ~ as.numeric(NA),
        TRUE ~ survey_hours
      )
    )

  # 4) detection criteria per primary occasion
  detection_critera_df <-
    efforts_detections_df |>
    # just focus on any efforts with detections
    filter(detection == 1) |>
    distinct(cell_id, survey_year, survey_night, detection) |>
    # left_join(season_dates_df |> select(-day_of_season)) |>
    group_by(cell_id, survey_year) |>
    tally(name = 'detection_nights') |>
    ungroup() |>
    mutate(
      # add detection criteria
      detection_criteria = case_when(
        detection_nights == 1 ~ '1-day',
        detection_nights > 1 ~ '2-day'
      )
    )

  # now combine with detections and summarise by cell, survey year, and occasion
  encounter_histories_long_df <-
    efforts_detections_df |>
    group_by(cell_id, survey_year, occasion) |>
    # 0 or 1 per sampling occasion
    summarize(
      detection = max(detection, na.rm = TRUE)
    ) |>
    suppressWarnings() |>
    suppressMessages() |>
    ungroup() |>
    # -Inf from summarising just means there was no effort during sampling occasion;
    # change to NA
    mutate(
      detection = case_when(
        detection == -Inf ~ as.numeric(NA),
        TRUE ~ detection
      )
    ) |>
    # join in various effort data frames
    left_join(detection_critera_df, by = join_by('cell_id', 'survey_year')) |>
    left_join(active_arus_df, by = join_by('cell_id', 'survey_year', 'occasion')) |>
    left_join(survey_hours_df, by = join_by('cell_id', 'survey_year', 'occasion')) |>
    left_join(dates_df, by = join_by('survey_year', 'occasion')) |>
    # organize
    select(cell_id, survey_year, occasion, survey_hours, detection_nights, detection_criteria, arus, mean_date, detection)

  # create two different columns: strict and liberal detection
  # if there's only 1 night of detections, convert that encounter to a 0 instead of 1
  # otherwise keep as is
  encounter_histories_long_df <-
    encounter_histories_long_df |>
    # strict detection
    mutate(
      detection_strict =
        case_when(
          detection == 1 & detection_criteria == '1-day' ~ 0,
          TRUE ~ detection
        )
    ) |>
    # liberal detection
    rename(detection_lib = detection)

  # calculate naive occupancy (across all occasions per site) while at it
  encounter_histories_long_df <-
    encounter_histories_long_df |>
    group_by(cell_id, survey_year) |>
    mutate(detection_naive = max(detection_lib, na.rm = TRUE))

  encounter_histories_long_df <-
    encounter_histories_long_df |>
    # just better organize data
    select(cell_id, survey_year, occasion, survey_hours, arus, mean_date, matches('detection_')) |>
    ungroup()

  encounter_histories_long_df <-
    encounter_histories_long_df |>
    mutate(
      mean_date = case_when(
        is.na(survey_hours) ~ as.numeric(NA),
        TRUE ~ mean_date
      )
    )

  # liberal first
  # convert to wide format
  encounter_histories_lib_wide_df <-
    encounter_histories_long_df |>
    select(
      cell_id,
      survey_year,
      occasion,
      detection = detection_lib
    ) |>
    pivot_wider(
      id_cols = c(cell_id, survey_year),
      names_from = occasion,
      names_glue = "{.value}_{occasion}",
      values_from = c(detection)
    )

  # strict
  # convert to wide format
  encounter_histories_strict_wide_df <-
    encounter_histories_long_df |>
    select(
      cell_id,
      survey_year,
      occasion,
      detection = detection_strict
    ) |>
    pivot_wider(
      id_cols = c(cell_id, survey_year),
      names_from = occasion,
      names_glue = "{.value}_{occasion}",
      values_from = c(detection)
    )

  # effort covariates
  effort_wide_df <-
    encounter_histories_long_df |>
    select(
      cell_id,
      survey_year,
      occasion,
      survey_hours,
      arus,
      mean_date
    ) |>
    mutate(
      across(c(survey_hours, arus), \(x) na_if(x, 0)),
      mean_date = case_when(
        is.na(survey_hours) ~ NA,
        TRUE ~ mean_date
      )
    ) |>
    pivot_longer(
      !c(cell_id, survey_year, occasion),
      names_to = "var",
      values_to = "value"
    ) |>
    pivot_wider(
      id_cols = c(cell_id, survey_year),
      names_from = c(var, occasion),
      names_glue = "{var}_{occasion}",
      values_from = c(value)
    ) |>
    select(cell_id, survey_year, matches('survey_hours'), matches('mean_date'), matches('arus'))

  occ_df <-
    list(
      encounters_long = encounter_histories_long_df,
      encounters_wide_lib = encounter_histories_lib_wide_df,
      encounters_wide_strict = encounter_histories_strict_wide_df,
      effort = effort_wide_df
    )

  if (type == 'static') {

    return(occ_df)

  }

  if (type == 'dynamic') {

    encounters_dynamic_df <-
      encounter_histories_long_df |>
      select(cell_id, survey_year, occasion, detection_strict) |>
      pivot_wider(
        id_cols = c('cell_id'),
        names_from = c(survey_year, occasion),
        names_glue = "{'det'}_{survey_year}_{occasion}",
        values_from = c(detection_strict)
      )

    effort_dynamic_df <-
      encounter_histories_long_df |>
      select(cell_id, survey_year, occasion, survey_hours, arus, mean_date) |>
      pivot_longer(
        !c(cell_id, survey_year, occasion),
        names_to = 'variable',
        values_to = 'value'
      ) |>
      pivot_wider(
        id_cols = c('cell_id'),
        names_from = c(variable, survey_year, occasion),
        names_glue = "{variable}_{survey_year}_{occasion}",
        values_from = c(value)
      )

    occ_df <-
      list(
        encounters_long = encounter_histories_long_df,
        encounters_wide_strict = encounters_dynamic_df,
        effort = effort_dynamic_df
      )

    return(occ_df)

  }

}
