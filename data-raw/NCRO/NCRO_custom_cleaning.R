# Custom code to prepare `NCRO` dataset

# Function to clean the NCRO field data
clean_ncro_field_data <- function(df_data) {
  # Pivot field data wider
  df_data |>
    dplyr::select(-units) |>
    tidyr::pivot_wider(names_from = parameter, values_from = result)
}

# Function to clean the NCRO lab data
clean_ncro_lab_data <- function(df_data) {
  # Add Sign variable which indicates <RL values, and convert Result to numeric making <RL values
  # equal to their RL
  df_data_c <- df_data |>
    dplyr::select(-units) |>
    dplyr::mutate(
      Sign = dplyr::if_else(stringr::str_detect(result, "^<"), "<", "="),
      result = as.numeric(dplyr::if_else(Sign == "<", reporting_limit, result)),
      .keep = "unused"
    )

  # Before restructuring lab data to wide format, remove laboratory duplicates by removing one at
  # random
  # Define grouping variables for each unique duplicate pair
  grp_lab_dups <- c("station_number", "sample_code", "Datetime", "parameter")

  df_data_lab_dups <- df_data_c |>
    dplyr::add_count(dplyr::pick(tidyselect::all_of(grp_lab_dups))) |>
    dplyr::filter(n > 1) |>
    dplyr::select(-n) |>
    dplyr::slice_sample(n = 1, by = tidyselect::all_of(grp_lab_dups))

  df_data_c |>
    dplyr::anti_join(
      df_data_lab_dups |> dplyr::select(tidyselect::all_of(grp_lab_dups)),
      by = grp_lab_dups
    ) |>
    dplyr::bind_rows(df_data_lab_dups) |>
    tidyr::pivot_wider(
      names_from = parameter,
      values_from = c(result, Sign),
      names_glue = "{parameter}_{.value}"
    ) |>
    # Clean up names for the columns with results
    dplyr::rename_with(\(x) stringr::str_remove(x, "_result$"))
}

# Function to clean NCRO Secchi depth and Microcystis data
clean_ncro_mvi_secchi_data <- function(df_data, df_stations) {
  df_data |>
    # Use the numeric codes for Microcystis
    dplyr::mutate(
      Microcystis = dplyr::recode_values(
        Microcystis,
        c("Not Visible", "Absent") ~ 1,
        "Low" ~ 2,
        "Medium" ~ 3,
        "High" ~ 4,
        "Extreme" ~ 5,
        c(NA, "Unknown") ~ NA_real_,
        unmatched = "error"
      )
    ) |>
    # Standardize the Paradise Cut upstream station to PDU (code is PDUP before 2024)
    dplyr::mutate(
      station_code = dplyr::if_else(station_code == "PDUP", "PDU", station_code)
    ) |>
    # Join standardized station numbers
    dplyr::left_join(
      df_stations |> dplyr::select(station_number, wqes_station_code),
      by = dplyr::join_by(station_code == wqes_station_code)
    ) |>
    # Remove Stations and Dates that are NA and records without Secchi and Microcystis values
    tidyr::drop_na(station_number, Date) |>
    dplyr::filter(!dplyr::if_all(c(Secchi, Microcystis), is.na)) |>
    dplyr::select(station_number, Date, Secchi, Microcystis) |>
    # Remove a few duplicated records
    dplyr::distinct()
}

#' Main Custom Cleaning Engine: NCRO
#'
#' Processes standardized input tables for NCRO, performing survey-specific transformations,
#' parameter harmonization, quality control filtering, and dataset joining prior to final
#' pipeline compilation.
#'
#' @param ls_std_data A named list of standardized tibbles returned by `pipeline_standardize()`.
#'
#' @return A consolidated, cleaned tibble ready for final pipeline steps.
#' @noRd
transform_NCRO_data <- function(ls_std_data) {
  # ---------------------------------------------------------------------------
  # 1. INPUT VALIDATION & EXTRACTION
  # ---------------------------------------------------------------------------

  # Validate input structure
  if (!is.list(ls_std_data) || length(ls_std_data) == 0) {
    cli::cli_abort("`ls_std_data` must be a non-empty list of data frames.")
  }

  # Define all required tables for this survey
  req_tables <- c(
    "data_1999_2022",
    "stations",
    "data_mvi_secchi",
    "data_lab",
    "data_field"
  )

  # Identify any missing tables
  missing_tables <- purrr::keep(
    req_tables,
    \(x) is.null(purrr::pluck(ls_std_data, x))
  )

  # Abort with a formatted list if any are missing
  if (length(missing_tables) > 0) {
    cli::cli_abort(c(
      "Required input table{?s} missing from standardized input list:",
      "x" = "Missing: {.val {missing_tables}}"
    ))
  }

  # Extract individual standardized tables safely using exact string indexing
  df_data_1999_2022 <- ls_std_data[["data_1999_2022"]]
  df_stations <- ls_std_data[["stations"]]
  df_data_mvi_secchi <- ls_std_data[["data_mvi_secchi"]]
  df_data_lab <- ls_std_data[["data_lab"]]
  df_data_field <- ls_std_data[["data_field"]]

  # ---------------------------------------------------------------------------
  # 2. SURVEY-SPECIFIC TRANSFORMATIONS & MERGING
  # ---------------------------------------------------------------------------

  # Apply specific cleaning functions
  df_data_mvi_secchi <- clean_ncro_mvi_secchi_data(
    df_data_mvi_secchi,
    df_stations
  )
  df_data_lab <- clean_ncro_lab_data(df_data_lab)
  df_data_field <- clean_ncro_field_data(df_data_field)

  # Combine field and laboratory data
  df_data_curr <-
    dplyr::full_join(
      df_data_field,
      df_data_lab,
      by = dplyr::join_by(station_number, sample_code, Datetime)
    ) |>
    # Remove samples not collected by NCRO at YB below Lisbon station (Sample Code prefix 'ES')
    dplyr::filter(!stringr::str_detect(sample_code, "^ES")) |>
    # Add Date column
    dplyr::mutate(Date = lubridate::date(Datetime), .before = Datetime) |>
    # Add Secchi depth and Microcystis data - there a few records without matches in the field/lab
    # data, but we'll use a left join for now
    dplyr::left_join(
      df_data_mvi_secchi,
      by = dplyr::join_by(station_number, Date)
    ) |>
    # Add standardized station names and lat-long coordinates
    dplyr::left_join(
      df_stations |>
        dplyr::select(station_number, Station, Latitude, Longitude),
      by = dplyr::join_by(station_number)
    ) |>
    # Fill in "=" for NA values within the _Sign variables
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with("_Sign"),
        \(x) dplyr::if_else(is.na(x), "=", x)
      )
    )

  # Add current data to 1999-2022 data and finish cleaning
  df_clean <-
    dplyr::bind_rows(df_data_1999_2022, df_data_curr) |>
    # Remove overlapping data or duplicates
    dplyr::distinct(Station, Datetime, .keep_all = TRUE) |>
    dplyr::arrange(Datetime)

  # Catch empty or corrupted data post-cleaning
  check_output_df(df_clean)

  cli::cli_alert_success("Custom cleaning complete")

  return(df_clean)
}
