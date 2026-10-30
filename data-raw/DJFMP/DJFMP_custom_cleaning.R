# Custom code to prepare `DJFMP` dataset

#' Main Custom Cleaning Engine: DJFMP
#'
#' Processes standardized input tables for DJFMP, performing survey-specific transformations,
#' parameter harmonization, quality control filtering, and dataset joining prior to final
#' pipeline compilation.
#'
#' @param ls_std_data A named list of standardized tibbles returned by `pipeline_standardize()`.
#'
#' @return A consolidated, cleaned tibble ready for final pipeline steps.
#' @noRd
transform_DJFMP_data <- function(ls_std_data) {
  # ---------------------------------------------------------------------------
  # 1. INPUT VALIDATION & EXTRACTION
  # ---------------------------------------------------------------------------

  # Validate input structure
  if (!is.list(ls_std_data) || length(ls_std_data) == 0) {
    cli::cli_abort("`ls_std_data` must be a non-empty list of data frames.")
  }

  # Define all required tables for this survey
  req_tables <- c("stations", "data_trawl_1976_2001", "data_trawl_2002_curr")

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
  df_stations <- ls_std_data[["stations"]]
  df_data_1976_2001 <- ls_std_data[["data_trawl_1976_2001"]]
  df_data_2002_curr <- ls_std_data[["data_trawl_2002_curr"]]

  # ---------------------------------------------------------------------------
  # 2. SURVEY-SPECIFIC TRANSFORMATIONS & MERGING
  # ---------------------------------------------------------------------------

  # Combine and finish cleaning data
  df_clean <-
    dplyr::bind_rows(df_data_1976_2001, df_data_2002_curr) |>
    # Remove conductivity data from dates before it was standardized >
    # Methods in metadata say they do not know if their data were corrected for temperature
    # before May 3 or 17 2019 so we will not use conductivity data before June 2019
    dplyr::mutate(
      Conductivity = dplyr::if_else(Date < "2019-06-01", NA_real_, Conductivity)
    ) |>
    # Remove duplicated rows
    dplyr::distinct(Station, Datetime, .keep_all = TRUE) |>
    # Add station coordinates
    dplyr::left_join(df_stations, by = dplyr::join_by(Station)) |>
    dplyr::arrange(Datetime)

  # Catch empty or corrupted data post-cleaning
  check_output_df(df_clean)

  cli::cli_alert_success("Custom cleaning complete")

  return(df_clean)
}
