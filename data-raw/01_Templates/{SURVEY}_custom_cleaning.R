# Custom code to prepare `{SURVEY}` dataset

#' Main Custom Cleaning Engine: {SURVEY}
#'
#' Processes standardized input tables for {SURVEY}, performing survey-specific transformations,
#' parameter harmonization, quality control filtering, and dataset joining prior to final
#' pipeline compilation.
#'
#' @param ls_std_data A named list of standardized tibbles returned by `pipeline_standardize()`.
#'
#' @return A consolidated, cleaned tibble ready for final pipeline steps.
#' @noRd
transform_{SURVEY}_data <- function(ls_std_data) {
  # ---------------------------------------------------------------------------
  # 1. INPUT VALIDATION & EXTRACTION
  # ---------------------------------------------------------------------------

  # Validate input structure
  if (!is.list(ls_std_data) || length(ls_std_data) == 0) {
    cli::cli_abort("`ls_std_data` must be a non-empty list of data frames.")
  }

  # Define all required tables for this survey
  req_tables <- c("data", "stations")

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
  df_data <- ls_std_data[["data"]]
  df_stations <- ls_std_data[["stations"]]

  # ---------------------------------------------------------------------------
  # 2. SURVEY-SPECIFIC TRANSFORMATIONS & MERGING
  # ---------------------------------------------------------------------------

  df_clean <- df_data #|>

  # Catch empty or corrupted data post-cleaning
  check_output_df(df_clean)

  cli::cli_alert_success("Custom cleaning complete")

  return(df_clean)
}
