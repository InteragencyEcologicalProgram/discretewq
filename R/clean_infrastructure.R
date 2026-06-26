# General data cleaning functions to be used across surveys

#' Standardize a Single Raw Input Data Frame to YAML Schema Specifications
#'
#' Orchestrates the structural and environmental parameter standardization workflow for
#' a single data input. The function handles optional column schema bypasses, renames and
#' validates columns, repairs raw Excel date/time artifacts, forces standardized Pacific
#' timezones, transforms specialized metrics (Depth and Secchi), and reconciles long-format
#' database parameters against configuration definitions.
#'
#' @param df_data A data frame or tibble containing raw unstandardized survey inputs.
#' @param input_cfg A list representing a parsed `inputs` block from a single input in the YAML
#'   configuration file.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#'
#' @return A structurally standardized data frame or tibble conforming to the package
#'   baseline schema.
#' @noRd
standardize_single_input <- function(df_data, input_cfg, config_path) {
  input_id <- input_cfg$input_id
  cli::cli_h3("Standardizing input: {.val {input_id}}")

  # Check if input_cfg has column configurations specified, and if it doesn't return df_data
  # without further standardization and provide message
  if (!"columns" %in% names(input_cfg)) {
    cli::cli_bullets(c(
      "v" = "Skipping standardization for {.val {input_id}}",
      "i" = "No column configurations provided",
      "!" = "If this is unexpected, modify configurations in {.path {config_path}} and re-run survey pipeline"
    ))
    return(df_data)
  }

  # Verify expected columns in raw data
  check_expected_cols(df_data, input_cfg$columns, config_path)

  # Rename and convert columns, while checking for parsing errors
  df_clean <- standardize_columns(df_data, input_cfg$columns, config_path)

  # Define colnames of df_std_cols for optional cleaning steps below
  cols <- colnames(df_clean)

  # Convert any numeric dates and times
  # Dates and times are converted to numeric values when importing Excel files with
  # text column type
  if ("Date" %in% cols && is.numeric(df_clean$Date)) {
    df_clean$Date <- convert_numeric_date(df_clean$Date)
  }
  if ("Time" %in% cols && is.numeric(df_clean$Time)) {
    df_clean$Time <- convert_numeric_time(df_clean$Time)
  }

  # Combine Date and Time columns into Datetime column if necessary
  if ("Date" %in% cols && "Time" %in% cols) {
    df_clean$Datetime <- combine_datetime(df_clean$Date, df_clean$Time)
  }

  # Standardize the timezone for a Datetime column to local time (America/Los_Angeles)
  if ("Datetime" %in% colnames(df_clean)) {
    df_clean$Datetime <- standardize_timezone(
      df_clean$Datetime,
      input_cfg$timezone,
      config_path
    )
  }

  # Set up extract_config_info function with partial arguments defined for extracting col_units
  extract_col_units <- purrr::partial(
    extract_config_info,
    cfg_object = input_cfg$columns,
    target_key = "target_name",
    extract_key = "col_units"
  )

  # Standardize units for Depth and Secchi columns
  if ("Depth" %in% cols) {
    depth_unit <- extract_col_units(target_val = "Depth")
    df_clean$Depth <- convert_depth(df_clean$Depth, depth_unit, config_path)
  }

  if ("Secchi" %in% cols) {
    secchi_unit <- extract_col_units(target_val = "Secchi")
    df_clean$Secchi <- convert_secchi(df_clean$Secchi, secchi_unit, config_path)
  }

  # Perform parameter standardization steps for input if it's in long format
  if ("parameters" %in% names(input_cfg)) {
    check_expected_params(df_clean, input_cfg$parameters, config_path)
    df_clean <- standardize_parameters(
      df_clean,
      input_cfg$parameters,
      config_path
    )
  }

  # continue cleaning steps here...
  # Standardize tide codes, lat-long operations?

  # Catch empty or corrupted data post-standardization
  check_output_df(df_clean)

  cli::cli_alert_success("Successfully standardized {.val {input_id}}")

  return(df_clean)
}

#' Dynamically Retrieve and Validate Custom Survey Cleaners
#'
#' @param acronym A string match representing the configuration source acronym.
#' @return A validated closure function specific to the requested survey.
#' @noRd
get_custom_cleaner <- function(acronym) {
  # Construct paths using established internal structure conventions
  cleaner_dir <- file.path("data-raw", acronym)
  cleaner_file <- file.path(cleaner_dir, paste0(acronym, "_custom_cleaning.R"))
  target_fn_name <- paste0("transform_", acronym, "_data")

  # Ensure the script file actually exists before sourcing
  if (!file.exists(cleaner_file)) {
    cli::cli_abort(c(
      "x" = "Custom cleaning function missing for survey {.val {acronym}}",
      "i" = "Expected to locate a custom cleaning script at: {.path {cleaner_file}}",
      "!" = "Please create this directory and script file to process this survey."
    ))
  }

  cli::cli_alert_info(
    "Loading custom cleaning script at: {.path {cleaner_file}}"
  )

  # Create a clean, isolated environment sandbox for sourcing the script while giving
  # full access to internal package tools from discretewq
  pkg_env <- asNamespace("discretewq")
  sandbox_env <- new.env(parent = pkg_env)

  # Catch syntax errors or runtime crashes inside the custom cleaning script
  rlang::try_fetch(
    expr = {
      sys.source(cleaner_file, envir = sandbox_env)
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "x" = "Failed to compile custom cleaning script for {.val {acronym}}:",
          "!" = "{cnd$message}",
          "i" = "Check for syntax errors or missing library qualifications inside {.path {cleaner_file}}"
        ),
        parent = cnd
      )
    }
  )

  # Ensure the expected function exists inside the script
  if (!exists(target_fn_name, envir = sandbox_env, mode = "function")) {
    cli::cli_abort(c(
      "x" = "Target cleaning function mismatch in {.path {cleaner_file}}",
      "i" = "The script compiled successfully but is missing the required function: {.fn {target_fn_name}}",
      "!" = "Ensure the main function in the custom cleaning script matches the survey signature exactly."
    ))
  }

  # Extract and return the function object safely
  get(target_fn_name, envir = sandbox_env)
}

#' Remove Rows Containing All Missing Water Quality Measurements
#'
#' @param df_data A data frame or tibble to clean.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#' @return A filtered data frame or tibble.
#' @noRd
rm_rows_all_miss_data <- function(df_data, config_path) {
  # Set up progress message
  status <- new.env()
  status$msg <- ""
  step_id <- cli::cli_progress_step(
    msg = "Removing rows containing 100% missing measurement data",
    msg_done = "Removing rows containing 100% missing measurement data{status$msg}"
  )

  # ASSERTION GATE: Identify which target measurement columns are actually present
  # ALL_MEAS_COLS defined in data-raw/internal_metadata.R
  present_meas <- intersect(ALL_MEAS_COLS, colnames(df_data))

  # Error if the data frame contains zero measurement columns
  if (length(present_meas) == 0) {
    cli::cli_abort(c(
      "x" = "Data frame schema verification failed in {.fn rm_rows_all_miss_data}",
      "!" = "The dataset does not contain any valid measurement columns",
      "i" = "Expected to find at least one column from {.var ALL_MEAS_COLS} in {.path data-raw/internal_metadata.R}",
      "*" = "Current data frame columns: {.val {colnames(df_data)}}",
      "!" = "Check configuration settings in {.path {config_path}}"
    ))
  }

  # Helper function to capture both true NAs and empty string placeholders
  is_blank_or_na <- function(x) {
    if (is.character(x)) {
      return(is.na(x) | trimws(x) == "")
    } else {
      return(is.na(x))
    }
  }

  # Execute the filter across safely identified columns
  df_clean <- df_data |>
    dplyr::filter(
      !dplyr::if_all(tidyselect::all_of(present_meas), is_blank_or_na)
    )

  # Catch empty or corrupted data post-cleaning
  check_output_df(df_clean)

  # Report out pipeline processing metrics
  initial_rows <- nrow(df_data)
  final_rows <- nrow(df_clean)
  dropped_rows <- initial_rows - final_rows

  status$msg <- cli::format_inline(
    "... {cli::no(dropped_rows)} row{?s} removed"
  )
  cli::cli_progress_done(id = step_id)

  return(df_clean)
}

#' Add Source column for Survey
#'
#' @param df_data A data frame or tibble to add column to.
#' @param survey A string to use to identify the survey in the `Source` column
#' @noRd
add_source_col <- function(df_data, survey) {
  cli::cli_progress_step("Adding {.var Source} column for {.val {survey}}")
  df_clean <- df_data |> dplyr::mutate(Source = survey, .before = 1)

  # Catch empty or corrupted data post-cleaning
  check_output_df(df_clean)

  return(df_clean)
}

#' Standardize Column Order and Drop Unnecessary Metadata
#'
#' @param df_data A data frame or tibble containing processed water quality data.
#' @return A data frame with a standardized schema and arranged columns.
#' @noRd
standardize_col_order <- function(df_data) {
  step_id <- cli::cli_progress_step(
    "Standardizing final data frame column schema"
  )

  # Target Selection: Subset and order only valid schema columns
  # ALL_COLS_ORDER defined in data-raw/internal_metadata.R
  df_clean <- df_data |> dplyr::select(tidyselect::any_of(ALL_COLS_ORDER))

  # Catch empty or corrupted data post-cleaning
  check_output_df(df_clean)

  # Complete the progress line with summary statistics
  cli::cli_progress_done(id = step_id)

  # Find out if the incoming data contains columns unspecified in ALL_COLS_ORDER
  incoming_cols <- colnames(df_data)
  unmapped_cols <- setdiff(incoming_cols, ALL_COLS_ORDER)
  n_drop <- length(unmapped_cols)

  # If unexpected data parameters are present, alert the user with an info log
  # This prevents quiet data loss during annual updates
  if (n_drop > 0) {
    cli::cli_bullets(c(
      "!" = "Dropping {n_drop} column{?s} from the dataset: {.val {unmapped_cols}}",
      "i" = "If {cli::qty(n_drop)}{?this/these} column{?s} should be retained, add {?it/them} to {.var ALL_COLS_ORDER} in {.path data-raw/internal_metadata.R}"
    ))
  }

  n_rows <- nrow(df_clean)
  n_rows_pretty <- format(n_rows, big.mark = ',', scientific = FALSE)
  cli::cli_alert_success(
    "Finalized schema with {n_rows_pretty} {cli::qty(n_rows)}row{?s} and {ncol(df_clean)} column{?s}"
  )

  return(df_clean)
}
