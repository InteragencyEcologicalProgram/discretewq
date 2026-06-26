# Functions for data validation and function assertions

#' Internal Data Frame Structural Assertion Guard
#'
#' @param object The object to evaluate (at the end of a function)
#' @return NULL invisibly if valid; produces a structured rlang error otherwise
#' @noRd
check_output_df <- function(object) {
  # Capture the name of the function that generated this output
  # Using caller_env(2) lets us find the actual name of the pipeline step
  calling_fn_expr <- sys.call(-1)
  calling_fn_name <- if (!is.null(calling_fn_expr)) {
    as.character(calling_fn_expr[[1]])
  } else {
    "unknown function"
  }

  # Ensure the function didn't accidentally return a list, vector, or NULL
  if (!is.data.frame(object)) {
    # Dynamically determine what type of bad object was actually passed and build a
    # friendly type description
    friendly_type <- paste0(
      "an object of class [",
      class(object)[1],
      "] (base type: ",
      typeof(object),
      ")"
    )

    cli::cli_abort(
      c(
        "x" = "Function {.fn {calling_fn_name}} returned an invalid data type",
        "!" = "Expected a {.cls data.frame} or {.cls tibble} as the final output",
        "i" = "Instead, the function generated {friendly_type}"
      ),
      # Points the error to the function *calling* the check
      call = rlang::caller_env()
    )
  }

  # Emptiness Check: Catch functions that accidentally filtered away 100% of the data
  if (nrow(object) == 0 || ncol(object) == 0) {
    cli::cli_abort(
      c(
        "x" = "Function {.fn {calling_fn_name}} returned an empty dataset",
        "!" = "The returned data frame must contain active row observations and columns",
        "i" = "Current output dimensions: {nrow(object)} row{?s} and {ncol(object)} column{?s}",
        "i" = "This usually means a filter step or an inner join dropped all records accidentally"
      ),
      # Keeps the stack trace clean and informative
      call = rlang::caller_env()
    )
  }

  invisible(TRUE)
}

#' Validate Essential Input Structure and Schema Definitions
#'
#' @param input_cfg A list representing a parsed `inputs` block from a single input in the YAML
#'   configuration file.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#'
#' @return NULL invisibly if valid; produces an error otherwise
#' @noRd
check_config_structure <- function(input_cfg, config_path) {
  input_id <- input_cfg$input_id
  input_type <- input_cfg$input_type
  columns <- input_cfg$columns

  # Verify input_id exists and is not blank
  if (is.null(input_id) || !nzchar(input_id)) {
    cli::cli_abort(c(
      "x" = "Malformed entry detected in {.path {config_path}}: Missing mandatory {.field input_id}",
      "!" = "Every data input under {.field inputs} must declare a unique identifier string"
    ))
  }

  # Verify input_type exists and is not blank
  if (is.null(input_type) || !nzchar(input_type)) {
    input_types <- c("API-EDI", "API-CNRA-lab", "API-CNRA-field", "local_file")
    cli::cli_abort(c(
      "x" = "Input {.val {input_id}} is missing an {.field input_type} definition",
      "i" = "Supported types include: {.val {input_types}}"
    ))
  }

  # Determine if columns are structurally required for this input
  # Default to TRUE if the parameter isn't explicitly defined in the YAML
  is_col_required <- purrr::pluck(input_cfg, "columns_required", .default = TRUE)

  # Verify columns array exists if required
  if (isTRUE(is_col_required)) {
    if (is.null(columns) || length(columns) == 0) {
      cli::cli_abort(c(
        "x" = "Input {.val {input_id}} has an empty or missing {.field columns} array",
        "i" = "If this data source is a raw pass-through that does not require column standardization, add {.code columns_required: false} to its YAML entry"
      ))
    }
  }

  invisible(NULL)
}

#' Validate Presence of Expected Raw Columns (Case-Insensitive)
#'
#' @param df_data A data frame or tibble to check.
#' @param columns_cfg A list representing a parsed `columns` block from a single input in the
#'   YAML configuration file.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#'
#' @return NULL invisibly if valid; produces an error otherwise
#' @noRd
check_expected_cols <- function(df_data, columns_cfg, config_path) {
  cli::cli_progress_step("Checking expected columns")

  # Extract expected names
  expected_raw_cols <- purrr::map_chr(columns_cfg, "raw_name")

  # Find which expected columns do not exist in the lowercase column pool of df_data
  missing_cols <- expected_raw_cols[
    !tolower(expected_raw_cols) %in% tolower(colnames(df_data))
  ]

  # Generate message and error if there are any missing columns
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "Missing expected raw columns in input",
      "!" = "Missing columns: {.val {missing_cols}}",
      "!" = "Check column names in raw data ({.var workflow_data}) to make sure they match {.field raw_name} for input in {.path {config_path}}"
    ))
  }

  invisible(NULL)
}

#' Validate Presence of Expected Parameters (Case-Insensitive)
#'
#' @param df_data A data frame or tibble to check.
#' @param parameters_cfg A list representing a parsed `parameters` block from a single input in
#'   the YAML configuration file.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#'
#' @return NULL invisibly if valid; produces an error otherwise
#' @noRd
check_expected_params <- function(df_data, parameters_cfg, config_path) {
  cli::cli_progress_step("Checking expected parameters and units")

  # Define parameter and units column names in df_data from parameters_cfg
  parameter_col <- parameters_cfg$parameter_col
  units_col <- parameters_cfg$units_col

  # Ensure the columns specified in the config actually exist in the data frame
  if (!all(c(parameter_col, units_col) %in% names(df_data))) {
    cli::cli_abort(c(
      "x" = "Validation configuration error for input",
      "!" = "The defined parameter column {.var {parameter_col}} or units column {.var {units_col}} could not be located in the raw data",
      "!" = "Check column names in raw data ({.var workflow_data}) to make sure they match {.field parameter_col} and {.field units_col} for input in {.path {config_path}}"
    ))
  }

  # Extract raw_name and raw_units for each expected parameter from the YAML config file
  expected_raw_params <-
    purrr::map(
      parameters_cfg$definitions,
      function(param) {
        raw_units <- purrr::pluck(param, "raw_units", .default = NA_character_)
        if (!is.na(raw_units) && trimws(raw_units) == "") {
          raw_units <- NA_character_
        }
        tibble::tibble(
          raw_name = purrr::pluck(param, "raw_name", .default = NA_character_),
          raw_units = raw_units
        )
      }
    ) |>
    purrr::list_rbind() |>
    tidyr::drop_na("raw_name")

  # Find distinct parameter-unit pairs in df_data, forcing clean lowercase matching keys
  df_data_params <- df_data |>
    dplyr::distinct(.data[[parameter_col]], .data[[units_col]]) |>
    dplyr::mutate(
      match_param = tolower(.data[[parameter_col]]),
      clean_units = dplyr::if_else(
        trimws(.data[[units_col]]) == "" | is.na(.data[[units_col]]),
        NA_character_,
        .data[[units_col]]
      ),
      match_units = tolower(.data$clean_units)
    )

  # Determine if there are any missing parameter-unit combinations using the
  # lowercase matching keys
  missing_params <- expected_raw_params |>
    dplyr::mutate(
      match_param = tolower(.data$raw_name),
      match_units = tolower(.data$raw_units)
    ) |>
    dplyr::left_join(df_data_params, by = c("match_param", "match_units")) |>
    # If the original raw name evaluates to NA, no match was found in the data frame
    dplyr::filter(is.na(.data[[parameter_col]])) |>
    dplyr::mutate(
      display_units = dplyr::if_else(
        is.na(.data$raw_units),
        "no units",
        .data$raw_units
      ),
      display_label = paste0(.data$raw_name, " (", .data$display_units, ")")
    ) |>
    dplyr::pull(.data$display_label)

  # Generate message and error if there are any missing parameter-unit combinations
  if (length(missing_params) > 0) {
    cli::cli_abort(c(
      "x" = "Missing expected parameter/unit combinations in {.var {parameter_col}} and {.var {units_col}} columns in input",
      "!" = "Missing definitions: {.val {missing_params}}",
      "!" = "Check configuration settings under the {.field parameters} section for input in {.path {config_path}}"
    ))
  }

  invisible(NULL)
}
