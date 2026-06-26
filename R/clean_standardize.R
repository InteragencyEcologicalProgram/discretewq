# Functions for schema standardization

#' Parse a Column Vector Based on Target Type
#'
#' @param vec A raw atomic vector.
#' @param type_goal The desired data type. Supported options: "character", "numeric", "date",
#'   "datetime", "character_time".
#' @param column_name Character string of the column name used for contextual error reporting.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#' @param dt_fmt An optional character string to specify a date-time format for a date-time object.
#'
#' @return A parsed vector of the target class.
#' @noRd
parse_column_vector <- function(
  vec,
  type_goal,
  column_name,
  config_path,
  dt_fmt = NULL
) {
  # Define valid types for type_goal
  valid_types <- c("character", "numeric", "date", "datetime", "character_time")

  # Exit with error if col_type isn't specified
  if (is.null(type_goal) || type_goal == "") {
    cli::cli_abort(c(
      "x" = "No {.field col_type} provided for {.var {column_name}}",
      "!" = "Please specify a valid {.field col_type} in {.path {config_path}}",
      "i" = "Supported types are: {.val {valid_types}}"
    ))
  }

  # Check for standardized type_goal and provide error if no match
  if (!type_goal %in% valid_types) {
    cli::cli_abort(c(
      "x" = "Invalid {.field col_type} {.val {type_goal}} for {.var {column_name}}",
      "!" = "Please specify a valid {.field col_type} in {.path {config_path}}",
      "i" = "Supported types are: {.val {valid_types}}"
    ))
  }

  # Require dt_fmt for date, datetime and character_time types, and provide error
  # message if its not provided
  if (
    type_goal %in% c("date", "datetime", "character_time") && is.null(dt_fmt)
  ) {
    cli::cli_abort(c(
      "x" = "{.field col_type} is set to {.val {type_goal}} for {.var {column_name}}, but no {.field col_format} was provided",
      "!" = "Please provide a date-time format as a character vector for {.field col_format} in {.path {config_path}}"
    ))
  }

  suppressWarnings({
    switch(
      type_goal,
      "character" = as.character(vec),
      "numeric" = as.numeric(vec),
      "date" = lubridate::date(lubridate::parse_date_time(vec, dt_fmt)),
      "datetime" = lubridate::parse_date_time(vec, dt_fmt),
      "character_time" = format(
        lubridate::parse_date_time(vec, dt_fmt),
        "%H:%M:%S"
      )
    )
  })
}

#' Standardize Data Frame Columns Based on YAML Schema (Case-Insensitive)
#'
#' @param df_data A data frame or tibble containing raw unstandardized survey inputs.
#' @param columns_cfg A list representing a parsed `columns` block from a single input in the
#'   YAML configuration file.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#'
#' @return A standardized tibble containing only configured columns in `columns_cfg`
#' @noRd
standardize_columns <- function(df_data, columns_cfg, config_path) {
  cli::cli_progress_step("Standardizing column names and types")

  # Initialize an empty base R character vector to collect errors
  error_log <- c()

  # Standardize each column separately
  ls_clean <- purrr::map(columns_cfg, function(col_cfg) {
    raw_col <- col_cfg$raw_name
    target_col <- col_cfg$target_name
    target_type <- col_cfg$col_type

    # Find the actual column name in the data frame that matches raw_col ignoring case
    actual_col_name <- colnames(df_data)[
      tolower(colnames(df_data)) == tolower(raw_col)
    ]

    # If a column was validated but somehow disappears, fallback safely
    if (length(actual_col_name) == 0) {
      actual_col_name <- raw_col
    }

    vec <- df_data[[actual_col_name]]
    orig_na_count <- sum(is.na(vec))

    # Extract the optional date_format if it exists, otherwise pass NULL
    fmt <- if (!is.null(col_cfg$col_format)) col_cfg$col_format else NULL

    # Execute parsing rules
    parsed_vec <- parse_column_vector(
      vec,
      target_type,
      raw_col,
      config_path,
      dt_fmt = fmt
    )

    new_na_count <- sum(is.na(parsed_vec))

    # Defensive programming check for newly introduced NAs
    if (new_na_count > orig_na_count) {
      failed_rows <- new_na_count - orig_na_count

      error_msg <- cli::cli_fmt(
        cli::cli_bullets(c(
          "x" = "Column {.var {actual_col_name}}: {failed_rows} value{?s} forced to `NA` during conversion to {.val {target_type}}"
        ))
      )

      # Modify error_logs in the parent environment
      error_log <<- c(error_log, error_msg)

      # Return a placeholder of NAs matching the row count so bind_cols doesn't break
      placeholder_vec <- rep(NA, nrow(df_data))
      return(tibble::tibble(!!target_col := placeholder_vec))
    }

    # If successful, return the cleanly parsed vector column
    return(tibble::tibble(!!target_col := parsed_vec))
  })

  # Evaluation Gate: Did any columns log errors?
  if (length(error_log) > 0) {
    cli::cli_abort(c(
      "x" = "Parsing failed for input with {length(error_log)} conversion error{?s}:",
      error_log,
      "!" = "Check values in raw data ({.var workflow_data}) and specify an appropriate {.field col_type} for columns in {.path {config_path}}"
    ))
  }

  # If everything passed cleanly, combine columns side-by-side
  df_clean <- dplyr::bind_cols(ls_clean)

  # Catch empty or corrupted data post-standardization
  check_output_df(df_clean)

  return(df_clean)
}

#' Standardize Long-Format Parameters Based on YAML Schema (Case-Insensitive)
#'
#' @param df_data A data frame or tibble whose columns have already been standardized.
#' @param parameters_cfg A list representing a parsed `parameters` block from a single input in
#'   the YAML configuration file.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#'
#' @return A filtered tibble containing standardized target parameter names
#' @noRd
standardize_parameters <- function(df_data, parameters_cfg, config_path) {
  # Set up progress message
  status <- new.env()
  status$msg <- ""
  step_id <- cli::cli_progress_step(
    msg = "Standardizing parameter names",
    msg_done = "Standardizing parameter names{status$msg}"
  )

  # Define parameter and units column names in df_data from parameters
  parameter_col <- parameters_cfg$parameter_col
  units_col <- parameters_cfg$units_col

  # Build a case-insensitive dual-key translation map from the YAML definitions
  # Combines both raw_name and raw_units into an absolute target identifier mapping
  param_map <-
    purrr::map(parameters_cfg$definitions, function(param) {
      if (!is.null(param$raw_name) && !is.null(param$target_name)) {
        raw_units <- purrr::pluck(param, "raw_units", .default = NA_character_)
        if (!is.na(raw_units) && trimws(raw_units) == "") {
          raw_units <- NA_character_
        }

        tibble::tibble(
          match_param = tolower(param$raw_name),
          match_units = tolower(raw_units),
          target_name = param$target_name
        )
      }
    }) |>
    purrr::list_rbind()

  # Extract unique parameter-unit blocks present inside the raw data frame
  initial_rows <- nrow(df_data)
  df_data_pairs <- df_data |>
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

  # Pinpoint which data combinations are missing from the configuration mapping
  unmapped_pairs <- df_data_pairs |>
    dplyr::left_join(param_map, by = c("match_param", "match_units")) |>
    dplyr::filter(is.na(.data$target_name)) |>
    dplyr::mutate(
      display_units = dplyr::if_else(
        is.na(.data$clean_units),
        "no units",
        .data[[units_col]]
      ),
      display_label = paste0(
        .data[[parameter_col]],
        " (",
        .data$display_units,
        ")"
      )
    ) |>
    dplyr::pull(.data$display_label)

  # Notify the user if rows are being excluded based on missing mapping criteria
  if (length(unmapped_pairs) > 0) {
    cli::cli_bullets(c(
      "i" = "Removing {length(unmapped_pairs)} unspecified parameter-unit combination{?s} not listed for input in {.path {config_path}}:",
      "*" = "Dropped entries: {.val {sort(unmapped_pairs)}}"
    ))
  }

  # Execute composite inner join to clean and filter observations simultaneously
  df_clean <- df_data |>
    dplyr::mutate(
      match_param = tolower(.data[[parameter_col]]),
      clean_units = dplyr::if_else(
        trimws(.data[[units_col]]) == "" | is.na(.data[[units_col]]),
        NA_character_,
        .data[[units_col]]
      ),
      match_units = tolower(.data$clean_units)
    ) |>
    dplyr::inner_join(param_map, by = c("match_param", "match_units")) |>
    # Overwrite parameter column with the standardized target variable layout name
    dplyr::mutate(!!parameter_col := .data$target_name) |>
    # Clean up relational keys
    dplyr::select(
      !tidyselect::any_of(c(
        "match_param",
        "clean_units",
        "match_units",
        "target_name"
      ))
    )

  # Catch empty or corrupted data post-standardization
  check_output_df(df_clean)

  # Log final completion row results
  final_rows <- nrow(df_clean)
  dropped_rows <- initial_rows - final_rows

  status$msg <- cli::format_inline(
    "... Kept {final_rows} row{?s} ({cli::no(dropped_rows)} row{?s} dropped)"
  )
  cli::cli_progress_done(id = step_id)

  return(df_clean)
}
