# General data import functions

#' Route and Execute Data Extraction for a Single Pipeline Input
#'
#' Acts as the master dispatch interface for the pipeline's ingestion phase. This function
#' reads an individual input configuration block and routes execution to the appropriate
#' network-based or file-based importer utility depending on the declared `input_type`.
#' Centralized resilience variables are threaded through this router to control download stability.
#'
#' @param input_cfg A list representing a parsed `inputs` block from a single input in the YAML
#'   configuration file.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#' @param max_attempts Integer defining the connection retry ceiling forwarded to API down-stream
#'   wrappers.
#' @param initial_delay Numeric value specifying the baseline wait duration (seconds) before
#'   executing the first retry step passed to `api_with_retry`.
#'
#' @return A unified data frame or tibble layout populated by the raw imported dataset.
#'   The data shape is validated against corruption metrics before being released back to
#'   the pipeline.
#' @noRd
import_single_input <- function(
  input_cfg,
  config_path,
  max_attempts,
  initial_delay
) {
  input_id <- input_cfg$input_id
  input_type <- input_cfg$input_type
  cli::cli_h3("Loading input: {.val {input_id}} ({input_type})")

  # Import based on type
  if (input_type == "API-EDI") {
    out <- import_edi_data(
      edi_pkg_id = input_cfg$input_package_id,
      rev_num = input_cfg$curr_rev,
      entity_regex = input_cfg$input_regex,
      config_path = config_path,
      max_attempts = max_attempts,
      initial_delay = initial_delay
    )
  } else if (input_type == "API-CNRA-lab") {
    out <-
      purrr::map(
        input_cfg$input_stations,
        \(x) {
          import_cnra_data(
            x,
            start_date = input_cfg$input_start_date,
            type = "lab",
            max_attempts = max_attempts
          )
        }
      ) |>
      purrr::list_rbind()
  } else if (input_type == "API-CNRA-field") {
    out <-
      purrr::map(
        input_cfg$input_stations,
        \(x) {
          import_cnra_data(
            x,
            start_date = input_cfg$input_start_date,
            type = "field",
            max_attempts = max_attempts
          )
        }
      ) |>
      purrr::list_rbind()
  } else if (
    input_type == "local_file" && "input_regex" %in% names(input_cfg)
  ) {
    # Import multiple local files that match a regex pattern first
    multi_files <- subset_data_input(
      list.files(input_cfg$input_path),
      input_cfg$input_regex,
      config_path,
      multi = TRUE
    )
    out <-
      purrr::map(
        multi_files,
        \(x) import_local_data(file.path(input_cfg$input_path, x), config_path)
      ) |>
      purrr::list_rbind()
  } else if (input_type == "local_file") {
    # Import single local files using their direct path specified in input
    out <- import_local_data(input_cfg$input_path, config_path)
  } else {
    cli::cli_abort("Unknown input type: {.val {input_type}}")
  }

  # Catch empty or corrupted inputs
  check_output_df(out)

  return(out)
}

#' Read a Comma-Separated Values File with All Columns Forced to Text
#'
#' A thin wrapper around `readr::read_csv()` that forces all incoming columns to parse
#' as character strings. This defensive parsing strategy prevents type coercion or formatting
#' mismatches across separate data inputs. It also features expanded missing value handling
#' specifically tuned for historical agency datasets.
#'
#' @param file A character string path to a file, connection, or raw vector.
#' @return A tibble with all variables instantiated as character columns.
#' @noRd
read_csv_text <- function(file) {
  readr::read_csv(
    file,
    col_types = list(.default = "c"),
    na = c("", "NA", "NA:NA")
  )
}

#' Read an Excel Spreadsheet File with All Columns Forced to Text
#'
#' A thin wrapper around `readxl::read_excel()` that explicitly assigns the `"text"` type
#' metadata universally across all target columns. This ensures that mixed numeric-and-string
#' logging parameters are safely read into memory without suffering column type coercion or
#' dropping text attributes.
#'
#' @param file A character string path to the target Excel spreadsheet (`.xls` or `.xlsx`).
#' @return A tibble with all variables instantiated as character columns.
#' @noRd
read_excel_text <- function(file) {
  readxl::read_excel(file, col_types = "text")
}

#' Import Local File Data Using Extension-Based Routing
#'
#' Validates the physical existence of a local data asset on disk and uses its file extension
#' to automatically dispatch execution to the correct file parser. Text-based tabular engines
#' force all columns to load natively as character data to ensure stability during subsequent
#' serialization loops. Unexpected system formatting exceptions are trapped via `rlang::try_fetch`.
#'
#' @param filepath Character string representing the full path to the data file.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#'
#' @return A standardized data frame or tibble layout containing the parsed file records.
#' @noRd
import_local_data <- function(filepath, config_path) {
  # Set up progress message
  status <- new.env()
  status$msg <- ""
  step_id <- cli::cli_progress_step(
    msg = "Importing local file: {.path {filepath}}",
    msg_done = "Importing local file: {.path {filepath}}{status$msg}"
  )

  # Check if file exists on disk
  if (!file.exists(filepath)) {
    cli::cli_abort(c(
      "x" = "File not found at path: {.path {filepath}}",
      "i" = "Please verify the path listed in {.path {config_path}}"
    ))
  }

  # Import data based on file extension
  file_ext <- tools::file_ext(filepath)
  df_data <- rlang::try_fetch(
    expr = {
      # Use switch safely; error out if the extension isn't supported
      switch(
        file_ext,
        csv = read_csv_text(filepath),
        xls = read_excel_text(filepath),
        xlsx = read_excel_text(filepath),
        rds = readRDS(filepath),
        cli::cli_abort(
          "Unsupported file extension: {.val {paste0('.', file_ext)}}"
        )
      )
    },
    error = function(cnd) {
      # Catch any unexpected internal parsing errors
      cli::cli_abort(
        "Failed to parse data format for file {.path {filepath}}",
        parent = cnd
      )
    }
  )

  # Finish progress message
  status$msg <- cli::format_inline(
    "... Data import complete ({nrow(df_data)} rows)"
  )
  cli::cli_progress_done(id = step_id)

  return(df_data)
}
