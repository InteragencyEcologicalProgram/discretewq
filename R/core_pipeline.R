# Primary working functions that read the YAML configurations and orchestrate the pipeline stages

#' Execute the Full Data Ingestion, Standardization, and Synthesis Pipeline
#'
#' Orchestrates the entire lifecycle of a survey data compilation workflow using a nested,
#' tiered caching system. The pipeline checks remote repository (EDI) statuses, tracks
#' cryptographic configuration states to determine caching layers (`STD` vs. `RAW`), safely
#' iterates through batch downloads, standardizes database structures, maps dynamic acronym-based
#' custom cleaner functions, and applies final structural tasks. Supports an interactive execution
#' mode to pause execution early and return intermediate data structures.
#'
#' @param config_path Character string path to the survey's YAML configuration file.
#' @param force_refresh Logical flag indicating whether to bypass existing local `.qdata` cache
#'   assets and force fresh API transactions and re-processing. Defaults to `FALSE`.
#' @param max_age_days Numeric ceiling indicating the maximum lifespan (in days) of local cached
#'   objects before expiration triggers an automatic update. Defaults to 7.
#' @param max_attempts Integer defining the connection retry ceiling forwarded to API down-stream
#'   wrappers. Defaults to 5.
#' @param initial_delay Numeric value specifying the baseline wait duration (seconds) before
#'   executing the first retry step. Defaults to 2.
#' @param interactive_until Character string defining the target execution stop boundary.
#'   Must be one of `"full"` (default), `"raw"`, `"standardize"`, or `"custom"`.
#'   If anything other than `"full"` is selected, execution halts at that stage and returns
#'   the intermediate dataset for interactive debugging/development.
#'
#' @return Depending on `interactive_until`:
#'   * `"full"`: A named list of two elements (`final_data`, `ls_edi_info`).
#'   * `"raw"`: A named list of the raw imported datasets (`ls_raw`).
#'   * `"standardize"`: A named list of the standardized datasets (`ls_standardized`).
#'   * `"custom"`: The data frame returned directly by the dynamic `cleaner_fn` before final
#'     cleaning (`df_custom_data`).
#' @noRd
run_survey_pipeline <- function(
  config_path,
  force_refresh = FALSE,
  max_age_days = 7,
  max_attempts = 5,
  initial_delay = 2,
  interactive_until = c("full", "raw", "standardize", "custom")
) {
  interactive_until <- rlang::arg_match(interactive_until)

  # Import configuration file for survey
  config <- yaml::read_yaml(config_path)

  # Provide messaging
  acronym <- config$source_metadata$acronym
  cli::cli_h1("Processing: {acronym}")

  # Run EDI status check
  # Only run if EDI is a data source - EDI packages are configured under update_status
  if ("edi_package" %in% names(config$update_status)) {
    cli::cli_h2("Running EDI Status Check")
    ls_edi_info <- check_edi_status(config$update_status, config_path)
    cli::cli_alert_success("Proceeding with updates to this data set")

    # Add current EDI revision numbers to the EDI input configuration sections before
    # creating cache ID's and downloading data
    config <- add_edi_revisions(config, ls_edi_info, config_path)
  } else {
    ls_edi_info <- NULL
  }

  # Configure cache settings
  # Generate unique identifier hashes based on the config structure and EDI package ID's (if used)
  # for the raw and standardized data caches
  cache_ids <- create_cache_ids(config, ls_edi_info)

  # Import and standardize data inputs
  # Extract input configuration block for repeated function inputs
  cfg_inputs <- config$inputs

  # --- PHASE 1: RAW DATA IMPORT ---
  ls_raw <- run_with_cache(
    cache_id = cache_ids$raw,
    force_refresh = force_refresh,
    max_age_days = max_age_days,
    expr = {
      cli::cli_h2("Importing Data")
      pipeline_import(cfg_inputs, config_path, max_attempts, initial_delay)
    }
  )

  # Early exit if user only requested raw data
  if (interactive_until == "raw") {
    cli::cli_alert_info(
      "Pipeline execution paused after raw data import (`interactive_until = 'raw'`)."
    )
    return(ls_raw)
  }

  # --- PHASE 2: STANDARDIZATION ---
  ls_standardized <- run_with_cache(
    cache_id = cache_ids$std,
    force_refresh = force_refresh,
    max_age_days = max_age_days,
    expr = {
      cli::cli_h2("Standardizing Data")
      pipeline_standardize(ls_raw, cfg_inputs, config_path)
    }
  )

  # Early exit if user only requested standardized data
  if (interactive_until == "standardize") {
    cli::cli_alert_info(
      "Pipeline execution paused after standardization (`interactive_until = 'standardize'`)."
    )
    return(ls_standardized)
  }

  # --- PHASE 3: CUSTOM CLEANING AND SYNTHESIS ---
  cli::cli_h2("Running custom cleaning script for {.val {acronym}}")

  # Safely pull the custom cleaning function
  cleaner_fn <- get_custom_cleaner(acronym)

  # Run the custom cleaning script across the standardized inputs
  df_custom_data <- cleaner_fn(ls_standardized)

  # Early exit if user only requested custom cleaner output
  if (interactive_until == "custom") {
    cli::cli_alert_success(
      "Pipeline execution paused after custom `cleaner_fn` (`interactive_until = 'custom'`)."
    )
    return(df_custom_data)
  }

  # --- PHASE 4: FINAL GENERAL CLEANING STEPS ---
  cli::cli_h2("Performing final cleaning steps")
  final_data <- df_custom_data |>
    # Remove rows where all measurements are NA, if they exist
    rm_rows_all_miss_data(config_path) |>
    # Add Source column
    add_source_col(acronym) |>
    # Standardize column order
    standardize_col_order()

  tibble::lst(final_data, ls_edi_info)
}

#' Safe Batch Ingestion of Configured Survey Data Inputs
#'
#' Maps across all raw data inputs defined inside a configuration profile using an isolated
#' error-boundary loop via `purrr::safely`. If one or more download channels fail due to
#' unrecoverable network exceptions or file system faults, the function gathers all faults into
#' a structural report, passes the trace arrays to the global workspace, and halts execution
#' before any data corruption cascades.
#'
#' @param cfg_inputs A list representing a parsed `inputs` block from the YAML configuration file.
#' @param config_path Character string path to the survey's YAML configuration file.
#' @param max_attempts Integer defining the connection retry ceiling forwarded to API down-stream
#'   wrappers.
#' @param initial_delay Numeric value specifying the baseline wait duration (seconds) before
#'   executing the first retry step.
#'
#' @return A named list of raw unstandardized data frames or tibbles, where elements are named
#'   by their respective `input_id`.
#' @noRd
pipeline_import <- function(
  cfg_inputs,
  config_path,
  max_attempts,
  initial_delay
) {
  # Import all inputs from config while executing resilient mapping and caching data
  safe_import <- purrr::safely(import_single_input)
  ls_raw_output <- purrr::map(
    cfg_inputs,
    \(x) safe_import(x, config_path, max_attempts, initial_delay)
  )

  # Transpose to separate raw results from caught error objects
  ls_separate_output <- purrr::list_transpose(ls_raw_output)
  ls_results <- ls_separate_output$result
  ls_error <- ls_separate_output$error

  # Assign input IDs as list element names for easy user access
  names(ls_results) <- purrr::map_chr(cfg_inputs, "input_id")
  names(ls_error) <- purrr::map_chr(cfg_inputs, "input_id")

  # If there are any errors, print the tree error report for each error
  if (any(!purrr::map_lgl(ls_error, is.null))) {
    report_pipeline_errors(
      error_list = ls_error,
      cfg_inputs = cfg_inputs,
      msg_template_fn = \(x) {
        cli::format_inline(
          "Failed to load input {.val {x$input_id}} from path/API"
        )
      }
    )
    # Halt execution completely after reporting everything
    cli::cli_abort(
      message = "Data pipeline halted. One or more inputs failed to load properly.",
      call = NULL
    )
  } else {
    cli::cli_text("")
    cli::cli_alert_success("All inputs loaded without errors")
  }

  return(ls_results)
}

#' Safe Batch Standardization of Ingested Pipeline Inputs
#'
#' Reorders and pairs incoming raw data matrices with their respective YAML configuration
#' structures before executing schema standardization under an isolated error boundary loop. If
#' schema validation fails, parsing drops, or column mismatches occur, the processing loop compiles
#' a master error tracking report and binds the upstream raw matrices directly to
#' "failed_workflow_data.qdata" for live debugging before execution halts.
#'
#' @param raw_list A named list of raw data frame objects returned by `pipeline_import()`.
#' @param cfg_inputs A list representing a parsed `inputs` block from the YAML configuration file.
#' @param config_path Character string path to the survey's YAML configuration file.
#'
#' @return A named list of structurally standardized data frames or tibbles conforming to
#'   the package baseline schema.
#' @noRd
pipeline_standardize <- function(raw_list, cfg_inputs, config_path) {
  # Make sure that the elements in raw_list are in the same order as the inputs in the
  # configuration file
  ls_raw_ordered <- purrr::map(cfg_inputs, \(x) raw_list[[x$input_id]])

  safe_standardize <- purrr::safely(standardize_single_input)
  ls_std_output <- purrr::map2(
    ls_raw_ordered,
    cfg_inputs,
    \(x, y) safe_standardize(x, y, config_path)
  )

  # Transpose to separate raw results from caught error objects
  ls_separate_output <- purrr::list_transpose(ls_std_output)
  ls_results <- ls_separate_output$result
  ls_error <- ls_separate_output$error

  # Assign input IDs as list element names for easy user access
  names(ls_results) <- purrr::map_chr(cfg_inputs, "input_id")
  names(ls_error) <- purrr::map_chr(cfg_inputs, "input_id")

  # If there are any errors, print the tree error report for each error
  if (any(!purrr::map_lgl(ls_error, is.null))) {
    report_pipeline_errors(
      error_list = ls_error,
      cfg_inputs = cfg_inputs,
      msg_template_fn = \(x) {
        cli::format_inline("Failed to standardize input {.val {x$input_id}}")
      }
    )
    # Save the raw data before standardization as a local binary file for inspection
    qs2::qd_save(raw_list, file = FP_FAILED_RAW_DATA)
    cli::cli_bullets(c(
      "i" = "Raw data before standardization saved to {.path {FP_FAILED_RAW_DATA}} for reference.",
      "i" = "Run {.code failed_workflow_data <- qs2::qd_read('{FP_FAILED_RAW_DATA}')} to load and inspect it."
    ))
    # Halt execution completely after reporting everything
    cli::cli_abort(
      message = "Data pipeline halted. One or more inputs failed to standardize properly.",
      call = NULL
    )
  } else {
    cli::cli_text("")
    cli::cli_alert_success("All inputs standardized without errors")
  }

  return(ls_results)
}
