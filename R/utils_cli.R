# Helper functions to extract objects or handle errors

#' Subset Data Inputs Using Regular Expression Matching
#'
#' Filters a character vector of data inputs (such as file paths, station lists, or entity names)
#' down to elements matching a specific regular expression. Features defensive error boundaries
#' to ensure that exact match limits are respected depending on whether single-file or multi-file
#' processing mode is active.
#'
#' @param data_inputs A character vector of available data items.
#' @param regex_pattern Character string containing the regular expression used for filtering.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#' @param multi Logical flag indicating whether to permit multiple matching elements.
#'   If `FALSE` (default), matching more than one element will trigger an execution error.
#'
#' @return A character vector of matched data input strings.
#' @noRd
subset_data_input <- function(
  data_inputs,
  regex_pattern,
  config_path,
  multi = FALSE
) {
  input_match <- stringr::str_subset(data_inputs, regex_pattern)

  # Check if input_match is anything other than a single string and provide message if so
  if (length(input_match) == 0) {
    cli::cli_abort(c(
      "x" = "The following regex pattern did not find a data input: {.val {regex_pattern}}",
      "!" = "Update input regex pattern in {.path {config_path}} before proceeding"
    ))
  } else if (length(input_match) > 1 && isFALSE(multi)) {
    cli::cli_abort(c(
      "x" = "The following regex pattern found more than one data input: {.val {regex_pattern}}",
      "!" = "Update input regex pattern in {.path {config_path}} to find only one input before proceeding"
    ))
  }

  return(input_match)
}

#' Extract Attributes from an Array of Nested Configuration Blocks
#'
#' Iterates across a list of configuration objects to find an element where a specific
#' key-value pair (`target_key == target_val`) matches, then extracts the requested
#' target attribute defined by `extract_key`.
#'
#' @param cfg_object A list representing a parsed block from the YAML configuration file
#'   (e.g., column blocks).
#' @param target_key Character string representing the key name used to filter blocks.
#' @param target_val Character string representing the specific value to look for.
#' @param extract_key Character string naming the target property to pluck out once a match
#'   is found.
#'
#' @return The value of the extracted property if found; returns `NULL` if no elements match.
#' @noRd
extract_config_info <- function(
  cfg_object,
  target_key,
  target_val,
  extract_key
) {
  matched_block <- purrr::keep(cfg_object, \(x) x[[target_key]] == target_val)

  if (is.null(matched_block)) {
    return(NULL)
  }

  purrr::pluck(matched_block, 1, extract_key)
}

#' Compile and Display Pipeline Ingestion Validation Errors
#'
#' Loops over a collection of caught condition objects generated during data ingestion,
#' constructs readable error messages using a customizable naming function, and outputs them
#' to the console. The complete array of raw error conditions is then bound to the global
#' environment to facilitate rapid interactive debugging.
#'
#' @param error_list A list containing caught error condition objects or `NULL` values.
#' @param cfg_inputs A list representing a parsed `inputs` block from the YAML configuration file.
#' @param msg_template_fn A callback function that accepts a configuration item block and
#'   returns a character string message prefix.
#'
#' @return Returns `NULL` invisibly; modifies the global workspace by assigning the
#'   `workflow_errors` diagnostic variable as a side effect.
#' @noRd
report_pipeline_errors <- function(error_list, cfg_inputs, msg_template_fn) {
  cli::cli_h1("Pipeline Error Report")

  # Extract and compile all errors
  for (i in seq_along(error_list)) {
    input_item <- cfg_inputs[[i]]
    cnd <- error_list[[i]]

    if (!is.null(cnd)) {
      resolved_msg <- msg_template_fn(input_item)
      wrapped_cnd <- rlang::error_cnd(
        message = cli::format_error(c("x" = resolved_msg)),
        parent = cnd
      )

      message(rlang::cnd_message(wrapped_cnd), "\n")
    }
  }

  # Save the full error tracking array to the global environment for live debugging
  workflow_errors <<- error_list
  cli::cli_alert_info(
    "Full error objects saved to global variable {.var workflow_errors} for inspection."
  )

  invisible(NULL)
}
