# Utility functions specific to EDI

#' Get the newest revision of an EDI data package
#'
#' @param edi_pkg_id Character or numeric string identifying the baseline EDI scope-identifier
#'   for the data package.
#' @return An integer representing the newest revision number of the EDI data package
#' @noRd
get_current_edi_id <- function(edi_pkg_id) {
  # Catch network, firewall, server-side, or invalid ID crashes at the source
  curr_rev <- rlang::try_fetch(
    expr = {
      EDIutils::list_data_package_revisions(
        scope = "edi",
        identifier = edi_pkg_id,
        filter = "newest"
      )
    },
    error = function(cnd) {
      cli::cli_abort(
        "Failed to reach the EDI registry or locate package identifier {.val {edi_pkg_id}}.",
        parent = cnd
      )
    }
  )

  return(curr_rev)
}

#' Check EDI Status
#'
#' Check if the most current revision of EDI publication differs from the revision used
#' in the last discretewq update. Provides messaging indicating whether to proceed with
#' the update and stops execution if check fails.
#'
#' @param cfg_update A list representing a parsed `update_status` block from the YAML
#'   configuration file.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#'
#' @return A named list of length two elements to be used in downstream functions that require
#'   this information. Could have info for more than one EDI package. The list elements include:
#'   * `edi_pkg_id`: the numeric EDI package ID.
#'   * `curr_rev`: the most current revision number.
#' @noRd
check_edi_status <- function(cfg_update, config_path) {
  # Define custom theme for .emph class
  cli::cli_div(
    theme = list(span.emph = list(color = "orange", font_weight = "bold"))
  )

  # Extract the package info from the update_status section of the configuration file
  packages <- cfg_update$edi_package

  # Check each EDI package listed under update_status (most of the time there is only one)
  package_check <- purrr::map(packages, function(package) {
    edi_pkg_id <- package$package_id
    last_rev <- package$last_revision

    # Obtain current revision number for EDI package
    curr_rev <- get_current_edi_id(edi_pkg_id)

    # Provide message on EDI revision status
    cli::cli_bullets(c(
      "i" = "Checking update status of edi.{edi_pkg_id}",
      "i" = "Revision {.emph #{last_rev}} was used during the last discretewq update",
      "i" = "The current revision of EDI publication is {.emph #{curr_rev}}"
    ))

    is_eligible <- if (curr_rev > last_rev) {
      cli::cli_bullets(c(
        ">" = "The EDI data package {.strong HAS} been updated since the last discretewq update"
      ))
      TRUE
    } else if (isTRUE(package$static)) {
      # If static is set to TRUE, proceed even if the data package hasn't been updated
      cli::cli_bullets(c(
        ">" = "The EDI data package {.strong HAS NOT} been updated since the last discretewq update",
        "!" = "However, it is {.strong eligible for update} because {.code static = TRUE} for this edi_package under {.var update_status} in {.path {config_path}}"
      ))
      TRUE
    } else {
      # Set as not needing revision if current revision isn't greater than revision used
      # in the last discretewq update, and if static is set to FALSE
      cli::cli_bullets(c(
        ">" = "The EDI data package {.strong HAS NOT} been updated since the last discretewq update",
        "x" = "EDI check {.strong FAILED}"
      ))
      FALSE
    }
    cli::cli_text("")
    return(tibble::lst(edi_pkg_id, curr_rev, is_eligible))
  })

  # Final single-evaluation status printout
  needs_revision <- all(purrr::map_lgl(package_check, "is_eligible"))
  if (isFALSE(needs_revision)) {
    cli::cli_abort(
      message = c(
        "x" = "Pipeline halted: EDI check returned {.val FALSE}",
        "i" = "One or more required EDI packages have not been updated since the last discretewq update",
        "!" = "To bypass this and run anyway, set {.code static = TRUE} for the failing packages in {.path {config_path}}"
      ),
      call = NULL
    )
  }

  # Remove is_eligible from the list and return
  purrr::map(package_check, \(x) purrr::discard_at(x, "is_eligible"))
}

#' Add Current EDI Revisions to Input sections within YAML Configuration
#'
#' @param config The full parsed list from the YAML configuration file.
#' @param edi_info A nested list structure returned from `check_edi_status()` outlining
#'   the package IDs and active revision numbers for the underlying raw data inputs.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#'
#' @return An updated configuration list structure.
#' @noRd
add_edi_revisions <- function(config, edi_info, config_path) {
  config$inputs <- purrr::map(config$inputs, function(input) {
    # Bypass any inputs not from EDI
    if (is.null(input$input_type) || input$input_type != "API-EDI") {
      return(input)
    }

    # Match input_package_id against the verified edi_pkg_id in edi_info
    target_id <- input$input_package_id
    match_pkg <- purrr::keep(edi_info, \(x) x$edi_pkg_id == target_id)

    # Produce error if an input relies on an ID that wasn't in update_status
    if (length(match_pkg) == 0) {
      cli::cli_abort(c(
        "x" = "Input {.val {input$input_id}} requires package {.val edi.{target_id}}, but it wasn't specified under {.field update_status$edi_package} in YAML configuration file",
        "!" = "Update {.path {config_path}} so that all {.field input_package_id} within {.field inputs} with {.code input_type: API-EDI} are specified under {.field update_status$edi_package}"
      ))
    }

    input$curr_rev <- as.integer(match_pkg[[1]]$curr_rev)
    return(input)
  })

  return(config)
}
