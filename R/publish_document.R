# Functions for final data publishing and documentation helpers

#' Save a Data Frame with a Dynamic Name to Package Data
#'
#' Serializes a finalized data frame or tibble directly to the standard package `data/`
#' directory as an `.rda` binary file. The function dynamically binds the data to the
#' variable symbol name specified by `dataset_name` inside an isolated temporary evaluation
#' environment before exporting. This ensures the asset parses into user sessions with the
#' correct package symbol name. Files are written using high-efficiency `bzip2` compression.
#'
#' @param data_final The finalized data frame or tibble generated from the workbench.
#' @param dataset_name Character string specifying the symbol name for the package data object.
#'
#' @return Returns `NULL` invisibly; builds directories and serializes an `.rda` file
#'   to disk as a side effect.
#' @noRd
save_package_data <- function(data_final, dataset_name) {
  cli::cli_progress_step("Saving dataset to package data directory")

  # Assign data to the dynamic package symbol name in a temporary environment
  env <- new.env()
  assign(dataset_name, data_final, envir = env)

  data_dir <- file.path(usethis::proj_get(), "data")
  out_path <- file.path(data_dir, paste0(dataset_name, ".rda"))

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  save(list = dataset_name, file = out_path, envir = env, compress = "bzip2")
  invisible(NULL)
}

#' Print Formatted Tibble Dimensions for Package Documentation
#'
#' Evaluates the row and column dimensions of the final integrated dataset and outputs
#' a formatted `@format` text string to the console. Large row numbers are formatted
#' with commas and scientific notation is suppressed to provide a clean string ready
#' for direct copy-pasting into `R/data.R`.
#'
#' @param data_final The finalized data frame or tibble generated from the workbench.
#' @return Returns `NULL` invisibly; outputs formatting instructions directly to the console.
#' @noRd
print_data_dims <- function(data_final) {
  num_rows <- format(nrow(data_final), big.mark = ',', scientific = FALSE)
  cli::cli_alert_info("Update data documentation in {.path R/data.R}:")
  cli::cli_bullets(c(
    "*" = "@format A tibble with {num_rows} rows and {ncol(data_final)} variables"
  ))
  invisible(NULL)
}

#' Generate and Display Data Ingestion Documentation Instructions for EDI Sources
#'
#' Interrogates the Environmental Data Initiative (EDI) portal API to retrieve live
#' citation, DOI, and metadata path elements for incoming data packages. The function
#' processes, filters, and formats this metadata into explicit markdown and copy-paste guidelines
#' to assist developers in synchronizing package-level records, including `README.Rmd`,
#' `R/data.R`, metadata templates, and publication tracking logs.
#'
#' @param edi_info A nested list structure returned from `check_edi_status()` outlining
#'   the package IDs and active revision numbers for the underlying raw data inputs.
#' @param data_final The finalized data frame or tibble generated from the workbench.
#'
#' @return Returns `NULL` invisibly; outputs structural metadata tracking banners directly
#'   to the console interface.
#' @noRd
document_helper_edi <- function(edi_info, data_final) {
  # Build a tibble of EDI ID's, citations, DOI's, and URL's
  step_meta <- cli::cli_progress_step(
    "Obtaining package citations and DOIs from EDI"
  )
  df_edi_info <-
    purrr::map(edi_info, function(pkg) {
      # Create full EDI package ID strings (edi.##.rev) from edi_info
      edi_id <- paste("edi", pkg$edi_pkg_id, pkg$curr_rev, sep = ".")

      tibble::tibble(
        edi_id = edi_id,
        # Create citations for EDI data packages
        edi_cit_raw = EDIutils::read_data_package_citation(
          edi_id,
          access = FALSE
        ),
        # Get DOI for EDI data package
        edi_doi = EDIutils::read_data_package_doi(edi_id),
        # Build the URL to package metadata
        edi_url = paste0(
          "https://portal.edirepository.org/nis/metadataviewer?packageid=",
          edi_id
        )
      )
    }) |>
    purrr::list_rbind()
  cli::cli_progress_done(id = step_meta)
  cli::cli_text("")

  # Citation for README:
  cli::cli_alert_info(
    "Update {cli::qty(nrow(df_edi_info))}citation{?s} in {.path README.Rmd}:"
  )
  ul_readme <- cli::cli_ul()
  df_edi_info |>
    dplyr::select("edi_cit_raw", "edi_doi", "edi_url") |>
    purrr::pwalk(function(edi_cit_raw, edi_doi, edi_url) {
      edi_cit_clean <- stringr::str_remove(
        edi_cit_raw,
        '(?<=Environmental Data Initiative\\.\\s)https.+'
      )
      cli::cli_li("{edi_cit_clean} [{edi_doi}]({edi_url})")
    })
  cli::cli_end(id = ul_readme)
  cli::cli_text("")

  # Data documentation:
  print_data_dims(data_final)
  ul_doc <- cli::cli_ul()
  purrr::walk(df_edi_info$edi_url, \(url) cli::cli_li("URL: {url}"))
  cli::cli_end(id = ul_doc)
  cli::cli_text("")

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  cli::cli_alert_info(
    "Update {.var Data_source} in {.path publication/data_objects/Delta_Integrated_WQ_metadata.csv}:"
  )
  ul_pub <- cli::cli_ul()
  purrr::walk(df_edi_info$edi_url, \(url) cli::cli_li("{url}"))
  cli::cli_end(id = ul_pub)
  cli::cli_text("")

  # metadata_templates/methods.docx
  cli::cli_alert_info(
    "Update {.val 5. Data Sources} in {.path publication/metadata_templates/methods.docx}:"
  )
  ul_methods <- cli::cli_ul()
  purrr::walk(df_edi_info$edi_cit_raw, \(cit) cli::cli_li("{cit}"))
  cli::cli_end(id = ul_methods)
  cli::cli_text("")

  # metadata_templates/provenance.txt
  cli::cli_alert_info(
    "Update {.var dataPackageID} in {.path publication/metadata_templates/provenance.txt}: {.val {df_edi_info$edi_id}}"
  )
  cli::cli_rule()
  invisible(NULL)
}

#' Generate Data Ingestion Documentation Instructions for Non-EDI Sources
#'
#' Builds alternative documentation tracking instructions to the console when an integrated
#' data input does not originate from an EDI data repository package. It dynamically calculates
#' temporal parameters using the maximum calendar year present in the data vector to supply
#' template reminders for files like `README.Rmd`, `methods.docx`, and structural provenance logs.
#'
#' @param data_final The finalized data frame or tibble generated from the workbench. Must contain
#'   a valid temporal column named `Date`.
#' @return Returns `NULL` invisibly; outputs check-lists and manual auditing notifications
#'   directly to the console interface.
#' @noRd
document_helper_other <- function(data_final) {
  dataset_yr <- max(lubridate::year(data_final$Date), na.rm = TRUE)

  # Citation for README:
  cli::cli_alert_info("Update citation in {.path README.Rmd}:")
  cli::cli_bullets(c("*" = "Year of dataset is {dataset_yr}"))
  cli::cli_text("")

  # Data documentation:
  print_data_dims(data_final)
  cli::cli_bullets(c(
    "*" = "Check the URL for metadata and information on methods in the @details section, and update if necessary"
  ))
  cli::cli_text("")

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  cli::cli_alert_info(
    "Check {.var Data_source} in {.path publication/data_objects/Delta_Integrated_WQ_metadata.csv}, and update if necessary"
  )
  cli::cli_text("")

  # metadata_templates/methods.docx
  cli::cli_alert_info(
    "Update {.val 5. Data Sources} in {.path publication/metadata_templates/methods.docx}:"
  )
  cli::cli_bullets(c("*" = "Year of dataset is {dataset_yr}"))
  cli::cli_text("")

  # metadata_templates/provenance.txt
  cli::cli_alert_info(
    "Check information in {.path publication/metadata_templates/provenance.txt}, and update if necessary"
  )
  cli::cli_rule()
  invisible(NULL)
}

#' Update EDI Package Last Revision Fields within YAML Configuration
#'
#' @param config The full parsed list from the YAML configuration file.
#' @param edi_info A nested list structure returned from `check_edi_status()` outlining
#'   the package IDs and active revision numbers for the underlying raw data inputs.
#' @param config_path Character string path to the survey's YAML configuration file.
#' @return An updated configuration list structure.
#' @noRd
update_edi_revisions <- function(config, edi_info, config_path) {
  # Guard against empty configuration or missing target sections
  if (is.null(config$update_status$edi_package)) {
    return(config)
  }

  config$update_status$edi_package <- purrr::map(
    config$update_status$edi_package,
    function(pkg) {
      # Extract the target package ID configured in this section
      target_id <- pkg$package_id

      # Match against the verified package IDs inside the edi_info list
      match_pkg <- purrr::keep(edi_info, \(x) x$edi_pkg_id == target_id)

      # Produce defensive error if the configured package wasn't fetched/verified
      if (length(match_pkg) == 0) {
        cli::cli_abort(c(
          "x" = "Configuration specifies EDI package {.val edi.{target_id}}, but no metadata was obtained for it through the EDI API.",
          "!" = "Verify that the identifier is valid and active in the EDI repository."
        ))
      }

      # Assign the current revision as the new baseline last_revision
      pkg$last_revision <- as.integer(match_pkg[[1]]$curr_rev)
      return(pkg)
    }
  )

  return(config)
}

#' Update and Format the Package YAML Configuration File
#'
#' Updates configuration tracking blocks (such as the tracking dates and updated live
#' repository revision indices) inside a structural survey configuration file. Once modified,
#' the function serializes the list back to disk and evaluates it line-by-line via regular
#' expressions to re-inject vertical whitespace blocks for optimal human readability.
#'
#' @param config_path Character string path to the survey's YAML configuration file.
#' @param edi_info A nested list structure returned from `check_edi_status()` outlining
#'   the package IDs and active revision numbers for the underlying raw data inputs.
#'   `NULL` if not applicable.
#'
#' @return Returns `NULL` invisibly; modifies the physical file on disk as a side effect.
#' @noRd
update_config_metadata <- function(config_path, edi_info = NULL) {
  cli::cli_progress_step(
    "Updating configuration metadata with current revision"
  )

  config <- yaml::read_yaml(config_path)
  config$update_status$last_update <- as.character(Sys.Date())

  if (!is.null(edi_info)) {
    config <- update_edi_revisions(config, edi_info, config_path)
  }

  # Write the core YAML with indented sequences and verbatim booleans
  yaml::write_yaml(
    config,
    file = config_path,
    indent.mapping.sequence = TRUE,
    handlers = list(logical = yaml::verbatim_logical)
  )

  # Post-process the file text to re-inject vertical whitespace
  yaml_lines <- readLines(config_path)

  # Space out Level-1 Keys
  level_1_positions <- stringr::str_which(yaml_lines, "^[a-zA-Z0-9_-]+:")
  if (length(level_1_positions) > 1) {
    target_l1 <- level_1_positions[-1]
    yaml_lines[target_l1] <- paste0("\n", yaml_lines[target_l1])
  }

  # Space out Input Blocks (using indented sequence spacing pattern)
  input_item_positions <- stringr::str_which(yaml_lines, "^\\s+-\\s+input_id:")
  if (length(input_item_positions) > 1) {
    target_inputs <- input_item_positions[-1]
    yaml_lines[target_inputs] <- paste0("\n", yaml_lines[target_inputs])
  }

  writeLines(yaml_lines, con = config_path)
  invisible(NULL)
}

#' Publish Compiled Dataset, Update Documentation Guides, and Version Configuration Metadata
#'
#' Coordinates the terminal step of the data compilation lifecycle. This function exports
#' the integrated data frame to the package's binary archive repository under high-efficiency
#' compression, interrogates metadata servers to output targeted manual copy-paste instructions
#' for documentation maintenance, and automatically increments revision indices inside the
#' survey's configuration file.
#'
#' @param data_final The finalized data frame or tibble generated from the workbench.
#' @param dataset_name Character string specifying the symbol name for the package data object.
#' @param config_path Character string path to the survey's YAML configuration file.
#' @param edi_info A nested list structure returned from `check_edi_status()` outlining
#'   the package IDs and active revision numbers for the underlying raw data inputs.
#'   `NULL` if not applicable.
#'
#' @return Returns `NULL` invisibly; serializes binary artifacts to disk, restructures
#'   configuration files, and outputs tracking guidelines to the console interface.
#' @noRd
publish_survey_data <- function(
  data_final,
  dataset_name,
  config_path,
  edi_info = NULL
) {
  cli::cli_h1(
    "Publishing Dataset and Providing Documentation: {.val {dataset_name}}"
  )

  # Save the data_final to the package data/ directory
  save_package_data(data_final, dataset_name)

  # Print documentation help strings to the console
  cli::cli_h2("Documentation information for manual update")
  if (!is.null(edi_info)) {
    document_helper_edi(edi_info, data_final)
  } else {
    document_helper_other(data_final)
  }

  # Update YAML configuration revision metadata
  update_config_metadata(config_path, edi_info)

  # Success summary banner
  cli::cli_bullets(c(
    "v" = "Dataset successfully archived",
    "v" = "YAML configuration file versioned up: {.path {config_path}}"
  ))
  invisible(NULL)
}
