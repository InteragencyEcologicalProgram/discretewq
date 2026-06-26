# Import functions for web API's

#' Execute an API Call with Exponential Backoff and Jitter Retry Logic
#'
#' Wraps a downloading or data-fetching function inside a stateful retry loop.
#' The function aborts execution immediately if an unrecoverable HTTP 404 error is
#' identified, but handles transient network errors by calculating an exponential backoff delay
#' paired with a random uniform jitter to prevent server overload.
#'
#' @param api_fn The core function to evaluate (e.g., a data retrieval endpoint).
#' @param max_attempts Integer defining the ceiling for total execution attempts.
#' @param initial_delay Numeric value specifying the baseline wait duration (seconds) before
#'   executing the first retry step.
#' @param ... Optional arguments passed directly through to the underlying `api_fn`.
#'
#' @return The evaluated data structure returned by `api_fn` upon successful execution.
#' @noRd
api_with_retry <- function(api_fn, max_attempts, initial_delay, ...) {
  current_delay <- initial_delay

  for (attempt in 1:max_attempts) {
    # Establish dynamic cli status text
    cli::cli_progress_message(
      "Attempt {attempt}/{max_attempts}: Downloading data..."
    )

    # Evaluate the API expression
    result <- rlang::try_fetch(
      expr = {
        data <- api_fn(...)
        list(status = "success", data = data)
      },
      # Catch and evaluate any error condition object (cnd)
      error = function(cnd) {
        error_msg <- conditionMessage(cnd)

        # Look for explicit HTTP 404 Indicators
        is_404 <- stringr::str_detect(
          error_msg,
          stringr::regex("404|Not Found", ignore_case = TRUE)
        )

        if (isTRUE(is_404)) {
          # Terminate execution immediately for unrecoverable 404 errors
          cli::cli_abort(
            "Server returned {.val 404 Not Found}. Execution halted.",
            parent = cnd
          )
        }

        # Handle all other transient network errors (like partial transfers)
        cli::cli_alert_warning(
          "Attempt {.val {attempt}} failed: {.emph {error_msg}}"
        )
        return(list(status = "transient_error", data = NULL))
      }
    )

    # Evaluate outcomes and handle control flow
    # Return data if successful
    if (result$status == "success") {
      return(result$data)
    }

    # If it is a transient error, execute exponential backoff with jitter
    if (attempt < max_attempts) {
      jitter <- runif(1, 0, 1)
      sleep_time <- current_delay + jitter

      cli::cli_alert_info(
        "Waiting {.val {round(sleep_time, 2)}} seconds before retrying..."
      )
      Sys.sleep(sleep_time)

      current_delay <- current_delay * 2
    } else {
      cli::cli_abort(
        "Download failed permanently after {.val {max_attempts}} attempts."
      )
    }
  }
}

#' Fetch and Extract a Specific Data Entity from an EDI Repository Package
#'
#' Interrogates an explicit Environmental Data Initiative (EDI) data package
#' publication to pull a target data entity string matching a defined regular
#' expression profile. Once resolved, the function downloads the asset over the
#' wire as a raw binary vector and parses it into a standard tabular layout.
#'
#' @param edi_pkg_id Character or numeric string identifying the baseline EDI scope-identifier
#'   for the data package.
#' @param rev_num Integer specifying the target revision number of the EDI data package.
#' @param entity_regex Character string regular expression used to uniquely target and match the
#'   intended data entity from the data package inventory.
#' @param config_path Character string path to the survey's YAML configuration file used for
#'   contextual error reporting.
#' @param max_attempts Integer defining the ceiling for total execution attempts passed to
#'   `api_with_retry`. Defaults to 5.
#' @param initial_delay Numeric value specifying the baseline wait duration (seconds) before
#'   executing the first retry step passed to `api_with_retry`. Defaults to 2.
#'
#' @return A processed data frame or tibble layout populated by the target dataset content.
#' @noRd
import_edi_data <- function(
  edi_pkg_id,
  rev_num,
  entity_regex,
  config_path,
  max_attempts = 5,
  initial_delay = 2
) {
  # Define full ID for current EDI data publication
  edi_full_id <- paste("edi", edi_pkg_id, rev_num, sep = ".")

  # Obtain all data entities for EDI data package
  df_edi_data_ent_all <- api_with_retry(
    api_fn = EDIutils::read_data_entity_names,
    max_attempts = max_attempts,
    initial_delay = initial_delay,
    packageId = edi_full_id
  )

  # Subset to data entity specified by its regex pattern
  entity_name <- subset_data_input(
    df_edi_data_ent_all$entityName,
    entity_regex,
    config_path
  )

  # Set up progress message
  status <- new.env()
  status$msg <- ""
  step_id <- cli::cli_progress_step(
    msg = "Downloading data entity: {.file {entity_name}}",
    msg_done = "Downloading data entity: {.file {entity_name}}{status$msg}"
  )

  # Define EDI entityId to download
  entity_id <- df_edi_data_ent_all$entityId[
    which(df_edi_data_ent_all$entityName == entity_name)
  ]

  # Download specified entity as a binary file
  data_entity_raw <- api_with_retry(
    api_fn = EDIutils::read_data_entity,
    max_attempts = max_attempts,
    initial_delay = initial_delay,
    packageId = edi_full_id,
    entityId = entity_id
  )

  # Import binary file using read_csv
  df_data <- read_csv_text(data_entity_raw)

  # Finish progress message
  status$msg <- cli::format_inline(
    "... Data import complete ({nrow(df_data)} rows)"
  )
  cli::cli_progress_done(id = step_id)

  return(df_data)
}

#' Import Water Quality Lab or Field Data from the CNRA Data Portal
#'
#' @param station_num Character string or numeric value identifying the target monitoring station.
#' @param start_date Character string or Date object representing the lower date bound (inclusive).
#' @param end_date Character string or Date object representing the upper date bound (inclusive).
#'   Defaults to the current system date (`Sys.Date()`).
#' @param type Character string indicating data source context; must be either "lab"
#'   or "field".
#' @param max_attempts Integer defining the ceiling for total execution attempts assigned to
#'   `httr2::req_retry`. Defaults to 5.
#'
#' @return A tidy tibble of case-standardized monitoring records. If no records match the query
#'   parameters, an empty tibble is returned safely.
#' @noRd
import_cnra_data <- function(
  station_num,
  start_date,
  end_date = Sys.Date(),
  type = c("lab", "field"),
  max_attempts = 5
) {
  # Determine resource ID based on type (lab vs. field data)
  type <- rlang::arg_match(type)
  resource_ids <- c(
    lab = "a9e7ef50-54c3-4031-8e44-aa46f3c660fe",
    field = "1911e554-37ab-44c0-89b0-8d7044dd891d"
  )
  target_resource_id <- resource_ids[type]

  # Ensure dates are treated strictly as standard ISO character strings
  start_date_str <- as.character(start_date)
  end_date_str <- as.character(end_date)

  # Set up progress message
  status <- new.env()
  status$msg <- ""
  type_label <- tools::toTitleCase(type)

  step_id <- cli::cli_progress_step(
    msg = "Querying CNRA {type_label} Data Portal: Station {.val {station_num}}",
    msg_done = "Querying CNRA {type_label} Data Portal: Station {.val {station_num}}{status$msg}"
  )

  # Build SQL Query based on type (lab vs. field data)
  if (type == "lab") {
    # Parses the "MM/DD/YYYY HH:MM" text format on the server
    date_filter_clause <- glue::glue_sql(
      'to_date("SAMPLE_DATE", \'MM/DD/YYYY\') >= {start_date_str}::date ',
      'AND to_date("SAMPLE_DATE", \'MM/DD/YYYY\') <= {end_date_str}::date',
      .con = DBI::ANSI()
    )
  } else {
    # Parses the standard "YYYY-MM-DD HH:MM:SS" timestamp natively
    date_filter_clause <- glue::glue_sql(
      '"SAMPLE_DATE"::date >= {start_date_str}::date ',
      'AND "SAMPLE_DATE"::date <= {end_date_str}::date',
      .con = DBI::ANSI()
    )
  }

  # Combine elements into a definitive SQL string
  sql_query_cnra <- glue::glue_sql(
    'SELECT * FROM {`target_resource_id`} ',
    'WHERE "STATION_NUMBER"::text = {station_num} ',
    'AND ',
    date_filter_clause,
    .con = DBI::ANSI()
  )

  # Execute web API call
  # Build the URL request
  req <- httr2::request(
    "https://data.cnra.ca.gov/api/3/action/datastore_search_sql"
  ) |>
    httr2::req_url_query(sql = sql_query_cnra) |>
    httr2::req_retry(max_tries = max_attempts)

  # Perform the query safely and catch server responses
  resp <- rlang::try_fetch(
    expr = {
      httr2::req_perform(req)
    },
    error = function(cnd) {
      status$msg <- cli::format_inline("... {.emph CRITICAL SYSTEM ERROR}")
      cli::cli_progress_done(id = step_id)
      cli::cli_abort(
        "CNRA Datastore API request execution failed",
        parent = cnd
      )
    }
  )

  # Extract and parse the payload structures cleanly
  payload <- httr2::resp_body_json(resp)
  records <- purrr::pluck(payload, "result", "records")

  # Safety check: Handle instances where zero records match your filter criteria
  if (length(records) == 0) {
    status$msg <- cli::format_inline("... {.strong No matching records found}")
    cli::cli_progress_done(id = step_id)
    return(tibble::tibble())
  }

  # Clean output
  df_data <- records |>
    tibble::tibble() |>
    tidyr::unnest_wider(1)

  # Standardize case of data frame column names
  names(df_data) <- tolower(names(df_data))

  df_data <- df_data |>
    dplyr::select(!tidyselect::any_of("_full_text"))

  # Close progress reporting boundaries cleanly
  status$msg <- cli::format_inline(
    "... Data import complete ({nrow(df_data)} rows)"
  )
  cli::cli_progress_done(id = step_id)

  return(df_data)
}
