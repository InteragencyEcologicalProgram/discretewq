# Global helper functions to help with data import and structure checking

# Internal helper functions ------------------------------------------------------------------

# Attribute getter functions used globally in helper functions
get_attr_file_name <- purrr::attr_getter("src_file")
get_attr_file_path <- purrr::attr_getter("src_path")

# For helper functions that take a data.frame as an input, check that the input is
# a non-empty data.frame
int_check_dataframe <- function(object) {
  # Extract name of object to be used in messages
  arg_name <- deparse(substitute(object))
  if (isFALSE(is.data.frame(object))) {
    rlang::abort(paste0(
      "'",
      arg_name,
      "' must be a data.frame, not a ",
      class(object)
    ))
  }
  if (rlang::is_empty(object)) {
    rlang::abort(paste0("'", arg_name, "' must contain data and not be empty"))
  }
}

# For helper functions that require specific column names in the input data.frame,
# check that those columns exist and that they are the correct type

# Check for parsing errors when converting columns from text to numeric or POSIXct
# >>> used internally in standardize_col_meta and date and time conversion functions
int_check_parsing <- function(df_data_orig, df_data_parsed) {
  # Obtain file path for source file for df_data_orig
  file_path <- get_attr_file_path(df_data_orig)

  # Check if the number of NA values are the same in each column between the original
  # and parsed dataframes
  df_parse_check <- list(df_data_orig, df_data_parsed) |>
    purrr::map(
      \(df) {
        dplyr::summarize(
          df,
          dplyr::across(tidyselect::everything(), \(x) sum(is.na(x)))
        ) |>
          tidyr::pivot_longer(
            tidyselect::everything(),
            names_to = "col_name",
            values_to = "num_NA"
          )
      }
    ) |>
    purrr::reduce(\(x, y) {
      dplyr::left_join(
        x,
        y,
        by = dplyr::join_by("col_name"),
        suffix = c("_orig", "_parsed")
      )
    }) |>
    dplyr::mutate(parse_check = .data$num_NA_orig == .data$num_NA_parsed)

  # Generate message for results of parsing check
  if (all(df_parse_check$parse_check)) {
    rlang::inform(c("v" = "All columns parsed correctly"))
  } else {
    df_parse_check_F <- df_parse_check |> dplyr::filter(!.data$parse_check)
    rlang::inform(c(
      "x" = paste(
        "The following columns did NOT parse correctly:",
        paste(df_parse_check_F$col_name, collapse = ", ")
      ),
      "i" = "Results of parsing check:"
    ))
    print(df_parse_check, n = 100)
    rlang::abort(c(
      "x" = "Data NOT converted",
      "!" = "Fix problem underlying parsing error before proceeding",
      "i" = paste0("Raw data can be found at the following path:\n", file_path)
    ))
  }
}

# Generate information for updating data dimensions for data documentation
# >>> used internally in documentation helpers
int_get_data_dims <- function(data_clean) {
  num_rows <- prettyNum(nrow(data_clean), big.mark = ',')
  rlang::inform(c(
    "i" = "Update data documentation in 'R/data.R':",
    "*" = glue::glue(
      "@format a tibble with {num_rows} rows and {ncol(data_clean)} variables"
    )
  ))
}


# Data download ------------------------------------------------------------------------------

# Import info from the yml configuration file for survey as a list
get_config_file <- function(survey) {
  # Obtain file name of configuration file for the specified survey
  filename <- paste0(
    dplyr::replace_values(
      survey,
      "20mm" ~ "twentymm",
      "Baystudy" ~ "baystudy",
      "Suisun" ~ "suisun"
    ),
    ".yml"
  )

  yaml::read_yaml(file.path("data-raw", filename))
}

# Get update information for a dataset including the ID of EDI publication used in
# last discretewq update and date the dataset was last updated
# Use the optional data_type argument if there is more than one EDI publication used
# for the survey such as "fish" or "zoop"
provide_update_info <- function(config_file) {
  # Extract information from configuration file for messaging
  survey_name <- config_file$survey_name
  source_data <- names(config_file$source_data)

  # Provide message with date of last update
  rlang::inform(c(
    "i" = paste(
      survey_name,
      "was last updated on",
      config_file$last_update
    )
  ))

  # Provide message for source data for survey
  source_data_c <- ifelse(
    length(source_data) > 1,
    paste(source_data, collapse = ", "),
    source_data
  )
  rlang::inform(c(
    "i" = paste("Source data for", survey_name, "is", source_data_c)
  ))
}

# Generic function to subset a vector of data entity files from a regex pattern
# Returns a string of a data entity file of length 1, generates error if returns
# anything else
subset_data_entity <- function(data_entities, regex_pattern) {
  data_entity_file <- stringr::str_subset(data_entities, regex_pattern)

  # Check if data entity file returns anything other than a single string and provide
  # message if so
  if (length(data_entity_file) == 0) {
    rlang::abort(c(
      "x" = paste(
        "The following data entity regex pattern did not find a data entity:",
        regex_pattern
      ),
      "i" = "Update data entity regex pattern before proceeding"
    ))
  } else if (length(data_entity_file) > 1) {
    rlang::abort(c(
      "x" = paste(
        "The following data entity regex pattern found more than one data entity:",
        regex_pattern
      ),
      "i" = "Update data entity regex pattern to find only one data entity before proceeding"
    ))
  } else {
    return(data_entity_file)
  }
}

extract_data_entity <- function(config_object) {
  purrr::chuck(config_object, "data_ent")
}

extract_entity_regex <- function(config_object) {
  data_entity <- extract_data_entity(config_object)
  data_entity_name <- purrr::map_chr(data_entity, "ent_name")
  data_entity_regex <- purrr::map(data_entity, "ent_regex")
  names(data_entity_regex) <- data_entity_name
  return(data_entity_regex)
}

extract_data_str <- function(config_object) {
  data_entity <- extract_data_entity(config_object)
  data_entity_name <- unlist(purrr::map_depth(data_entity, 1, "ent_name"))
  data_str <- purrr::map_depth(data_entity, 1, "data_str")
  names(data_str) <- data_entity_name
  return(data_str)
}

# Add functions for standard messaging

# Add function to save file to tempdir

## EDI ---------------------------------------------------------------------------------------

extract_edi_package_info <- function(edi_object) {
  pack_id <- purrr::chuck(edi_object, "pack_id")
  last_rev <- purrr::chuck(edi_object, "last_rev")
  static <- purrr::chuck(edi_object, "static")
  tibble::lst(pack_id, last_rev, static)
}

# Get ID for most current revision of EDI publication and check if it differs from
# the revision used in the last discretewq update
get_current_edi_id <- function(edi_object) {
  # Get EDI ID for current revision of EDI publication
  edi_scope <- "edi"
  edi_pack_id <- edi_object$pack_id
  last_rev <- edi_object$last_rev
  curr_rev <- EDIutils::list_data_package_revisions(
    scope = edi_scope,
    identifier = edi_pack_id,
    filter = "newest"
  )

  # Create full EDI ID for current revision
  edi_id_curr <- paste(edi_scope, edi_pack_id, curr_rev, sep = ".")

  # Provide message on EDI revision status
  rlang::inform(c(
    "i" = paste(
      paste("edi", edi_pack_id, last_rev, sep = "."),
      "was used during the last discretewq update"
    ),
    "i" = paste("The current revision of EDI publication is", edi_id_curr)
  ))

  # Provide further messaging on whether it differs from the revision used in the last
  # discretewq update
  if (curr_rev > last_rev) {
    rlang::inform(c(
      "i" = "The EDI data package has been updated since the last discretewq update",
      "!" = "Proceeding with updates to this data set"
    ))
    return(edi_id_curr)
  } else if (isTRUE(edi_object$static)) {
    # If static is set to TRUE, proceed even if the data package hasn't been updated
    rlang::inform(c(
      "i" = "The EDI data package hasn't been updated since the last discretewq update",
      "!" = "Proceeding with updates to this data set, because static = TRUE in configuration file"
    ))
    return(edi_id_curr)
  } else {
    # Stop execution if current revision isn't greater than revision used in the last
    # discretewq update, and if static is set to FALSE
    rlang::abort(c(
      "i" = "The EDI data package hasn't been updated since the last discretewq update",
      "x" = "Stopping updates to this data set"
    ))
  }
}

# Download specified data entities from most current revision of EDI publication and
# save raw bytes files to a temporary directory
# Returns a named vector of filepaths of the downloaded data
get_edi_data <- function(edi_object) {
  # Extract EDI package info
  edi_pack_info <- extract_edi_package_info(edi_object)

  # Determine most current revision of EDI publication and check if it differs from the revision
  # used in the last discretewq update
  edi_curr_rev <- get_current_edi_id(edi_pack_info)

  # Obtain all data entities for EDI data package
  df_edi_data_ent_all <- EDIutils::read_data_entity_names(edi_curr_rev)
  rlang::inform(c(
    "i" = paste0(
      "Data entities for ",
      edi_curr_rev,
      " include:\n",
      paste(df_edi_data_ent_all$entityName, collapse = "\n"),
      "\n"
    )
  ))

  # Extract data entity regex information and use to subset entities specified by their
  # regex patterns
  data_entity_regex <- extract_entity_regex(edi_object)
  edi_entity_names <- purrr::map(
    data_entity_regex,
    \(x) subset_data_entity(df_edi_data_ent_all$entityName, x)
  )

  # Provide message on which data entities will be downloaded
  rlang::inform(c(
    "i" = paste0(
      "Downloading data entities:\n",
      paste(edi_entity_names, collapse = "\n"),
      "\n"
    )
  ))

  # Define EDI entityIds to download
  edi_entity_names_idx <- match(
    edi_entity_names,
    df_edi_data_ent_all$entityName
  )
  edi_entity_ids <- df_edi_data_ent_all$entityId[edi_entity_names_idx]

  # Clean up edi_entity_names to be used as file names
  # Remove .csv file extensions from entity names if they exist, add edi_id suffix and
  # .bin file extension, and add file path to file name
  edi_id_suffix <- paste0(
    stringr::str_replace_all(edi_curr_rev, "\\.", "_"),
    ".bin"
  )
  edi_entity_names_c <- edi_entity_names |>
    purrr::map(tools::file_path_sans_ext) |>
    purrr::map(\(x) file.path(tempdir(), paste(x, edi_id_suffix, sep = "_")))

  # Add file names to EDI entityIds
  names(edi_entity_ids) <- edi_entity_names_c

  # Download each specified entity to a temporary directory
  ls_edi_data_raw <- purrr::map(
    edi_entity_ids,
    \(x) EDIutils::read_data_entity(edi_curr_rev, entityId = x)
  )

  for (i in 1:length(ls_edi_data_raw)) {
    file_raw <- names(ls_edi_data_raw)[i]
    con <- file(file_raw, "wb")
    writeBin(ls_edi_data_raw[[i]], con)
    close(con)
  }

  rlang::inform(c(
    "v" = "All files successfully downloaded to temporary directory"
  ))

  return(edi_entity_names_c)
}

run_standard_workflow <- function(survey) {
  # Import configuration file for survey
  ls_config <- get_config_file(survey)

  # Provide update messaging for survey
  provide_update_info(ls_config)

  # Extract source data information from configuration file
  ls_source_data <- ls_config$source_data
  source_data_names <- names(ls_source_data)
  ls_source_data_str <-
    purrr::map(ls_source_data, extract_data_str) |>
    purrr::list_flatten()

  # Create an empty list to place combined data paths for all data entities
  ls_data_path_all <- list()

  # Download data to temporary directory if necessary
  # EDI
  if (any(grepl("^EDI", source_data_names))) {
    # Extract info for EDI package(s)
    ls_edi_info <- purrr::keep_at(ls_source_data, \(x) grep("^EDI", x))
    # Download EDI data
    ls_edi_info <- purrr::map(ls_edi_info, get_edi_data)
    ls_data_path_all <- ls_edi_info
  }

  # Import data
  ls_data_all <-
    purrr::list_flatten(ls_data_path_all) |>
    purrr::map(import_raw_data)

  return(tibble::lst(ls_data_all, ls_source_data_str))

  # After combining all info into ls_data_path_all, and importing data into ls_data_all
  # Check for expected column names
  # Convert to numeric, date, and time formats
  # Rename columns to standardized names
  # Combine Date and Time columns if necessary
  # Convert Secchi and Depth columns if necessary
  # Convert tide codes if necessary
  # Convert lat-long coordinates if necessary
  # If data is in long format, check parameter names and units,
  # and then rename to standardized names
}

# Download discrete lab data from CNRA data portal and save csv file to a temporary
# directory
get_cnra_data_lab <- function(station_num, start_date, end_date = Sys.Date()) {
  # Generate HTTP request URL
  base_url <- "https://data.cnra.ca.gov/api/3/action/datastore_search_sql?sql="
  sql_query_lab <- paste0(
    r"(SELECT * from "a9e7ef50-54c3-4031-8e44-aa46f3c660fe" WHERE "station_number" = ')"#,
    # station_num,
    # r"(' AND "sample_date" BETWEEN ')",
    # start_date,
    # "' AND '",
    # end_date,
    # "'"
  )

  # Call API, transform JSON data into data frame
  df_data_lab <-
    jsonlite::read_json(utils::URLencode(paste0(base_url, sql_query_lab))) |>
    purrr::pluck("result", "records") |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    # remove the expensive full_text column
    dplyr::select(!tidyselect::any_of("_full_text"))

  # Save data to temporary directory
  df_data_lab |>
    readr::write_csv(
      file = file.path(tempdir(), paste0(station_num, "_cnra_lab_data.csv"))
    )

  rlang::inform(c(
    "v" = paste(station_num, "successfully downloaded to temporary directory")
  ))
}

# Download discrete field measurement data from CNRA data portal and save csv file to
# a temporary directory
get_cnra_data_field <- function(
  station_num,
  start_date,
  end_date = Sys.Date()
) {
  # Generate HTTP request URL
  base_url <- "https://data.cnra.ca.gov/api/3/action/datastore_search_sql?sql="
  sql_query_field <- paste0(
    r"(SELECT * from "1911e554-37ab-44c0-89b0-8d7044dd891d" WHERE "station_number" = ')",
    station_num,
    r"(' AND "sample_date" BETWEEN ')",
    start_date,
    "' AND '",
    end_date,
    "'"
  )

  # Call API, transform JSON data into data frame
  df_data_field <-
    jsonlite::read_json(utils::URLencode(paste0(base_url, sql_query_field))) |>
    purrr::pluck("result", "records") |>
    tibble::tibble() |>
    tidyr::unnest_wider(1) |>
    # remove the expensive full_text column
    dplyr::select(!tidyselect::any_of("_full_text"))

  # Save data to temporary directory
  df_data_field |>
    readr::write_csv(
      file = file.path(tempdir(), paste0(station_num, "_cnra_field_data.csv"))
    )

  rlang::inform(c(
    "v" = paste(station_num, "successfully downloaded to temporary directory")
  ))
}

# Download specified data entities from a Science Base item and save files to a
# temporary directory
# Returns a named vector of filepaths of the downloaded data
get_scibase_data <- function(item_id, entity_regex) {
  # Compile all data entities for specified Science Base item ID
  df_sb_files <- sbtools::item_list_files(item_id)
  rlang::inform(c(
    "i" = paste0(
      "Data entities for ",
      item_id,
      " include:\n",
      paste(df_sb_files$fname, collapse = "\n"),
      "\n"
    )
  ))

  # Subset to desired data entities
  df_sb_data_ent_sub <- entity_regex |>
    tibble::enframe(name = "data_entity", value = "data_entity_regex") |>
    dplyr::mutate(
      data_entity_name = purrr::map_chr(
        .data$data_entity_regex,
        \(x) subset_data_entity(df_sb_files$fname, x)
      ),
      # Add file path and item_id prefix to file name
      data_entity_fp = file.path(
        tempdir(),
        paste(
          stringr::str_sub(item_id, end = 6),
          .data$data_entity_name,
          sep = "_"
        )
      )
    )

  rlang::inform(c(
    "i" = paste0(
      "Downloading data entities:\n",
      paste(df_sb_data_ent_sub$data_entity_name, collapse = "\n"),
      "\n"
    )
  ))

  # Proceed with downloading desired data entities from Science Base item
  purrr::map2(
    df_sb_data_ent_sub$data_entity_name,
    df_sb_data_ent_sub$data_entity_fp,
    \(x, y) {
      sbtools::item_file_download(
        sb_id = item_id,
        names = x,
        destinations = y,
        overwrite_file = TRUE
      )
    }
  )

  rlang::inform(c(
    "v" = "All files successfully downloaded to temporary directory"
  ))

  df_sb_data_ent_sub |>
    dplyr::select(tidyselect::all_of(c("data_entity", "data_entity_fp"))) |>
    tibble::deframe()
}

# Download discrete water quality data from USGS samples API and save files to a
# temporary directory
get_usgs_samples_data <- function(site_id) {
  # Define parameters of interest
  parameters <- c(
    "Total ammonia (NH4+ and NH3) as nitrogen, water, filtered",
    "Nitrate plus nitrite as nitrogen, water, filtered",
    "Orthophosphate as phosphorus, water, filtered",
    "Dissolved organic carbon (DOC), water, filtered",
    "Chlorophyll a (Chl-a), phytoplankton in water, chromatographic-fluorometric method",
    "Dissolved oxygen (DO), water, unfiltered",
    "pH, water, unfiltered, field",
    "Temperature, water",
    "Specific conductance, water, unfiltered, normalized to 25 degrees Celsius"
  )

  # Download data
  df_data <- dataRetrieval::read_waterdata_samples(
    monitoringLocationIdentifier = site_id,
    characteristicUserSupplied = parameters,
    dataProfile = "narrow",
    convertType = FALSE
  )

  # Save data to temporary directory
  df_data |>
    readr::write_csv(
      file = file.path(tempdir(), paste0(site_id, "_usgs_samples_data.csv"))
    )

  rlang::inform(c(
    "v" = paste(site_id, "successfully downloaded to temporary directory")
  ))
}


# Import and clean data --------------------------------------------------

# Import raw data using read function determined by the file extension.
# Imports all columns as text.
import_raw_data <- function(filepath) {
  # Extract file name and provide message
  file_name <- basename(filepath)
  rlang::inform(c("i" = paste("Attempting to import:", file_name)))

  # Define import functions
  read_csv_text <- function() {
    readr::read_csv(
      filepath,
      col_types = list(.default = "c"),
      na = c("", "NA", "NA:NA")
    )
  }

  read_excel_text <- function() {
    readxl::read_excel(filepath, col_types = "text")
  }

  # Import data based on file extension
  file_ext <- tools::file_ext(filepath)
  df_data_raw <- switch(
    file_ext,
    csv = read_csv_text(),
    bin = read_csv_text(),
    xls = read_excel_text(),
    xlsx = read_excel_text()
  )

  rlang::inform(c("v" = "Data import complete\n"))

  # Add file_name and file_path attributes to df_data_raw
  df_data_raw <- structure(
    df_data_raw,
    src_file = file_name,
    src_path = filepath
  )

  return(df_data_raw)
}

# Import column metadata for a data entity to be used in the standardize_col_meta
# function
# Use the optional data_type argument if there is more than one EDI publication used
# for the survey such as "fish" or "zoop"
import_col_meta <- function(survey, entity_name, data_type = NULL) {
  # Change survey if data_type isn't NULL
  if (!is.null(data_type)) {
    survey <- paste(survey, data_type, sep = "_")
  }

  # Import data column metadata table and filter to survey and data entity
  readr::read_csv(
    "data-raw/01_Global/Data_column_metadata.csv",
    show_col_types = FALSE
  ) |>
    dplyr::filter(.data$Survey == survey, .data$Data_entity == entity_name)
}

# Perform checks on column names and types, then apply standardized column formatting
standardize_col_meta <- function(df_data, df_col_meta) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Obtain file name and path for source file for df_data
  file_name <- get_attr_file_name(df_data)
  file_path <- get_attr_file_path(df_data)

  rlang::inform(c("i" = paste("Checking column names and types in", file_name)))

  # Check if expected columns in df_col_meta exist in df_data
  df_col_check <- df_col_meta |>
    dplyr::mutate(
      col_name_check = purrr::map_lgl(
        .data$Col_name_exp,
        \(x) any(names(df_data) == x)
      )
    )

  # Generate message for results of check
  if (all(df_col_check$col_name_check)) {
    rlang::inform(c("v" = "All column names are correct"))
  } else {
    df_col_check_F <- df_col_check |> dplyr::filter(!.data$col_name_check)
    rlang::abort(c(
      "x" = paste(
        "The following expected columns are NOT present in the data frame:",
        paste(df_col_check_F$Col_name_exp, collapse = ", ")
      ),
      "!" = "Update expected column names in Data_column_metadata.csv before proceeding",
      "i" = paste0("Raw data can be found at the following path:\n", file_path)
    ))
  }

  # Select columns specified in data column metadata table
  df_data_c <- df_data |>
    dplyr::select(tidyselect::all_of(df_col_meta$Col_name_exp))

  # Convert specified columns to numeric if there are any
  if (any(df_col_meta$Col_type == "numeric")) {
    col_numeric <- df_col_meta |>
      dplyr::filter(.data$Col_type == "numeric") |>
      dplyr::pull(.data$Col_name_exp)

    df_data_c <- df_data_c |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(col_numeric), as.numeric))

    rlang::inform(c(
      "i" = paste(
        "Converting the following columns to numeric:",
        paste(col_numeric, collapse = ", ")
      ),
      "i" = "Checking for data parsing errors"
    ))

    # Check for parsing errors when converting columns to numeric
    list(df_data, df_data_c) |>
      purrr::map(\(x) dplyr::select(x, tidyselect::all_of(col_numeric))) |>
      purrr::reduce(int_check_parsing)
  }

  # Rename columns according to new names in data column metadata table
  names(df_data_c) <- df_col_meta$Col_name_new
  rlang::inform(c("v" = "Column names standardized\n"))
  return(df_data_c)
}

# Parse Date column to the specified date format
convert_date <- function(df_data, date_fmt) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Obtain file name for source file for df_data
  file_name <- get_attr_file_name(df_data)

  df_data_c <- df_data |>
    dplyr::mutate(
      Date = lubridate::date(lubridate::parse_date_time(.data$Date, date_fmt))
    )

  rlang::inform(c(
    "i" = paste(
      "Converting Date column from character to date format in",
      file_name
    )
  ))
  # Run parsing check
  list(df_data, df_data_c) |>
    purrr::map(\(x) dplyr::select(x, "Date")) |>
    purrr::reduce(int_check_parsing)

  cat("\n")
  return(df_data_c)
}

# Parse Time column and format as HH:MM:SS
convert_time <- function(df_data, time_fmt) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Obtain file name for source file for df_data
  file_name <- get_attr_file_name(df_data)

  df_data_c <- df_data |>
    dplyr::mutate(
      Time = format(
        lubridate::parse_date_time(.data$Time, time_fmt),
        "%H:%M:%S"
      )
    )

  rlang::inform(c("i" = paste("Standardizing Time column in", file_name)))
  # Run parsing check
  list(df_data, df_data_c) |>
    purrr::map(\(x) dplyr::select(x, "Time")) |>
    purrr::reduce(int_check_parsing)

  cat("\n")
  return(df_data_c)
}

# Parse Datetime column to the specified format making sure Datetime is in local time
# (America/Los_Angeles). Also create a Date column from the parsed Datetime column.
convert_datetime <- function(df_data, datetime_fmt, timezone) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Obtain file name for source file for df_data
  file_name <- get_attr_file_name(df_data)

  df_data_c <- df_data |>
    dplyr::mutate(
      Datetime = lubridate::parse_date_time(
        .data$Datetime,
        datetime_fmt,
        tz = timezone
      ),
      # Make sure Datetime is in local time (America/Los_Angeles) for all surveys
      Datetime = lubridate::with_tz(
        .data$Datetime,
        tzone = "America/Los_Angeles"
      ),
      Date = lubridate::date(.data$Datetime),
      .before = "Datetime"
    )

  rlang::inform(c(
    "i" = paste(
      "Converting Datetime column from character to datetime format in",
      file_name
    )
  ))
  # Run parsing check
  list(df_data, df_data_c) |>
    purrr::map(\(x) dplyr::select(x, "Datetime")) |>
    purrr::reduce(int_check_parsing)

  rlang::inform(c("i" = "Creating Date column from parsed Datetime column\n"))
  return(df_data_c)
}

# Create Datetime column from Date and Time columns making sure Datetime is in local
# time (America/Los_Angeles)
combine_datetime <- function(df_data, dt_fmt = "Ymd HMS", timezone) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  df_data |>
    dplyr::mutate(
      Datetime = lubridate::parse_date_time(
        dplyr::if_else(
          is.na(.data$Time),
          NA_character_,
          paste(.data$Date, .data$Time)
        ),
        orders = dt_fmt,
        tz = timezone
      ),
      # Make sure Datetime is in local time (America/Los_Angeles) for all surveys
      Datetime = lubridate::with_tz(
        .data$Datetime,
        tzone = "America/Los_Angeles"
      ),
      .after = "Date"
    ) |>
    dplyr::select(-"Time")
}

# Convert depth from feet to meters
convert_depth <- function(df_data, depth_unit = c("meters", "feet")) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Argument checking
  depth_unit <- rlang::arg_match(depth_unit)

  # Skip if depth_unit is meters
  if (depth_unit == "meters") {
    df_data <- df_data
    rlang::inform(c(
      "v" = "Depth column is in meters. No conversion necessary.\n"
    ))
    # Otherwise, if depth_unit is feet, convert to meters
  } else if (depth_unit == "feet") {
    df_data <- df_data |> dplyr::mutate(Depth = .data$Depth * 0.3048)
    rlang::inform(c("v" = "Depth column converted from feet to meters.\n"))
  }

  return(df_data)
}

# Convert Secchi depth from meters to centimeters
convert_secchi <- function(df_data, secchi_unit = c("cm", "meters")) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Argument checking
  secchi_unit <- rlang::arg_match(secchi_unit)

  # Skip if secchi_unit is centimeters
  if (secchi_unit == "cm") {
    df_data <- df_data
    rlang::inform(c(
      "v" = "Secchi column is in centimeters. No conversion necessary.\n"
    ))
    # Otherwise, if secchi_unit is meters, convert to centimeters
  } else if (secchi_unit == "meters") {
    df_data <- df_data |> dplyr::mutate(Secchi = .data$Secchi * 100)
    rlang::inform(c(
      "v" = "Secchi column converted from meters to centimeters.\n"
    ))
  }

  return(df_data)
}

# Standardize CDFW tide codes
standardize_tide_code <- function(df_data) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  df_data |>
    dplyr::mutate(
      Tide = dplyr::case_match(
        .data$Tide,
        1 ~ "High Slack",
        2 ~ "Ebb",
        3 ~ "Low Slack",
        4 ~ "Flood"
      )
    )
}

# Separate coordinate strings to Degrees-Minutes (DM) or Degrees-Minutes-Seconds
# (DMS) in separate columns. delim_chr defines the delimiter by which to separate
# strings and can either be a single space, hyphen or underscore and must be specified
separate_lat_long <- function(df_data, delim_chr, coord_comp = c("DM", "DMS")) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Define coordinate components to produce (DM vs DMS)
  coord_comp <- rlang::arg_match(coord_comp)
  delim_chr <- rlang::arg_match(delim_chr, values = c(" ", "-", "_"))

  if (coord_comp == "DM") {
    coord_comp_lat <- c("Lat_Deg", "Lat_Min")
    coord_comp_long <- c("Long_Deg", "Long_Min")
  } else if (coord_comp == "DMS") {
    coord_comp_lat <- c("Lat_Deg", "Lat_Min", "Lat_Sec")
    coord_comp_long <- c("Long_Deg", "Long_Min", "Long_Sec")
  }

  df_data |>
    tidyr::separate_wider_delim(
      "Latitude",
      delim = delim_chr,
      names = coord_comp_lat
    ) |>
    tidyr::separate_wider_delim(
      "Longitude",
      delim = delim_chr,
      names = coord_comp_long
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with(c("Lat_", "Long_")),
        as.numeric
      )
    )
}

# Convert coordinates from DMS to decimal degrees
convert_lat_long <- function(df_data, coord_comp = c("DM", "DMS")) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Argument checking
  coord_comp <- rlang::arg_match(coord_comp)

  # Convert coordinates based on coord_comp argument
  switch(
    coord_comp,
    DM = dplyr::mutate(
      df_data,
      Latitude = .data$Lat_Deg + .data$Lat_Min / 60,
      Longitude = (.data$Long_Deg + .data$Long_Min / 60) * -1,
      .keep = "unused"
    ),
    DMS = dplyr::mutate(
      df_data,
      Latitude = .data$Lat_Deg + .data$Lat_Min / 60 + .data$Lat_Sec / 3600,
      Longitude = (.data$Long_Deg +
        .data$Long_Min / 60 +
        .data$Long_Sec / 3600) *
        -1,
      .keep = "unused"
    )
  )
}

# Resolve field vs. fixed sampling coordinates. Prefers fixed coordinates over field.
# Adds a Field_coords column to indicate with coordinates were collected in the field
# if necessary.
resolve_lat_long <- function(df_data) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  df_data_c <- df_data |>
    dplyr::mutate(
      Field_coords = dplyr::case_when(
        is.na(.data$Latitude) & !is.na(.data$Latitude_field) ~ TRUE,
        is.na(.data$Longitude) & !is.na(.data$Longitude_field) ~ TRUE,
        .default = FALSE
      ),
      Latitude = dplyr::if_else(
        is.na(.data$Latitude),
        .data$Latitude_field,
        .data$Latitude
      ),
      Longitude = dplyr::if_else(
        is.na(.data$Longitude),
        .data$Longitude_field,
        .data$Longitude
      ),
      .keep = "unused"
    )

  # Remove Field_coords column if all values are FALSE
  if (all(df_data_c$Field_coords == FALSE)) {
    df_data_c <- df_data_c |> dplyr::select(-"Field_coords")
  }

  return(df_data_c)
}

# Import parameters metadata for a data entity to be used in the standardize_param
# function
import_param_meta <- function(survey, type = c("Field", "Lab", "Both")) {
  type <- rlang::arg_match(type)

  # Import parameter metadata table and filter to survey and type
  readr::read_csv(
    "data-raw/01_Global/Parameters_metadata.csv",
    show_col_types = FALSE
  ) |>
    dplyr::filter(.data$Survey == survey, .data$Type == type) |>
    dplyr::select(
      tidyselect::all_of(c("Parameter_exp", "Parameter_std", "Units_exp"))
    )
}

# Standardize parameter names for data structured in long format
standardize_param <- function(df_data, df_param_meta) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Obtain file name and path for source file for df_data
  file_name <- get_attr_file_name(df_data)
  file_path <- get_attr_file_path(df_data)

  rlang::inform(c(
    "i" = paste("Checking parameter names and units in", file_name)
  ))

  # Specify join spec for df_data >> df_param_meta
  param_join <- dplyr::join_by(
    "Parameter" == "Parameter_exp",
    "Units" == "Units_exp"
  )

  # Check if expected parameters in df_param_meta exist in df_data
  df_param_check <- df_data |>
    dplyr::distinct(.data$Parameter, .data$Units) |>
    dplyr::arrange(.data$Parameter, .data$Units) |>
    dplyr::full_join(df_param_meta, by = param_join, keep = TRUE)

  # Generate message for results of check - expected parameters
  if (any(is.na(df_param_check$Parameter))) {
    df_param_miss <- df_param_check |>
      dplyr::filter(is.na(.data$Parameter)) |>
      dplyr::mutate(
        Parameter_exp = paste0(.data$Parameter_exp, " (", .data$Units_exp, ")")
      )

    print(df_param_check, n = 100)
    rlang::abort(c(
      "x" = paste(
        "The following expected parameters are NOT present in the dataset:",
        paste(df_param_miss$Parameter_exp, collapse = ", ")
      ),
      "!" = "Update expected parameter names and units in Parameters_metadata.csv before proceeding",
      "i" = paste0("Raw data can be found at the following path:\n", file_path)
    ))
  } else {
    rlang::inform(c(
      "v" = "All parameter names are correct. Proceeding with standardizing names."
    ))
  }

  # Generate message for results of check - removal of unwanted parameters
  if (any(is.na(df_param_check$Parameter_exp))) {
    df_param_rm <- df_param_check |>
      dplyr::filter(is.na(.data$Parameter_exp)) |>
      dplyr::mutate(
        Parameter = dplyr::if_else(
          is.na(.data$Units),
          .data$Parameter,
          paste0(.data$Parameter, " (", .data$Units, ")")
        )
      )

    rlang::inform(c(
      "i" = paste0(
        "The following parameters were removed from the dataset:\n",
        paste(df_param_rm$Parameter, collapse = "\n")
      )
    ))
  } else {
    rlang::inform(c("i" = "No parameters removed from dataset"))
  }

  # Proceed with standardizing parameter names in data frame
  df_data |>
    dplyr::left_join(df_param_meta, by = param_join) |>
    tidyr::drop_na("Parameter_std") |>
    dplyr::select(!tidyselect::all_of(c("Parameter", "Units")))
}

# Add Source column
add_source_col <- function(df_data, survey) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  df_data |> dplyr::mutate(Source = survey, .before = 1)
}

# Delete rows where all measurements are NA
rm_rows_all_miss_data <- function(df_data) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Define all possible columns with water quality measurements
  all_meas <- c(
    "Microcystis",
    "Secchi",
    "Temperature",
    "Temperature_bottom",
    "Conductivity",
    "Conductivity_bottom",
    "Salinity",
    "Salinity_bottom",
    "DissolvedOxygen",
    "DissolvedOxygen_bottom",
    "DissolvedOxygenPercent",
    "DissolvedOxygenPercent_bottom",
    "pH",
    "pH_bottom",
    "TurbidityNTU",
    "TurbidityNTU_bottom",
    "TurbidityFNU",
    "TurbidityFNU_bottom",
    "Chlorophyll",
    "Pheophytin",
    "TotAmmonia",
    "DissAmmonia",
    "DissNitrateNitrite",
    "TotPhos",
    "DissOrthophos",
    "TON",
    "DON",
    "TKN",
    "DissSilica",
    "TDS",
    "DissBromide",
    "DissCalcium",
    "TotChloride",
    "DissChloride",
    "TotAlkalinity",
    "DOC",
    "TOC",
    "TSS",
    "VSS"
  )
  df_data |> dplyr::filter(!dplyr::if_all(tidyselect::any_of(all_meas), is.na))
}

# Apply standardized column order, also removes any unnecessary columns
standardize_col_order <- function(df_data) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Define standardized column order for all possible columns
  all_cols_order <- c(
    "Source",
    "Station",
    "Latitude",
    "Longitude",
    "Field_coords",
    "Date",
    "Datetime",
    "Depth",
    "Sample_depth_surface",
    "Sample_depth_nutr_surface",
    "Sample_depth_bottom",
    "Tide",
    "Microcystis",
    "Secchi",
    "Secchi_estimated",
    "Temperature",
    "Temperature_bottom",
    "Conductivity",
    "Conductivity_bottom",
    "Salinity",
    "Salinity_bottom",
    "DissolvedOxygen",
    "DissolvedOxygen_bottom",
    "DissolvedOxygenPercent",
    "DissolvedOxygenPercent_bottom",
    "pH",
    "pH_bottom",
    "TurbidityNTU",
    "TurbidityNTU_bottom",
    "TurbidityFNU",
    "TurbidityFNU_bottom",
    "Chlorophyll_Sign",
    "Chlorophyll",
    "Pheophytin_Sign",
    "Pheophytin",
    "TotAmmonia_Sign",
    "TotAmmonia",
    "DissAmmonia_Sign",
    "DissAmmonia",
    "DissNitrateNitrite_Sign",
    "DissNitrateNitrite",
    "TotPhos_Sign",
    "TotPhos",
    "DissOrthophos_Sign",
    "DissOrthophos",
    "TON_Sign",
    "TON",
    "DON_Sign",
    "DON",
    "TKN_Sign",
    "TKN",
    "DissSilica_Sign",
    "DissSilica",
    "TDS_Sign",
    "TDS",
    "DissBromide_Sign",
    "DissBromide",
    "DissCalcium_Sign",
    "DissCalcium",
    "TotChloride_Sign",
    "TotChloride",
    "DissChloride_Sign",
    "DissChloride",
    "TotAlkalinity_Sign",
    "TotAlkalinity",
    "DOC_Sign",
    "DOC",
    "TOC_Sign",
    "TOC",
    "TSS_Sign",
    "TSS",
    "VSS_Sign",
    "VSS",
    "Notes"
  )

  df_data |> dplyr::select(tidyselect::any_of(all_cols_order))
}


# Data documentation -----------------------------------------------------

# Update attribute information for a dataset including the ID of EDI publication used
# in the current update and date the dataset was updated
# Provide a named vector to edi_id if there is more than one EDI publication used for
# the survey. The names should reflect the data type such as "fish" or "zoop"
add_update_info <- function(df_data, edi_id = NULL) {
  # Check if df_data is a non-empty data.frame
  int_check_dataframe(df_data)

  # Remove carried-over src_file and src_path attributes
  attr(df_data, "src_file") <- NULL
  attr(df_data, "src_path") <- NULL

  # If edi_id is NULL, just update last_update date, otherwise update both
  if (is.null(edi_id)) {
    structure(df_data, last_update = Sys.Date())
  } else if (length(edi_id) == 1) {
    structure(df_data, edi_id = edi_id, last_update = Sys.Date())
  } else {
    ls_edi_id <- as.list(rlang::set_names(edi_id, \(x) paste0("edi_id_", x)))
    rlang::exec(structure, df_data, !!!ls_edi_id, last_update = Sys.Date())
  }
}

# Documentation helper for datasets derived from EDI data packages
document_helper_edi <- function(edi_id, data_clean) {
  # Check if df_clean is a non-empty data.frame
  int_check_dataframe(data_clean)

  # Generate EDI info
  # Citation for EDI data package
  edi_cit_raw <- EDIutils::read_data_package_citation(edi_id, access = FALSE)

  # DOI for EDI data package
  edi_doi <- EDIutils::read_data_package_doi(edi_id)

  # URL for EDI data package
  edi_url <- paste0(
    "https://portal.edirepository.org/nis/metadataviewer?packageid=",
    edi_id
  )

  # Citation for README:
  edi_cit_README <- paste0(
    stringr::str_remove(
      edi_cit_raw,
      '(?<=Environmental Data Initiative\\.\\s)https.+'
    ),
    "[",
    edi_doi,
    "](",
    edi_url,
    ")"
  )
  rlang::inform(c(
    "i" = "Update citation in 'README.Rmd':",
    "*" = paste(edi_cit_README, "\n")
  ))

  # Data documentation:
  int_get_data_dims(data_clean)
  rlang::inform(c("*" = paste("URL:", edi_url, "\n")))

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  rlang::inform(c(
    "i" = "Update 'Data_source' in 'publication/data_objects/Delta_Integrated_WQ_metadata.csv':",
    "*" = paste(edi_url, "\n")
  ))

  # metadata_templates/methods.docx
  rlang::inform(c(
    "i" = "Update '5. Data Sources' in 'publication/metadata_templates/methods.docx':",
    "*" = paste(edi_cit_raw, "\n")
  ))

  # metadata_templates/provenance.txt
  rlang::inform(c(
    "i" = paste(
      "Update 'dataPackageID' in 'publication/metadata_templates/provenance.txt':",
      edi_id
    )
  ))
}

# Documentation helper for datasets derived from sources other than EDI data packages
document_helper_other <- function(data_clean) {
  # Check if df_clean is a non-empty data.frame
  int_check_dataframe(data_clean)

  dataset_yr <- max(lubridate::year(data_clean$Date))

  # Citation for README:
  rlang::inform(c(
    "i" = "Update citation in 'README.Rmd':",
    "*" = paste("Year of dataset is", dataset_yr, "\n")
  ))

  # Data documentation:
  int_get_data_dims(data_clean)
  rlang::inform(c(
    "*" = paste(
      "Check URL for metadata and information on methods in @details section,",
      "and update if necessary\n"
    )
  ))

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  rlang::inform(c(
    "i" = paste(
      "Check 'Data_source' in 'publication/data_objects/Delta_Integrated_WQ_metadata.csv',",
      "and update if necessary\n"
    )
  ))

  # metadata_templates/methods.docx
  rlang::inform(c(
    "i" = "Update '5. Data Sources' in 'publication/metadata_templates/methods.docx':",
    "*" = paste("Year of dataset is", dataset_yr, "\n")
  ))

  # metadata_templates/provenance.txt
  rlang::inform(c(
    "i" = paste(
      "Check information in 'publication/metadata_templates/provenance.txt',",
      "and update if necessary"
    )
  ))
}
