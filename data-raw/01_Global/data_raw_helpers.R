# Global helper functions to help with data import and structure checking

library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(tibble)
library(rlang)

# Install EDIutils if its not installed already
if (!requireNamespace("EDIutils", quietly = TRUE)) {
  install.packages("EDIutils")
}
library(EDIutils)

# Install jsonlite if its not installed already
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

# Install sbtools if its not installed already
if (!requireNamespace("sbtools", quietly = TRUE)) {
  install.packages("sbtools")
}

# Install dataRetrieval (at least version 2.7.19) if its not installed already
if (!requireNamespace("dataRetrieval", quietly = TRUE)) {
  install.packages("dataRetrieval")
} else if (packageVersion("dataRetrieval") < "2.7.19") {
  install.packages("dataRetrieval")
}


# Internal helper functions ----------------------------------------------

# Attribute getter functions used globally in helper functions
get_attr_file_name <- attr_getter("src_file")
get_attr_file_path <- attr_getter("src_path")

# Check for parsing errors when converting columns from text to numeric or POSIXct
# >>> used internally in standardize_col_meta and date and time conversion functions
int_check_parsing <- function(df_data_orig, df_data_parsed) {
  # Obtain file path for source file for df_data_orig
  file_path <- get_attr_file_path(df_data_orig)

  # Check if the number of NA values are the same in each column between the original
  # and parsed dataframes
  df_parse_check <- list(df_data_orig, df_data_parsed) %>%
    map(
      \(df) {
        summarize(df, across(everything(), \(x) sum(is.na(x)))) %>%
          pivot_longer(
            everything(),
            names_to = "col_name",
            values_to = "num_NA"
          )
      }
    ) %>%
    reduce(\(x, y) {
      left_join(x, y, by = join_by(col_name), suffix = c("_orig", "_parsed"))
    }) %>%
    mutate(parse_check = num_NA_orig == num_NA_parsed)

  # Generate message for results of parsing check
  if (all(df_parse_check$parse_check)) {
    inform(c("v" = "All columns parsed correctly"))
  } else {
    df_parse_check_F <- df_parse_check %>% dplyr::filter(!parse_check)
    inform(c(
      "x" = paste(
        "The following columns did NOT parse correctly:",
        paste(df_parse_check_F$col_name, collapse = ", ")
      ),
      "i" = "Results of parsing check:"
    ))
    print(df_parse_check, n = 100)
    abort(c(
      "x" = "Data NOT imported",
      "!" = "Fix problem underlying parsing error before proceeding",
      "i" = paste0("Raw data can be found at the following path:\n", file_path)
    ))
  }
}

# Generate information for updating data dimensions for data documentation
# >>> used internally in documentation helpers
int_get_data_dims <- function(data_clean) {
  num_rows <- prettyNum(nrow(data_clean), big.mark = ',')
  inform(c(
    "i" = "Update data documentation in 'R/data.R':",
    "*" = glue::glue(
      "@format a tibble with {num_rows} rows and {ncol(data_clean)} variables"
    )
  ))
}


# Data download ----------------------------------------------------------

# Generic function to subset a vector of data entity files from a regex pattern
# Returns a string of a data entity file of length 1, generates error if returns
# anything else
subset_data_entity <- function(data_entities, regex_pattern) {
  data_entity_file <- str_subset(data_entities, regex_pattern)

  # Check if data entity file returns anything other than a single string and provide
  # message if so
  if (length(data_entity_file) == 0) {
    abort(c(
      "x" = paste(
        "The following data entity regex pattern did not find a data entity:",
        regex_pattern
      ),
      "i" = "Update data entity regex pattern before proceeding"
    ))
  } else if (length(data_entity_file) > 1) {
    abort(c(
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

# Get update information for a dataset including the ID of EDI publication used in
# last discretewq update and date the dataset was last updated
# Use the optional data_type argument if there is more than one EDI publication used
# for the survey such as "fish" or "zoop"
get_update_info <- function(survey, data_type = NULL) {
  # Obtain file name of .rda file for the specified survey
  filename <- case_match(
    survey,
    "20mm" ~ "twentymm.rda",
    "Baystudy" ~ "baystudy.rda",
    "DJFMP" ~ "DJFMP.rda",
    "DOP" ~ "DOP.rda",
    "EDSM" ~ "EDSM.rda",
    "EMP" ~ "EMP.rda",
    "FMWT" ~ "FMWT.rda",
    "NCRO" ~ "NCRO.rda",
    "SDO" ~ "SDO.rda",
    "SKT" ~ "SKT.rda",
    "SLS" ~ "SLS.rda",
    "STN" ~ "STN.rda",
    "Suisun" ~ "suisun.rda",
    "USBR" ~ "USBR.rda",
    "USGS_CAWSC" ~ "USGS_CAWSC.rda",
    "USGS_SFBS" ~ "USGS_SFBS.rda",
    "YBFMP" ~ "YBFMP.rda"
  )

  # Temporarily load survey dataset from data folder
  df_data <- load(file.path("data", filename))

  # Change edi_id name and survey if data_type isn't NULL
  if (!is.null(data_type)) {
    edi_id_name <- paste0("edi_id_", data_type)
    survey <- paste(survey, data_type, sep = "-")
  } else {
    edi_id_name <- "edi_id"
  }

  # Create attribute getter functions
  get_edi_id <- attr_getter(edi_id_name)
  get_last_update <- attr_getter("last_update")

  # Extract attributes
  edi_id <- get_edi_id(get(df_data))
  last_update <- get_last_update(get(df_data))

  # Provide message on current update status for survey
  if (is.null(edi_id)) {
    inform(c(
      "i" = paste(
        "Data from EDI wasn't used for",
        survey,
        "during the last discretewq update"
      ),
      "i" = paste(survey, "was last updated on", last_update)
    ))
  } else {
    inform(c(
      "i" = paste(
        "EDI revision",
        edi_id,
        "was used for",
        survey,
        "during the last discretewq update"
      ),
      "i" = paste(survey, "was last updated on", last_update)
    ))
  }

  lst(edi_id, last_update)
}

# Get ID for most current revision of EDI publication and check if it differs from
# the revision used in the last discretewq update
get_latest_edi_id <- function(edi_pack_id, last_rev = NULL) {
  # Get EDI ID for latest revision of EDI publication
  edi_scope <- "edi"
  latest_rev <- list_data_package_revisions(
    scope = edi_scope,
    identifier = edi_pack_id,
    filter = "newest"
  )

  # Create full EDI ID for latest revision
  edi_id_latest <- paste(edi_scope, edi_pack_id, latest_rev, sep = ".")

  # Provide message on EDI revision status
  inform(c(
    "i" = paste("The latest revision of EDI publication is", edi_id_latest)
  ))

  # Provide further messaging on whether it differs from the revision
  # used in the last discretewq update. Only do so if last_rev is provided.
  if (!is.null(last_rev)) {
    last_rev <- as.numeric(str_extract(last_rev, "(?<=edi\\.\\d{2,5}\\.)\\d+"))
    if (latest_rev > last_rev) {
      inform(c(
        "i" = "The EDI data package has been updated since the last discretewq update",
        "!" = "Proceed with running remainder of R script to update this data set"
      ))
    } else {
      inform(c(
        "i" = "The EDI data package hasn't been updated since the last discretewq update",
        "x" = "There is no need to update this data set"
      ))
    }
  }

  # Return EDI ID for latest revision
  return(edi_id_latest)
}

# Download specified data entities from most current revision of EDI publication and
# save raw bytes files to a temporary directory
# Returns a named vector of filepaths of the downloaded data
get_edi_data <- function(edi_id_latest, entity_regex) {
  # Get data entity names for specified EDI ID
  df_edi_data_ent_all <- read_data_entity_names(edi_id_latest)
  inform(c(
    "i" = paste0(
      "Data entities for ",
      edi_id_latest,
      " include:\n",
      paste(df_edi_data_ent_all$entityName, collapse = "\n"),
      "\n"
    )
  ))

  # Subset to desired data entities
  df_edi_data_ent_sub <- entity_regex %>%
    enframe(name = "data_entity", value = "data_entity_regex") %>%
    mutate(
      data_entity_name = map_chr(
        data_entity_regex,
        \(x) subset_data_entity(df_edi_data_ent_all$entityName, x)
      )
    )

  inform(c(
    "i" = paste0(
      "Downloading data entities:\n",
      paste(df_edi_data_ent_sub$data_entity_name, collapse = "\n"),
      "\n"
    )
  ))

  # Proceed with downloading desired data entities from EDI data package
  temp_dir <- tempdir()
  df_edi_data_ent_final <- df_edi_data_ent_sub %>%
    left_join(
      df_edi_data_ent_all,
      by = join_by(data_entity_name == entityName)
    ) %>%
    # Clean up data entity names to be used as file names
    mutate(
      # Remove .csv file extensions from entity names if they exist
      data_entity_name = str_remove(data_entity_name, "\\.csv$"),
      # Add edi_id suffix and .bin file extension
      data_entity_name = paste0(
        data_entity_name,
        "_",
        str_replace_all(edi_id_latest, "\\.", "_"),
        ".bin"
      ),
      # Add file path to file name
      data_entity_fp = file.path(temp_dir, data_entity_name)
    )

  ls_edi_data_raw <-
    map(
      df_edi_data_ent_final$entityId,
      \(x) read_data_entity(edi_id_latest, entityId = x)
    ) %>%
    set_names(df_edi_data_ent_final$data_entity_name)

  for (i in 1:length(ls_edi_data_raw)) {
    file_raw <- file.path(temp_dir, glue::glue("{names(ls_edi_data_raw)[i]}"))
    con <- file(file_raw, "wb")
    writeBin(ls_edi_data_raw[[i]], con)
    close(con)
  }

  inform(c("v" = "All files successfully downloaded to temporary directory"))

  df_edi_data_ent_final %>%
    select(data_entity, data_entity_fp) %>%
    deframe()
}

# Download discrete lab data from CNRA data portal and save csv file to a temporary
# directory
get_cnra_data_lab <- function(station_num, start_date, end_date = today()) {
  # Generate HTTP request URL
  base_url <- "https://data.cnra.ca.gov/api/3/action/datastore_search_sql?sql="
  sql_query_lab <- paste0(
    r"(SELECT * from "a9e7ef50-54c3-4031-8e44-aa46f3c660fe" WHERE "station_number" = ')",
    station_num,
    r"(' AND "sample_date" BETWEEN ')",
    start_date,
    "' AND '",
    end_date,
    "'"
  )

  # Call API, transform JSON data into data frame
  df_data_lab <-
    jsonlite::read_json(URLencode(paste0(base_url, sql_query_lab))) %>%
    pluck("result", "records") %>%
    tibble() %>%
    unnest_wider(1) %>%
    # remove the expensive full_text column
    select(!any_of("_full_text"))

  # Save data to temporary directory
  df_data_lab %>%
    write_csv(
      file = file.path(tempdir(), paste0(station_num, "_cnra_lab_data.csv"))
    )

  inform(c(
    "v" = paste(station_num, "successfully downloaded to temporary directory")
  ))
}

# Download discrete field measurement data from CNRA data portal and save csv file to
# a temporary directory
get_cnra_data_field <- function(station_num, start_date, end_date = today()) {
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
    jsonlite::read_json(URLencode(paste0(base_url, sql_query_field))) %>%
    pluck("result", "records") %>%
    tibble() %>%
    unnest_wider(1) %>%
    # remove the expensive full_text column
    select(!any_of("_full_text"))

  # Save data to temporary directory
  df_data_field %>%
    write_csv(
      file = file.path(tempdir(), paste0(station_num, "_cnra_field_data.csv"))
    )

  inform(c(
    "v" = paste(station_num, "successfully downloaded to temporary directory")
  ))
}

# Download specified data entities from a Science Base item and save files to a
# temporary directory
# Returns a named vector of filepaths of the downloaded data
get_scibase_data <- function(item_id, entity_regex) {
  # Compile all data entities for specified Science Base item ID
  df_sb_files <- sbtools::item_list_files(item_id)
  inform(c(
    "i" = paste0(
      "Data entities for ",
      item_id,
      " include:\n",
      paste(df_sb_files$fname, collapse = "\n"),
      "\n"
    )
  ))

  # Subset to desired data entities
  df_sb_data_ent_sub <- entity_regex %>%
    enframe(name = "data_entity", value = "data_entity_regex") %>%
    mutate(
      data_entity_name = map_chr(
        data_entity_regex,
        \(x) subset_data_entity(df_sb_files$fname, x)
      ),
      # Add file path and item_id prefix to file name
      data_entity_fp = file.path(
        tempdir(),
        paste(str_sub(item_id, end = 6), data_entity_name, sep = "_")
      )
    )

  inform(c(
    "i" = paste0(
      "Downloading data entities:\n",
      paste(df_sb_data_ent_sub$data_entity_name, collapse = "\n"),
      "\n"
    )
  ))

  # Proceed with downloading desired data entities from Science Base item
  map2(
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

  inform(c("v" = "All files successfully downloaded to temporary directory"))

  df_sb_data_ent_sub %>%
    select(data_entity, data_entity_fp) %>%
    deframe()
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
  df_data %>%
    write_csv(
      file = file.path(tempdir(), paste0(site_id, "_usgs_samples_data.csv"))
    )

  inform(c(
    "v" = paste(site_id, "successfully downloaded to temporary directory")
  ))
}


# Import and clean data --------------------------------------------------

# Import raw data using specified read function. Imports all columns as text.
# Defaults to read_csv
import_raw_data <- function(
  filepath,
  import_fun = c("read_csv", "read_excel")
) {
  import_fun <- arg_match(import_fun)

  file_name <- basename(filepath)
  inform(c("i" = paste("Attempting to import:", file_name)))

  # Check import function and import data
  df_data_raw <- switch(
    import_fun,
    read_csv = read_csv(
      filepath,
      col_types = list(.default = "c"),
      na = c("", "NA", "NA:NA")
    ),
    read_excel = read_excel(filepath, col_types = "text")
  )

  inform(c("v" = "Data import complete\n"))

  # Add file_name and file_path attritbutes to df_data_raw
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
  read_csv(
    "data-raw/01_Global/Data_column_metadata.csv",
    show_col_types = FALSE
  ) %>%
    dplyr::filter(Survey == survey, Data_entity == entity_name)
}

# Perform checks on column names and types, then apply standardized column formatting
standardize_col_meta <- function(df_data, df_col_meta) {
  # Obtain file name and path for source file for df_data
  file_name <- get_attr_file_name(df_data)
  file_path <- get_attr_file_path(df_data)

  inform(c("i" = paste("Checking column names and types in", file_name)))

  # Check if expected columns in df_col_meta exist in df_data
  df_col_check <- df_col_meta %>%
    mutate(
      col_name_check = map_lgl(Col_name_exp, \(x) any(names(df_data) == x))
    )

  # Generate message for results of check
  if (all(df_col_check$col_name_check)) {
    inform(c("v" = "All column names are correct"))
  } else {
    df_col_check_F <- df_col_check %>% dplyr::filter(!col_name_check)
    abort(c(
      "x" = paste(
        "The following expected columns are NOT present in the data frame:",
        paste(df_col_check_F$Col_name_exp, collapse = ", ")
      ),
      "!" = "Update expected column names in Data_column_metadata.csv before proceeding",
      "i" = paste0("Raw data can be found at the following path:\n", file_path)
    ))
  }

  # Select columns specified in data column metadata table
  df_data_c <- df_data %>% select(all_of(df_col_meta$Col_name_exp))

  # Convert specified columns to numeric if there are any
  if (any(df_col_meta$Col_type == "numeric")) {
    col_numeric <- df_col_meta %>%
      dplyr::filter(Col_type == "numeric") %>%
      pull(Col_name_exp)

    df_data_c <- df_data_c %>% mutate(across(all_of(col_numeric), as.numeric))

    inform(c(
      "i" = paste(
        "Converting the following columns to numeric:",
        paste(col_numeric, collapse = ", ")
      ),
      "i" = "Checking for data parsing errors"
    ))

    # Check for parsing errors when converting columns to numeric
    list(df_data, df_data_c) %>%
      map(\(x) select(x, all_of(col_numeric))) %>%
      reduce(int_check_parsing)
  }

  # Rename columns according to new names in data column metadata table
  names(df_data_c) <- df_col_meta$Col_name_new
  inform(c("v" = "Column names standardized\n"))
  return(df_data_c)
}

# Parse Date column to the specified date format
convert_date <- function(df_data, date_fmt) {
  # Obtain file name for source file for df_data
  file_name <- get_attr_file_name(df_data)

  df_data_c <- df_data %>%
    mutate(Date = date(parse_date_time(Date, date_fmt)))

  inform(c(
    "i" = paste(
      "Converting Date column from character to date format in",
      file_name
    )
  ))
  # Run parsing check
  list(df_data, df_data_c) %>%
    map(\(x) select(x, Date)) %>%
    reduce(int_check_parsing)

  cat("\n")
  return(df_data_c)
}

# Parse Time column and format as HH:MM:SS
convert_time <- function(df_data, time_fmt) {
  # Obtain file name for source file for df_data
  file_name <- get_attr_file_name(df_data)

  df_data_c <- df_data %>%
    mutate(Time = format(parse_date_time(Time, time_fmt), "%H:%M:%S"))

  inform(c("i" = paste("Standardizing Time column in", file_name)))
  # Run parsing check
  list(df_data, df_data_c) %>%
    map(\(x) select(x, Time)) %>%
    reduce(int_check_parsing)

  cat("\n")
  return(df_data_c)
}

# Parse Datetime column to the specified format making sure Datetime is in local time
# (America/Los_Angeles). Also create a Date column from the parsed Datetime column.
convert_datetime <- function(df_data, datetime_fmt, timezone) {
  # Obtain file name for source file for df_data
  file_name <- get_attr_file_name(df_data)

  df_data_c <- df_data %>%
    mutate(
      Datetime = parse_date_time(Datetime, datetime_fmt, tz = timezone),
      # Make sure Datetime is in local time (America/Los_Angeles) for all surveys
      Datetime = with_tz(Datetime, tzone = "America/Los_Angeles"),
      Date = date(Datetime),
      .before = Datetime
    )

  inform(c(
    "i" = paste(
      "Converting Datetime column from character to datetime format in",
      file_name
    )
  ))
  # Run parsing check
  list(df_data, df_data_c) %>%
    map(\(x) select(x, Datetime)) %>%
    reduce(int_check_parsing)

  inform(c("i" = "Creating Date column from parsed Datetime column\n"))
  return(df_data_c)
}

# Create Datetime column from Date and Time columns making sure Datetime is in local
# time (America/Los_Angeles)
combine_datetime <- function(df_data, dt_fmt = "Ymd HMS", timezone) {
  df_data %>%
    mutate(
      Datetime = parse_date_time(
        if_else(is.na(Time), NA_character_, paste(Date, Time)),
        orders = dt_fmt,
        tz = timezone
      ),
      # Make sure Datetime is in local time (America/Los_Angeles) for all surveys
      Datetime = with_tz(Datetime, tzone = "America/Los_Angeles"),
      .after = Date
    ) %>%
    select(-Time)
}

# Convert depth from feet to meters
convert_depth <- function(df_data, depth_unit = c("meters", "feet")) {
  # Argument checking
  depth_unit <- arg_match(depth_unit)

  # Skip if depth_unit is meters
  if (depth_unit == "meters") {
    df_data <- df_data
    inform(c("v" = "Depth column is in meters. No conversion necessary.\n"))
    # Otherwise, if depth_unit is feet, convert to meters
  } else if (depth_unit == "feet") {
    df_data <- df_data %>% mutate(Depth = Depth * 0.3048)
    inform(c("v" = "Depth column converted from feet to meters.\n"))
  }

  return(df_data)
}

# Convert Secchi depth from meters to centimeters
convert_secchi <- function(df_data, secchi_unit = c("cm", "meters")) {
  # Argument checking
  secchi_unit <- arg_match(secchi_unit)

  # Skip if secchi_unit is centimeters
  if (secchi_unit == "cm") {
    df_data <- df_data
    inform(c(
      "v" = "Secchi column is in centimeters. No conversion necessary.\n"
    ))
    # Otherwise, if secchi_unit is meters, convert to centimeters
  } else if (secchi_unit == "meters") {
    df_data <- df_data %>% mutate(Secchi = Secchi * 100)
    inform(c("v" = "Secchi column converted from meters to centimeters.\n"))
  }

  return(df_data)
}

# Standardize CDFW tide codes
standardize_tide_code <- function(df_data) {
  df_data %>%
    mutate(
      Tide = case_match(
        Tide,
        1 ~ "High Slack",
        2 ~ "Ebb",
        3 ~ "Low Slack",
        4 ~ "Flood"
      )
    )
}

# Separate coordinate strings to Degrees-Minutes (DM) or Degrees-Minutes-Seconds
# (DMS) in separate columns
separate_lat_long <- function(df_data, delim_chr, coord_comp = c("DM", "DMS")) {
  # Define coordinate components to produce (DM vs DMS)
  coord_comp <- arg_match(coord_comp)
  if (coord_comp == "DM") {
    coord_comp_lat <- c("Lat_Deg", "Lat_Min")
    coord_comp_long <- c("Long_Deg", "Long_Min")
  } else if (coord_comp == "DMS") {
    coord_comp_lat <- c("Lat_Deg", "Lat_Min", "Lat_Sec")
    coord_comp_long <- c("Long_Deg", "Long_Min", "Long_Sec")
  }

  df_data %>%
    separate_wider_delim(
      Latitude,
      delim = delim_chr,
      names = coord_comp_lat
    ) %>%
    separate_wider_delim(
      Longitude,
      delim = delim_chr,
      names = coord_comp_long
    ) %>%
    mutate(across(starts_with(c("Lat_", "Long_")), as.numeric))
}

# Convert coordinates from DMS to decimal degrees
convert_lat_long <- function(df_data, coord_comp = c("DM", "DMS")) {
  # Argument checking
  coord_comp <- arg_match(coord_comp)

  # Convert coordinates based on coord_comp argument
  switch(
    coord_comp,
    DM = mutate(
      df_data,
      Latitude = Lat_Deg + Lat_Min / 60,
      Longitude = Long_Deg - Long_Min / 60,
      .keep = "unused"
    ),
    DMS = mutate(
      df_data,
      Latitude = Lat_Deg + Lat_Min / 60 + Lat_Sec / 3600,
      Longitude = (Long_Deg + Long_Min / 60 + Long_Sec / 3600) * -1,
      .keep = "unused"
    )
  )
}

# Resolve field vs. fixed sampling coordinates. Prefers fixed coordinates over field.
# Adds a Field_coords column to indicate with coordinates were collected in the field
# if necessary.
resolve_lat_long <- function(df_data) {
  df_data_c <- df_data %>%
    mutate(
      Field_coords = case_when(
        is.na(Latitude) & !is.na(Latitude_field) ~ TRUE,
        is.na(Longitude) & !is.na(Longitude_field) ~ TRUE,
        .default = FALSE
      ),
      Latitude = if_else(is.na(Latitude), Latitude_field, Latitude),
      Longitude = if_else(is.na(Longitude), Longitude_field, Longitude),
      .keep = "unused"
    )

  # Remove Field_coords column if all values are FALSE
  if (all(df_data_c$Field_coords == FALSE)) {
    df_data_c <- df_data_c %>% select(-Field_coords)
  }

  return(df_data_c)
}

# Import parameters metadata for a data entity to be used in the standardize_param
# function
import_param_meta <- function(survey, type = c("Field", "Lab", "Both")) {
  type <- arg_match(type)

  # Import parameter metadata table and filter to survey and type
  read_csv(
    "data-raw/01_Global/Parameters_metadata.csv",
    show_col_types = FALSE
  ) %>%
    dplyr::filter(Survey == survey, Type == type) %>%
    select(Parameter_exp, Parameter_std, Units_exp)
}

# Standardize parameter names for data structured in long format
standardize_param <- function(df_data, df_param_meta) {
  # Obtain file name and path for source file for df_data
  file_name <- attr(df_data, "src_file")
  file_path <- attr(df_data, "src_path")

  inform(c("i" = paste("Checking parameter names and units in", file_name)))

  # Specify join spec for df_data >> df_param_meta
  param_join <- join_by(Parameter == Parameter_exp, Units == Units_exp)

  # Check if expected parameters in df_param_meta exist in df_data
  df_param_check <- df_data %>%
    distinct(Parameter, Units) %>%
    arrange(Parameter, Units) %>%
    full_join(df_param_meta, by = param_join, keep = TRUE)

  # Generate message for results of check - expected parameters
  if (any(is.na(df_param_check$Parameter))) {
    df_param_miss <- df_param_check %>%
      dplyr::filter(is.na(Parameter)) %>%
      mutate(Parameter_exp = paste0(Parameter_exp, " (", Units_exp, ")"))

    print(df_param_check, n = 100)
    abort(c(
      "x" = paste(
        "The following expected parameters are NOT present in the dataset:",
        paste(df_param_miss$Parameter_exp, collapse = ", ")
      ),
      "!" = "Update expected parameter names and units in Parameters_metadata.csv before proceeding",
      "i" = paste0("Raw data can be found at the following path:\n", file_path)
    ))
  } else {
    inform(c(
      "v" = "All parameter names are correct. Proceeding with standardizing names."
    ))
  }

  # Generate message for results of check - removal of unwanted parameters
  if (any(is.na(df_param_check$Parameter_exp))) {
    df_param_rm <- df_param_check %>%
      dplyr::filter(is.na(Parameter_exp)) %>%
      mutate(
        Parameter = if_else(
          is.na(Units),
          Parameter,
          paste0(Parameter, " (", Units, ")")
        )
      )

    inform(c(
      "i" = paste0(
        "The following parameters were removed from the dataset:\n",
        paste(df_param_rm$Parameter, collapse = "\n")
      )
    ))
  } else {
    inform(c("i" = "No parameters removed from dataset"))
  }

  # Proceed with standardizing parameter names in data frame
  df_data %>%
    left_join(df_param_meta, by = param_join) %>%
    drop_na(Parameter_std) %>%
    select(-c(Parameter, Units))
}

# Add Source column
add_source_col <- function(df_data, survey) {
  df_data %>% mutate(Source = survey, .before = 1)
}

# Delete rows where all measurements are NA
rm_rows_all_miss_data <- function(df_data) {
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
  df_data %>% dplyr::filter(!if_all(any_of(all_meas), is.na))
}

# Apply standardized column order, also removes any unnecessary columns
standardize_col_order <- function(df_data) {
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

  df_data %>% select(any_of(all_cols_order))
}


# Data documentation -----------------------------------------------------

# Update attribute information for a dataset including the ID of EDI publication used
# in the current update and date the dataset was updated
# Provide a named vector to edi_id if there is more than one EDI publication used for
# the survey. The names should reflect the data type such as "fish" or "zoop"
add_update_info <- function(df_data, edi_id = NULL) {
  # Remove carried-over src_file and src_path attributes
  attr(df_data, "src_file") <- NULL
  attr(df_data, "src_path") <- NULL

  # If edi_id is NULL, just update last_update date, otherwise update both
  if (is.null(edi_id)) {
    structure(df_data, last_update = Sys.Date())
  } else if (length(edi_id) == 1) {
    structure(df_data, edi_id = edi_id, last_update = Sys.Date())
  } else {
    ls_edi_id <- as.list(set_names(edi_id, \(x) paste0("edi_id_", x)))
    exec(structure, df_data, !!!ls_edi_id, last_update = Sys.Date())
  }
}

# Documentation helper for datasets derived from EDI data packages
document_helper_edi <- function(edi_id, data_clean) {
  # Generate EDI info
  # Citation for EDI data package
  edi_cit_raw <- read_data_package_citation(edi_id, access = FALSE)

  # DOI for EDI data package
  edi_doi <- read_data_package_doi(edi_id)

  # URL for EDI data package
  edi_url <- paste0(
    "https://portal.edirepository.org/nis/metadataviewer?packageid=",
    edi_id
  )

  # Citation for README:
  edi_cit_README <- paste0(
    str_remove(edi_cit_raw, '(?<=Environmental Data Initiative\\.\\s)https.+'),
    "[",
    edi_doi,
    "](",
    edi_url,
    ")"
  )
  inform(c(
    "i" = "Update citation in 'README.Rmd':",
    "*" = paste(edi_cit_README, "\n")
  ))

  # Data documentation:
  int_get_data_dims(data_clean)
  inform(c("*" = paste("URL:", edi_url, "\n")))

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  inform(c(
    "i" = "Update 'Data_source' in 'publication/data_objects/Delta_Integrated_WQ_metadata.csv':",
    "*" = paste(edi_url, "\n")
  ))

  # metadata_templates/methods.docx
  inform(c(
    "i" = "Update '5. Data Sources' in 'publication/metadata_templates/methods.docx':",
    "*" = paste(edi_cit_raw, "\n")
  ))

  # metadata_templates/provenance.txt
  inform(c(
    "i" = paste(
      "Update 'dataPackageID' in 'publication/metadata_templates/provenance.txt':",
      edi_id
    )
  ))
}

# Documentation helper for datasets derived from sources other than EDI data packages
document_helper_other <- function(data_clean) {
  dataset_yr <- max(year(data_clean$Date))

  # Citation for README:
  inform(c(
    "i" = "Update citation in 'README.Rmd':",
    "*" = paste("Year of dataset is", dataset_yr, "\n")
  ))

  # Data documentation:
  int_get_data_dims(data_clean)
  inform(c(
    "*" = paste(
      "Check URL for metadata and information on methods in @details section,",
      "and update if necessary\n"
    )
  ))

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  inform(c(
    "i" = paste(
      "Check 'Data_source' in 'publication/data_objects/Delta_Integrated_WQ_metadata.csv',",
      "and update if necessary\n"
    )
  ))

  # metadata_templates/methods.docx
  inform(c(
    "i" = "Update '5. Data Sources' in 'publication/metadata_templates/methods.docx':",
    "*" = paste("Year of dataset is", dataset_yr, "\n")
  ))

  # metadata_templates/provenance.txt
  inform(c(
    "i" = paste(
      "Check information in 'publication/metadata_templates/provenance.txt',",
      "and update if necessary"
    )
  ))
}
