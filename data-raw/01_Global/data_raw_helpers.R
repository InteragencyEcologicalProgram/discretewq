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

# Data download -----------------------------------------------------------

# Get update information for a dataset including the ID of EDI publication used in last discretewq
  # update and date the dataset was last updated
# Use the optional data_type argument if there is more than one EDI publication used for the survey
  # such as "fish" or "zoop"
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

  # Create attribute getter functions
  edi_id_name <- if (!is.null(data_type)) paste0("edi_id_", data_type) else "edi_id"
  get_edi_id <- attr_getter(edi_id_name)
  get_last_update <- attr_getter("last_update")

  # Extract attributes
  edi_id <- get_edi_id(get(df_data))
  last_update <- get_last_update(get(df_data))

  lst(edi_id, last_update)
}

# Get ID for most current revision of EDI publication and check if it differs from the revision
  # used in the last discretewq update
get_latest_edi_id <- function(survey, data_type = NULL) {
  # Get ID of EDI publication used in last discretewq update
  ls_update_info <- get_update_info(survey, data_type)

  # If edi_id of dataset is NULL, return NULL
  if (is.null(ls_update_info$edi_id)) return(NULL)

  # Otherwise, get EDI ID for latest revision of EDI publication
  edi_scope <- "edi"
  edi_package_id <- as.numeric(str_extract(ls_update_info$edi_id, "(?<=edi\\.)\\d+(?=\\.)"))
  latest_rev <- list_data_package_revisions(
    scope = edi_scope,
    identifier = edi_package_id,
    filter = "newest"
  )

  # Check if EDI ID for latest revision differs from the revision used in the last
    # discretewq update
  update_rev <- as.numeric(str_extract(ls_update_info$edi_id, "(?<=edi\\.\\d{2,5}\\.)\\d+"))
  edi_id_diff <- if (latest_rev > update_rev) TRUE else FALSE

  # Return EDI ID's for last update, latest revision, and difference status
  list(
    "edi_id_update" = paste(edi_scope, edi_package_id, update_rev, sep = "."),
    "edi_id_latest" = paste(edi_scope, edi_package_id, latest_rev, sep = "."),
    "edi_id_diff" = edi_id_diff
  )
}

# Download specified data entities from most current revision of EDI publication and save raw bytes
  # files to a temporary directory - stops if its the same as the last discretewq update
# Provide an EDI ID in the optional edi_id argument to download data from a specific package
  # revision.
get_edi_data <- function(survey, data_type = NULL, edi_id = NULL) {
  # Obtain EDI data package ID for most recent revision if edi_id is NULL
  edi_id <- edi_id %||% get_latest_edi_id(survey, data_type)

  # If edi_id is still NULL, survey data isn't available on EDI - abort with message
  if (is.null(edi_id)) {
    abort(paste(
      "Survey data isn't available on EDI. If this is the first time using data from EDI to",
      "update this dataset,\nuse optional 'edi_id' argument to specify ID."
    ))
  }

  # If the data on EDI is the same version that was used during the last discretewq update,
    # stop and provide message
  if (is.list(edi_id) && isFALSE(edi_id$edi_id_diff)) {
    inform(c(
      "i" = paste0(
        "The EDI data package hasn't been updated since the last discretewq update (",
        edi_id$edi_id_update, ")"
      ),
      "*" = "There is no need to update this data set"
    ))
    return(NULL)
  }

  # Otherwise, proceed with downloading data from EDI data package
  if (is.list(edi_id)) {
    inform(c(
      "i" = paste0(
        "The EDI data package has been updated since the last discretewq update (",
        edi_id$edi_id_update, ")"
      ),
      "*" = paste0("Proceeding with downloading data from ", edi_id$edi_id_latest, "\n")
    ))
    edi_id <- edi_id$edi_id_latest
  }

  # Get data entity names for specified EDI ID
  df_edi_data_ent_all <- read_data_entity_names(edi_id)
  inform(c(
    "i" = paste0(
      "Data entities for ", edi_id, " include:\n",
      paste(df_edi_data_ent_all$entityName, collapse = "\n"), "\n"
    ))
  )

  # Rename survey if data_type isn't NULL
  survey <- if (!is.null(data_type)) paste(survey, data_type, sep = "_") else survey

  # Import data entities metadata table and filter to survey
  df_edi_data_ent <-
    read_csv("data-raw/01_Global/Data_entity_metadata.csv", show_col_types = FALSE) %>%
    dplyr::filter(Survey == survey, Source == "EDI")

  # Subset to desired data entities
  df_edi_data_ent_sub <- df_edi_data_ent %>%
    mutate(
      Data_entity,
      Data_entity_edi_name = map_chr(
        Data_entity_regex,
        \(x) str_subset(df_edi_data_ent_all$entityName, x)
      ),
      Data_entity_empty = map_lgl(Data_entity_edi_name, is_empty),
      .keep = "used"
    )

  # Check if regex patterns for desired data entities return expected results
  if (any(df_edi_data_ent_sub$Data_entity_empty)) {
    df_edi_data_ent_fail <- df_edi_data_ent_sub %>% dplyr::filter(Data_entity_empty)
    abort(c(
      "x" = paste(
        "The following data entity regex patterns did not find a data entity:",
        paste(df_edi_data_ent_fail$Data_entity_regex, collapse = ", ")
      ),
      "i" = "Update data entity regex patterns in EDI_data_entity_metadata.csv before proceeding"
    ))
  } else {
    inform(c(
      "i" = paste0(
        "Downloading data entities:\n",
        paste(df_edi_data_ent_sub$Data_entity_edi_name, collapse = "\n"), "\n"
      )
    ))
  }

  # Proceed with downloading desired data entities from EDI data package
  temp_dir <- tempdir()
  df_edi_data_ent_final <- df_edi_data_ent_sub %>%
    left_join(df_edi_data_ent_all, by = join_by(Data_entity_edi_name == entityName)) %>%
    # Clean up data entity names to be used as file names
    mutate(
      # Remove .csv file extensions from entity names if they exist
      Data_entity_edi_name = str_remove(Data_entity_edi_name, "\\.csv$"),
      # Add survey suffix and .bin file extension
      Data_entity_edi_name = paste0(Data_entity_edi_name, "_", survey, ".bin"),
      # Add file path to file name
      Data_entity_fp = file.path(temp_dir, Data_entity_edi_name)
    )

  ls_edi_data_raw <-
    map(df_edi_data_ent_final$entityId, \(x) read_data_entity(edi_id, entityId = x)) %>%
    set_names(df_edi_data_ent_final$Data_entity_edi_name)

  for (i in 1:length(ls_edi_data_raw)) {
    file_raw <- file.path(temp_dir, glue::glue("{names(ls_edi_data_raw)[i]}"))
    con <- file(file_raw, "wb")
    writeBin(ls_edi_data_raw[[i]], con)
    close(con)
  }

  inform(c("v" = "All files successfully downloaded to temporary directory"))
  return(list(
    "edi_id" = edi_id,
    "df_edi_files" = df_edi_data_ent_final %>% select(Data_entity, Data_entity_fp)
  ))
}

# Download discrete lab data from CNRA data portal and save csv file to a temporary directory
get_cnra_data_lab <- function(station_num, start_date, end_date = today()) {
  # Generate HTTP request URL
  base_url <- "https://data.cnra.ca.gov/api/3/action/datastore_search_sql?sql="
  sql_query_lab <- paste0(
    r"(SELECT * from "a9e7ef50-54c3-4031-8e44-aa46f3c660fe" WHERE "station_number" = ')",
    station_num, r"(' AND "sample_date" BETWEEN ')", start_date, "' AND '", end_date, "'"
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
  df_data_lab %>% write_csv(
    file = file.path(tempdir(), paste0(station_num, "_cnra_lab_data.csv"))
  )

  inform(c("v" = paste(station_num, "successfully downloaded to temporary directory")))
}

# Download discrete field measurement data from CNRA data portal and save csv file to a temporary
  # directory
get_cnra_data_field <- function(station_num, start_date, end_date = today()) {
  # Generate HTTP request URL
  base_url <- "https://data.cnra.ca.gov/api/3/action/datastore_search_sql?sql="
  sql_query_field <- paste0(
    r"(SELECT * from "1911e554-37ab-44c0-89b0-8d7044dd891d" WHERE "station_number" = ')",
    station_num, r"(' AND "sample_date" BETWEEN ')", start_date, "' AND '", end_date, "'"
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
  df_data_field %>% write_csv(
    file = file.path(tempdir(), paste0(station_num, "_cnra_field_data.csv"))
  )

  inform(c("v" = paste(station_num, "successfully downloaded to temporary directory")))
}

# Download specified data entities from a Science Base item and save files to a temporary
  # directory
get_scibase_data <- function(item_id, entity_regex) {
  # Compile all data entities for specified Science Base item ID
  df_sb_files <- sbtools::item_list_files(item_id)
  inform(c(
    "i" = paste0(
      "Data entities for ", item_id, " include:\n",
      paste(df_sb_files$fname, collapse = "\n"), "\n"
    ))
  )

  # Subset to desired data entities
  sb_ent_sub <- map_chr(entity_regex, \(x) str_subset(df_sb_files$fname, x))
  inform(c(
    "i" = paste0("Downloading data entities:\n", paste(sb_ent_sub, collapse = "\n"), "\n")
  ))

  # Proceed with downloading desired data entities from Science Base item
  map(
    sb_ent_sub,
    \(x) sbtools::item_file_download(
      sb_id = item_id,
      names = x,
      destinations = file.path(tempdir(), x),
      overwrite_file = TRUE
    )
  )

  inform(c("v" = "All files successfully downloaded to temporary directory"))
}

# Download discrete water quality data from USGS samples API and save files to a temporary directory
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
  df_data %>% write_csv(
    file = file.path(tempdir(), paste0(site_id, "_usgs_samples_data.csv"))
  )

  inform(c("v" = paste(site_id, "successfully downloaded to temporary directory")))
}


# Import and clean data ---------------------------------------------------

# Import raw data using specified read function. Imports all columns as text.
# >>> used internally in import_proc_data
import_raw_data <- function(filepath, import_fun = c("read_csv", "read_excel")) {
  import_fun <- arg_match(import_fun)

  inform(c("i" = paste("Attempting to import:", basename(filepath))))

  # Check import function and import data
  df_data_raw <- switch(import_fun,
    read_csv = read_csv(filepath, col_types = list(.default = "c"), na = c("", "NA", "NA:NA")),
    read_excel = read_excel(filepath, col_types = "text")
  )

  inform(c("v" = "Data import complete\n"))
  return(df_data_raw)
}

# Check for parsing errors when converting columns from text to numeric or POSIXct
# >>> used internally in standardize_col_meta and date and time conversion functions
check_parsing <- function(df_data_orig, df_data_parsed, filepath) {
  # Check if the number of NA values are the same in each column between the original and parsed
    # dataframes
  df_parse_check <- list(df_data_orig, df_data_parsed) %>%
    map(
      \(df) summarize(df, across(everything(), \(x) sum(is.na(x)))) %>%
        pivot_longer(everything(), names_to = "col_name", values_to = "num_NA")
    ) %>%
    reduce(\(x, y) left_join(x, y, by = join_by(col_name), suffix = c("_orig", "_parsed"))) %>%
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
      "i" = "Fix problem underlying parsing error before proceeding",
      "i" = paste0("Raw data can be found at the following path:\n", filepath)
    ))
  }
}

# Perform checks on column names and types, then apply standardized formatting
# >>> used internally in import_proc_data
standardize_col_meta <- function(df_data, df_col_meta, filepath) {
  inform(c("i" = paste("Checking column names and types in", basename(filepath))))

  # Check if expected columns in df_col_meta exist in df_data
  df_col_check <- df_col_meta %>%
    mutate(col_name_check = map_lgl(Col_name_exp, \(x) any(names(df_data) == x)))

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
      "i" = "Update expected column names in Data_column_metadata.csv before proceeding"
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
      reduce(\(x, y) check_parsing(x, y, filepath))
  }

  # Rename columns according to new names in data column metadata table
  names(df_data_c) <- df_col_meta$Col_name_new
  inform(c("v" = "Column names standardized\n"))
  return(df_data_c)
}

# Parse Date column if the date format is specified
# >>> used internally in import_proc_data
convert_date <- function(df_data, date_fmt, filepath) {
  # Skip if date_fmt isn't specified
  if (is.na(date_fmt)) return(df_data)

  inform(c("i" = paste("Converting Date column in", basename(filepath))))
  df_data_c <- df_data %>% mutate(Date = date(parse_date_time(Date, date_fmt)))

  # Run parsing check
  list(df_data, df_data_c) %>%
    map(\(x) select(x, Date)) %>%
    reduce(\(x, y) check_parsing(x, y, filepath))

  cat("\n")
  return(df_data_c)
}

# Parse Time column and format as HH:MM:SS if the time format is specified
# >>> used internally in import_proc_data
convert_time <- function(df_data, time_fmt, filepath) {
  # Skip if time_fmt isn't specified
  if (is.na(time_fmt)) return(df_data)

  inform(c("i" = paste("Converting Time column in", basename(filepath))))
  df_data_c <- df_data %>% mutate(Time = format(parse_date_time(Time, time_fmt), "%H:%M:%S"))

  # Run parsing check
  list(df_data, df_data_c) %>%
    map(\(x) select(x, Time)) %>%
    reduce(\(x, y) check_parsing(x, y, filepath))

  cat("\n")
  return(df_data_c)
}

# Parse Datetime column if the datetime format is specified. Also create a Date column from the
  # parsed Datetime column.
# >>> used internally in import_proc_data
convert_datetime <- function(df_data, datetime_fmt, timezone, filepath) {
  # Skip if datetime_fmt isn't specified
  if (is.na(datetime_fmt)) return(df_data)

  inform(c("i" = paste("Converting Datetime column in", basename(filepath))))
  df_data_c <- df_data %>%
    mutate(
      Datetime = parse_date_time(Datetime, datetime_fmt, tz = timezone),
      # Make sure Datetime is in local time (America/Los_Angeles) for all surveys
      Datetime = with_tz(Datetime, tzone = "America/Los_Angeles"),
      Date = date(Datetime),
      .before = Datetime
    )

  # Run parsing check
  list(df_data, df_data_c) %>%
    map(\(x) select(x, Datetime)) %>%
    reduce(\(x, y) check_parsing(x, y, filepath))

  inform(c("i" = "Creating Date column from parsed Datetime column\n"))
  return(df_data_c)
}

# Convert depth from feet to meters
# >>> used internally in import_proc_data
convert_depth <- function(df_data, depth_unit = c("meters", "feet"), filepath) {
  # Skip if depth_unit isn't specified
  if (is.na(depth_unit)) return(df_data)

  inform(c("i" = paste("Checking Depth column in", basename(filepath))))

  # Argument checking
  depth_unit <- arg_match(depth_unit)

  # Skip if depth_unit is meters
  if (depth_unit == "meters") {
    inform(c("v" = "Depth column is in meters. No conversion necessary.\n"))
    return(df_data)
    # Otherwise, if depth_unit is feet, convert to meters
  } else if (depth_unit == "feet") {
    inform(c("v" = "Depth column converted from feet to meters.\n"))
    df_data %>% mutate(Depth = Depth * 0.3048)
  }
}

# Convert Secchi depth from meters to centimeters
# >>> used internally in import_proc_data
convert_secchi <- function(df_data, secchi_unit = c("centimeters", "meters"), filepath) {
  # Skip if secchi_unit isn't specified
  if (is.na(secchi_unit)) return(df_data)

  inform(c("i" = paste("Checking Secchi column in", basename(filepath))))

  # Argument checking
  secchi_unit <- arg_match(secchi_unit)

  # Skip if secchi_unit is centimeters
  if (secchi_unit == "centimeters") {
    inform(c("v" = "Secchi column is in centimeters. No conversion necessary.\n"))
    return(df_data)
    # Otherwise, if secchi_unit is meters, convert to centimeters
  } else if (secchi_unit == "meters") {
    inform(c("v" = "Secchi column converted from meters to centimeters.\n"))
    df_data %>% mutate(Secchi = Secchi * 100)
  }
}

# Standardize CDFW tide codes
# >>> used internally in import_proc_data
standardize_tide_code <- function(df_data, tide_lgl, filepath) {
  # Skip if tide_lgl is FALSE
  if (isFALSE(tide_lgl)) return(df_data)

  inform(c("i" = paste("Standardizing Tide column in", basename(filepath))))
  df_data %>%
    mutate(Tide = case_match(Tide, 4 ~ "Flood", 3 ~ "Low Slack", 2 ~ "Ebb", 1 ~ "High Slack"))
}

# Convert coordinates from DMS to decimal degrees
# >>> used internally in import_proc_data, can also be used independently
convert_lat_long <- function(
  df_data,
  coord_comp = c("DM", "DMS"),
  filepath = NULL
) {
  # Skip if coord_comp isn't specified
  if (is.na(coord_comp)) return(df_data)

  # Generate message if filepath is provided
  if (!is.null(filepath)) {
    inform(
      c(
        "i" = paste(
          "Converting Latitude and Longitude columns to decimal degrees in",
          basename(filepath)
        )
      )
    )
  }

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

# Import raw data while running checks for column names and types, then apply standardized
  # formatting
# df_files argument is only for data downloaded from EDI, keep it as NULL if data isn't from EDI
import_proc_data <- function(survey, data_type = NULL, df_files = NULL) {
  # Rename survey if data_type isn't NULL
  survey <- if (!is.null(data_type)) paste(survey, data_type, sep = "_") else survey

  # Import data entities and data column metadata tables and filter to survey
  ndf_data_ent <-
    c(
      "data-raw/01_Global/Data_entity_metadata.csv",
      "data-raw/01_Global/Data_column_metadata.csv"
    ) %>%
    map(\(x) read_csv(x, show_col_types = FALSE) %>% dplyr::filter(Survey == survey)) %>%
    # Nest data column metadata table under entities table
    reduce(\(x, y) nest_join(x, y, by = join_by(Survey, Data_entity), name = "df_col_meta"))

  # Determine file paths for importing data
  # Create empty list to contain metadata
  ls_data <- list()

  # Files from EDI
  if (any(ndf_data_ent$Source == "EDI")) {
    ndf_data_ent_edi <- ndf_data_ent %>%
      dplyr::filter(Source == "EDI") %>%
      # Add file paths to EDI data downloaded to temporary directory
      left_join(df_files, by = join_by(Data_entity))

    ls_data <- append(ls_data, list(ndf_data_ent_edi))
  }

  # Other files saved in temporary directory but not from EDI
  if (any(!str_detect(ndf_data_ent$Source, "^data-raw|EDI"))) {
    ndf_data_ent_tempdir <- ndf_data_ent %>%
      dplyr::filter(!str_detect(Source, "^data-raw|EDI")) %>%
      # Determine file paths for data entities on temporary directory
      mutate(
        Data_entity_fp = map(
          Data_entity_regex,
          \(x) list.files(tempdir(), pattern = x, full.names = TRUE)
        )
      ) %>%
      # Unnest Data_entity_fp in case it contains more than one file path
      unnest(Data_entity_fp)

    ls_data <- append(ls_data, list(ndf_data_ent_tempdir))
  }

  # Files saved locally in data-raw
  if (any(str_detect(ndf_data_ent$Source, "^data-raw"))) {
    ndf_data_ent_dataraw <- ndf_data_ent %>%
      dplyr::filter(str_detect(Source, "^data-raw")) %>%
      # Determine file paths for data entities
      mutate(
        Data_entity_fp = map2(
          Source, Data_entity_regex,
          \(x, y) list.files(x, pattern = y, full.names = TRUE)
        )
      ) %>%
      # Unnest Data_entity_fp in case it contains more than one file path
      unnest(Data_entity_fp)

    ls_data <- append(ls_data, list(ndf_data_ent_dataraw))
  }

  # Combine metadata, import files and process each one
  ndf_data_ent_c <- list_rbind(ls_data) %>%
    mutate(
      # Import data
      df_data = map2(Data_entity_fp, Read_function, import_raw_data),
      # Perform checks and minor processing on column names and types
      df_data_c = pmap(list(df_data, df_col_meta, Data_entity_fp), standardize_col_meta),
      # Convert Date columns where specified
      df_data_c = pmap(list(df_data_c, Date_format, Data_entity_fp), convert_date),
      # Convert Time columns where specified
      df_data_c = pmap(list(df_data_c, Time_format, Data_entity_fp), convert_time),
      # Convert Datetime columns where specified
      df_data_c = pmap(
        list(df_data_c, Datetime_format, Time_zone, Data_entity_fp),
        convert_datetime
      ),
      # Convert Depth from feet to meters if necessary
      df_data_c = pmap(list(df_data_c, Depth_unit, Data_entity_fp), convert_depth),
      # Convert Secchi depth from meters to centimeters if necessary
      df_data_c = pmap(list(df_data_c, Secchi_unit, Data_entity_fp), convert_secchi),
      # Standardize CDFW tide code if necessary
      df_data_c = pmap(list(df_data_c, Standardize_tide, Data_entity_fp), standardize_tide_code),
      # Convert coordinates from DMS to decimal degrees if necessary
      df_data_c = pmap(list(df_data_c, Convert_coordinates, Data_entity_fp), convert_lat_long)
    )

  # Combine any data frames that share a common Data_entity and return a list of processed
    # data entities
  ndf_data_ent_c %>%
    select(Data_entity, df_data_c) %>%
    nest(data = df_data_c) %>%
    mutate(data = map(data, \(x) unnest(x, df_data_c))) %>%
    deframe()
}

# Standardize parameter names for data structured in long format
standardize_param <- function(df_data, survey, type = c("Field", "Lab", "Both")) {
  type <- arg_match(type)

  # Import parameter table and filter to survey and type
  df_param <-
    read_csv("data-raw/01_Global/Parameters_metadata.csv", show_col_types = FALSE) %>%
    dplyr::filter(Survey == survey, Type == type) %>%
    select(Parameter_exp, Parameter_std, Units_exp)

  # Specify join spec for df_data >> df_param
  param_join <- join_by(Parameter == Parameter_exp, Units == Units_exp)

  # Check if expected parameters in df_param exist in df_data
  df_param_check <- df_data %>%
    distinct(Parameter, Units) %>%
    arrange(Parameter, Units) %>%
    full_join(df_param, by = param_join, keep = TRUE)

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
      "i" = "Update expected parameter names and units in Parameters_metadata.csv before proceeding"
    ))
  } else {
    inform(c("v" = "All parameter names are correct. Proceeding with standardizing names."))
  }

  # Generate message for results of check - removal of unwanted parameters
  if (any(is.na(df_param_check$Parameter_exp))) {
    df_param_rm <- df_param_check %>%
      dplyr::filter(is.na(Parameter_exp)) %>%
      mutate(Parameter = if_else(is.na(Units), Parameter, paste0(Parameter, " (", Units, ")")))

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
    left_join(df_param, by = param_join) %>%
    drop_na(Parameter_std) %>%
    select(-c(Parameter, Units))
}

# Create Datetime column from Date and Time columns making sure Datetime is in local time
  # (America/Los_Angeles)
combine_datetime <- function(df_data, dt_fmt = "Ymd HMS", timezone) {
  df_data %>%
    mutate(
      Datetime = parse_date_time(
        if_else(is.na(Time), NA_character_, paste(Date, Time)),
        orders = dt_fmt, tz = timezone
      ),
      # Make sure Datetime is in local time (America/Los_Angeles) for all surveys
      Datetime = with_tz(Datetime, tzone = "America/Los_Angeles"),
      .after = Date
    ) %>%
    select(-Time)
}

# Add Source column
add_source_col <- function(df_data, survey) {
  df_data %>% mutate(Source = survey, .before = 1)
}

# Separate coordinate strings to Degrees-Minutes (DM) or Degrees-Minutes-Seconds (DMS)
# in separate columns
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

# Resolve field vs. fixed sampling coordinates. Prefers fixed coordinates over field. Adds a
  # Field_coords column to indicate with coordinates were collected in the field if necessary.
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

# Delete rows where all measurements are NA
rm_rows_all_miss_data <- function(df_data) {
  # Define all possible columns with water quality measurements
  all_meas <- c(
    "Microcystis", "Secchi", "Temperature", "Temperature_bottom", "Conductivity",
    "Conductivity_bottom", "Salinity", "Salinity_bottom", "DissolvedOxygen",
    "DissolvedOxygen_bottom", "DissolvedOxygenPercent", "DissolvedOxygenPercent_bottom", "pH",
    "pH_bottom", "TurbidityNTU", "TurbidityNTU_bottom", "TurbidityFNU", "TurbidityFNU_bottom",
    "Chlorophyll", "Pheophytin", "TotAmmonia", "DissAmmonia", "DissNitrateNitrite", "TotPhos",
    "DissOrthophos", "TON", "DON", "TKN", "DissSilica", "TDS", "DissBromide", "DissCalcium",
    "TotChloride", "DissChloride", "TotAlkalinity", "DOC", "TOC", "TSS", "VSS"
  )
  df_data %>% dplyr::filter(!if_all(any_of(all_meas), is.na))
}

# Apply standardized column order, also removes any unnecessary columns
standardize_col_order <- function(df_data) {
  # Define standardized column order for all possible columns
  all_cols_order <- c(
    "Source", "Station", "Latitude", "Longitude", "Field_coords", "Date", "Datetime",
    "Depth", "Sample_depth_surface", "Sample_depth_nutr_surface", "Sample_depth_bottom", "Tide",
    "Microcystis", "Secchi", "Secchi_estimated", "Temperature", "Temperature_bottom",
    "Conductivity", "Conductivity_bottom", "Salinity", "Salinity_bottom", "DissolvedOxygen",
    "DissolvedOxygen_bottom", "DissolvedOxygenPercent", "DissolvedOxygenPercent_bottom", "pH",
    "pH_bottom", "TurbidityNTU", "TurbidityNTU_bottom", "TurbidityFNU", "TurbidityFNU_bottom",
    "Chlorophyll_Sign", "Chlorophyll", "Pheophytin_Sign", "Pheophytin", "TotAmmonia_Sign",
    "TotAmmonia", "DissAmmonia_Sign", "DissAmmonia", "DissNitrateNitrite_Sign",
    "DissNitrateNitrite", "TotPhos_Sign", "TotPhos", "DissOrthophos_Sign", "DissOrthophos",
    "TON_Sign", "TON", "DON_Sign", "DON", "TKN_Sign", "TKN", "DissSilica_Sign", "DissSilica",
    "TDS_Sign", "TDS", "DissBromide_Sign", "DissBromide", "DissCalcium_Sign", "DissCalcium",
    "TotChloride_Sign", "TotChloride", "DissChloride_Sign", "DissChloride", "TotAlkalinity_Sign",
    "TotAlkalinity", "DOC_Sign", "DOC", "TOC_Sign", "TOC", "TSS_Sign", "TSS", "VSS_Sign", "VSS",
    "Notes"
  )

  df_data %>% select(any_of(all_cols_order))
}


# Data documentation ------------------------------------------------------

# Update attribute information for a dataset including the ID of EDI publication used in the
  # current update and date the dataset was updated
# Provide a named vector to edi_id if there is more than one EDI publication used for the survey.
  # The names should reflect the data type such as "fish" or "zoop"
add_update_info <- function(df_data, edi_id = NULL) {
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

# Generate information for updating data dimensions for data documentation
# >>> used internally in documentation helpers
get_data_dims <- function(data_clean) {
  num_rows <- prettyNum(nrow(data_clean), big.mark = ',')
  inform(c(
    "i" = "Update data documentation in 'R/data.R':",
    "*" = glue::glue("@format a tibble with {num_rows} rows and {ncol(data_clean)} variables")
  ))
}

# Documentation helper for datasets derived from EDI data packages
document_helper_edi <- function(edi_id, data_clean) {
  # Generate EDI info
  # Citation for EDI data package
  edi_cit_raw <- read_data_package_citation(edi_id, access = FALSE)

  # DOI for EDI data package
  edi_doi <- read_data_package_doi(edi_id)

  # URL for EDI data package
  edi_url <- paste0("https://portal.edirepository.org/nis/metadataviewer?packageid=", edi_id)

  # Citation for README:
  edi_cit_README <- paste0(
    str_remove(edi_cit_raw, '(?<=Environmental Data Initiative\\.\\s)https.+'),
    "[", edi_doi, "](", edi_url, ")"
  )
  inform(c(
    "i" = "Update citation in 'README.Rmd':",
    "*" = paste(edi_cit_README, "\n")
  ))

  # Data documentation:
  get_data_dims(data_clean)
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
      "Update 'dataPackageID' in 'publication/metadata_templates/provenance.txt':", edi_id)
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
  get_data_dims(data_clean)
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
