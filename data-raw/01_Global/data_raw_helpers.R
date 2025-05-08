# Global helper functions to help with data import and structure checking

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(lubridate)
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


# Data download -----------------------------------------------------------

# Get ID for most current revision of EDI publication and check if it differs from the revision used
  # in the last discretewq update
get_latest_edi_id <- function(survey) {
  # Import EDI data package metadata table and filter to survey
  df_edi_meta <-
    read_csv("data-raw/01_Global/EDI_data_package_metadata.csv", show_col_types = FALSE) %>%
    dplyr::filter(Survey == survey)

  edi_scope <- "edi"
  edi_package_id <- df_edi_meta$Data_pack_id
  latest_rev <- list_data_package_revisions(
    scope = edi_scope,
    identifier = edi_package_id,
    filter = "newest"
  )

  latest_id <- paste(edi_scope, edi_package_id, latest_rev, sep = ".")
  inform(c("i" = paste("The latest revision for data package", edi_package_id, "is", latest_id)))

  if (latest_rev == df_edi_meta$Revision_last) {
    withr::with_options(
      list(show.error.messages = FALSE),
      {
        inform(c(
          "i" = paste0(
            "The EDI data package hasn't been updated since the last discretewq update (",
            df_edi_meta$Data_pack_id_full_last, ")"
          ),
          "*" = "There is no need to update this data set"
        ))
        stop()
      }
    )
  } else {
    inform(c(
      "i" = paste0(
        "The EDI data package has been updated since the last discretewq update (",
        df_edi_meta$Data_pack_id_full_last, ")"
      ),
      "*" = "Proceeding with updating this data set\n"
    ))
  }

  return(latest_id)
}

# Get data entity names for specified EDI ID
get_edi_data_entities <- function(edi_id) {
  df_data_ent <- read_data_entity_names(edi_id)
  inform(c(
    "i" = paste0(
      "Data entities for ", edi_id, " include:\n",
      paste(df_data_ent$entityName, collapse = "\n"), "\n"
    ))
  )
  return(df_data_ent$entityName)
}

# Download specified data entities from an EDI package and save raw bytes files to a temporary
  # directory
get_edi_data <- function(edi_id, entity_names) {
  df_data_ent <- read_data_entity_names(edi_id)
  df_data_ent_filt <- df_data_ent %>% dplyr::filter(entityName %in% entity_names)

  ls_data_raw <-
    map(df_data_ent_filt$entityId, \(x) read_data_entity(edi_id, entityId = x)) %>%
    set_names(df_data_ent_filt$entityName)

  temp_dir <- tempdir()
  for (i in 1:length(ls_data_raw)) {
    file_raw <- file.path(temp_dir, glue::glue("{names(ls_data_raw)[i]}.bin"))
    con <- file(file_raw, "wb")
    writeBin(ls_data_raw[[i]], con)
    close(con)
  }
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
  df_data_lab %>% write_excel_csv(
    file = file.path(tempdir(), paste0(station_num, "_lab_data.csv"))
  )
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
  df_data_field %>% write_excel_csv(
    file = file.path(tempdir(), paste0(station_num, "_field_data.csv"))
  )
}


# Import and clean data ---------------------------------------------------

# Import raw data while running checks for column names and types, then apply standardized
  # formatting
import_raw_data <- function(file, survey, entity) {
  # Import data with all columns as character
  df_data_raw <- read_csv(file, col_types = list(.default = "c"))

  # Import data column metadata table and filter to survey and entity
  df_col_meta <-
    read_csv("data-raw/01_Global/Data_column_metadata.csv", show_col_types = FALSE) %>%
    dplyr::filter(Survey == survey, Data_entity == entity)

  # Check if expected columns in df_col_meta exist in df_data_raw
  df_col_check <- df_col_meta %>%
    mutate(col_name_check = map_lgl(Col_name_exp, \(x) any(names(df_data_raw) == x)))

  # Generate message for results of check
  inform(c(
    "i" = paste(
      "Attempting to import:",
      stringr::str_extract(file, "(?<=/).+\\.(bin|csv)$")
    )
  ))

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
  df_data_c <- df_data_raw %>% select(all_of(df_col_meta$Col_name_exp))

  # Convert specified columns to numeric if there are any
  if (any(df_col_meta$Col_type == "numeric")) {
    col_numeric <- df_col_meta %>%
      dplyr::filter(Col_type == "numeric") %>%
      pull(Col_name_exp)

    df_data_c <- df_data_c %>% mutate(across(all_of(col_numeric), as.numeric))

    # Check for parsing errors when converting columns to numeric
    df_parse_check <-
      map(
        list(df_data_raw, df_data_c),
        \(df) summarize(df, across(all_of(col_numeric), \(x) sum(is.na(x)))) %>%
          pivot_longer(everything(), names_to = "col_name", values_to = "num_NA")
      ) %>%
      reduce(\(x, y) left_join(x, y, by = join_by(col_name), suffix = c("_raw", "_clean"))) %>%
      mutate(parse_check = num_NA_raw == num_NA_clean)

    # Generate message for results of parsing check
    if (all(df_parse_check$parse_check)) {
      inform(c("v" = "All numeric columns parsed correctly"))
    } else {
      df_parse_check_F <- df_parse_check %>% dplyr::filter(!parse_check)
      inform(c(
        "x" = paste(
          "The following columns did NOT parse as numeric correctly:",
          paste(df_parse_check_F$col_name, collapse = ", ")
        ),
        "i" = "Results of parsing check:"
      ))
      print(df_parse_check_F, n = 100)
      abort(c(
        "x" = "Data NOT imported",
        "i" = "Fix problem underlying parsing error before proceeding"
      ))
    }
  }

  # Rename columns according to new names in data column metadata table
  names(df_data_c) <- df_col_meta$Col_name_new
  inform(c("v" = "Data import complete\n"))
  return(df_data_c)
}

# Standardize analyte names for data structured in long format
standardize_analytes <- function(df, survey, type = c("Field", "Lab")) {
  type <- arg_match(type)

  # Import analytes table and filter to survey and type
  df_analytes <-
    read_csv("data-raw/01_Global/Analytes_metadata.csv", show_col_types = FALSE) %>%
    dplyr::filter(Survey == survey, Type == type) %>%
    select(Analyte_exp, Analyte_std, Units_exp)

  # Specify join spec for df >> df_analytes
  analytes_join <- join_by(Analyte == Analyte_exp, Units == Units_exp)

  # Check if expected analytes in df_analytes exist in df
  df_analytes_check <- df %>%
    distinct(Analyte, Units) %>%
    arrange(Analyte, Units) %>%
    full_join(df_analytes, by = analytes_join, keep = TRUE)

  # Generate message for results of check - expected analytes
  if (any(is.na(df_analytes_check$Analyte))) {
    df_analytes_miss <- df_analytes_check %>%
      dplyr::filter(is.na(Analyte)) %>%
      mutate(Analyte_exp = paste0(Analyte_exp, " (", Units_exp, ")"))

    print(df_analytes_check, n = 100)
    abort(c(
      "x" = paste(
        "The following expected analytes are NOT present in the dataset:",
        paste(df_analytes_miss$Analyte_exp, collapse = ", ")
      ),
      "i" = "Update expected analyte names and units in Analytes_metadata.csv before proceeding"
    ))
  } else {
    inform(c("v" = "All analyte names are correct. Proceeding with standardizing names."))
  }

  # Generate message for results of check - removal of unwanted analytes
  if (any(is.na(df_analytes_check$Analyte_exp))) {
    df_analytes_rm <- df_analytes_check %>%
      dplyr::filter(is.na(Analyte_exp)) %>%
      mutate(Analyte = if_else(is.na(Units), Analyte, paste0(Analyte, " (", Units, ")")))

    inform(c(
      "i" = paste0(
        "The following analytes were removed from the dataset:\n",
        paste(df_analytes_rm$Analyte, collapse = "\n")
      )
    ))
  } else {
    inform(c("i" = "No analytes removed from dataset"))
  }

  # Proceed with standardizing analyte names in df
  df %>%
    left_join(df_analytes, by = analytes_join) %>%
    drop_na(Analyte_std) %>%
    select(-c(Analyte, Units))
}

# Parse Datetime columns
# If the data has separate Date and Time columns, combine them into a Datetime column and parse the
  # Date column
# If the data has a Datetime column, parse it, and create a Date column
convert_datetime <- function(df_data,
                             date_format,
                             time_format,
                             timezone,
                             entity_name = NULL) {
  # Skip if data doesn't have a Date format specified - for EDI automated workflow
  if (is.na(date_format)) return(df_data)

  # Skip if data doesn't have a Datetime column or Date and Time columns
  if (!(any(names(df_data) == "Datetime") | (any(names(df_data) == "Date") & any(names(df_data) == "Time")))) {
    warn(c(
      "Data does NOT have either a Datetime column or Date and Time columns",
      "i" = "Returning data without changes"
    ))
    return(df_data)
  }

  # Proceed with parsing depending on which date/time columns df_data contains
  if (any(names(df_data) == "Datetime")) {
    df_data_c <- df_data %>%
      mutate(
        Datetime = parse_date_time(Datetime, paste(date_format, time_format), tz = timezone),
        # Make sure Datetime is in local time (America/Los_Angeles) for all surveys
        Datetime = with_tz(Datetime, tzone = "America/Los_Angeles"),
        Date = date(Datetime),
        .before = Datetime
      )

    # Define Datetime columns before and after conversion for parsing check
    col_datetime_orig <- "Datetime"
    col_datetime_conv <- "Datetime"
  } else if (any(names(df_data) == "Date") & any(names(df_data) == "Time")) {
    df_data_c <- df_data %>%
      mutate(
        Date = date(parse_date_time(Date, date_format, tz = timezone)),
        Datetime = parse_date_time(
          if_else(is.na(Time), NA_character_, paste(Date, Time)),
          paste("Ymd", time_format),
          tz = timezone
        ),
        # Make sure Datetime is in local time (America/Los_Angeles) for all surveys
        Datetime = with_tz(Datetime, tzone = "America/Los_Angeles"),
        .keep = "unused", .after = Date
      )

    # Define Datetime columns before and after conversion for parsing check
    col_datetime_orig <- c("Date", "Time")
    col_datetime_conv <- c("Date", "Datetime")
  }

  # Check for parsing errors when converting columns to datetime
  df_parse_check <-
    map2(
      list(df_data, df_data_c),
      list(col_datetime_orig, col_datetime_conv),
      \(df, col_check) summarize(df, across(all_of(col_check), \(x) sum(is.na(x)))) %>%
        pivot_longer(everything(), names_to = "col_name", values_to = "num_NA")
    ) %>%
    map2(c("_raw", "_clean"), \(df, suff) rename_with(df, \(x) paste0(x, suff))) %>%
    reduce(bind_cols) %>%
    mutate(parse_check = num_NA_raw == num_NA_clean)

  # Generate message for results of parsing check
  if (all(df_parse_check$parse_check)) {
    if (is.null(entity_name)) {
      parse_msg <- "All datetime columns parsed correctly"
    } else {
      parse_msg <- paste("All datetime columns parsed correctly in", entity_name)
    }
    inform(c("v" = parse_msg))
  } else {
    if (is.null(entity_name)) {
      parse_msg <- "Datetime columns did NOT parse correctly"
    } else {
      parse_msg <- paste("Datetime columns did NOT parse correctly in", entity_name)
    }
    inform(c(
      "x" = parse_msg,
      "i" = "Results of parsing check:"
    ))
    print(df_parse_check, n = 100)
    inform(c("i" = "Date and Time columns from dataset:"))
    print(df_data %>% select(all_of(col_datetime_orig)) %>% slice_head(n = 5))
    abort(c("i" = "Fix problem underlying parsing error before proceeding"))
  }

  return(df_data_c)
}

# Convert depth from feet to meters
convert_depth <- function(df_data, depth_unit, entity_name = NULL) {
  # Skip if data doesn't have Depth unit specified
  if (is.na(depth_unit)) {
    return(df_data)
  # Skip if Depth unit is meters
  } else if (depth_unit == "meters") {
    return(df_data)
  # Otherwise, if Depth unit is feet, convert to meters
  } else if (depth_unit == "feet"){
    if (is.null(entity_name)) {
      depth_msg <- "Depth column converted from feet to meters"
    } else {
      depth_msg <- paste("Depth column in", entity_name, "converted from feet to meters")
    }
    inform(c("v" = depth_msg))
    df_data %>% mutate(Depth = Depth * 0.3048)
  }
}

# Convert Secchi depth from meters to centimeters
convert_secchi <- function(df_data, secchi_unit, entity_name = NULL) {
  # Skip if data doesn't have Secchi unit specified
  if (is.na(secchi_unit)) {
    return(df_data)
    # Skip if Secchi unit is centimeters
  } else if (secchi_unit == "centimeters") {
    return(df_data)
    # Otherwise, if Secchi unit is meters, convert to centimeters
  } else if (secchi_unit == "meters"){
    if (is.null(entity_name)) {
      secchi_msg <- "Secchi depth column converted from meters to centimeters"
    } else {
      secchi_msg <- paste(
        "Secchi depth column in", entity_name, "converted from meters to centimeters"
      )
    }
    inform(c("v" = secchi_msg))
    df_data %>% mutate(Secchi = Secchi * 100)
  }
}

# Add Source column
add_source_col <- function(df_data, survey) {
  df_data %>% mutate(Source = survey, .before = 1)
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

# Import and process data derived from an EDI package
# This is a generalized workflow to be used across all EDI data packages
# Returns a list of processed data entities and the EDI ID
import_proc_edi_data <- function(survey) {
  # Obtain EDI data package ID for most recent revision
  # Stops if its the same as the last discretewq update
  edi_id_curr <- get_latest_edi_id(survey)

  # Compile all data entities for EDI data package
  edi_data_ent_all <- get_edi_data_entities(edi_id_curr)

  # Import EDI data entities metadata table and filter to survey
  df_edi_ent <-
    read_csv("data-raw/01_Global/EDI_data_entity_metadata.csv", show_col_types = FALSE) %>%
    dplyr::filter(Survey == survey)

  # Subset to desired data entities
  df_edi_ent_sub <- df_edi_ent %>%
    mutate(
      Data_entity_edi_name = map(Data_entity_regex, \(x) grep(x, edi_data_ent_all, value = TRUE)),
      Data_entity_empty = map_lgl(Data_entity_edi_name, is_empty),
      .keep = "used"
    )

  # Check if regex patterns for desired data entities return expected results
  if (any(df_edi_ent_sub$Data_entity_empty)) {
    df_edi_ent_fail <- df_edi_ent_sub %>% dplyr::filter(Data_entity_empty)
    abort(c(
      "x" = paste(
        "The following data entity regex patterns did not find a data entity:",
        paste(df_edi_ent_fail$Data_entity_regex, collapse = ", ")
      ),
      "i" = "Update data entity regex patterns in EDI_data_entity_metadata.csv before proceeding"
    ))
  } else {
    inform(c(
      "i" = paste0(
        "Downloading data entities:\n",
        paste(df_edi_ent_sub$Data_entity_edi_name, collapse = "\n"), "\n"
      )
    ))
  }

  # Download data entities to temporary directory
  get_edi_data(edi_id_curr, df_edi_ent_sub$Data_entity_edi_name)
  temp_files <- list.files(tempdir(), full.names = TRUE)

  # Import data and perform checks and minor processing
  ndf_edi_ent <- df_edi_ent %>%
    mutate(
      # Determine file paths for data entities on temporary directory
      Data_entity_fp = map_chr(Data_entity_regex, \(x) grep(x, temp_files, value = TRUE)),
      # Add data entity name
      Data_entity_name = map_chr(Data_entity_regex, \(x) grep(x, edi_data_ent_all, value = TRUE)),
      # Import data
      df_data = map2(Data_entity_fp, Data_entity, \(x, y) import_raw_data(x, survey, y)),
      # Parse Datetime columns
      df_data = pmap(
        list(df_data, Date_format, Time_format, Time_zone, Data_entity_name),
        convert_datetime
      ),
      # Convert Depth from feet to meters if necessary
      df_data = pmap(list(df_data, Depth_unit, Data_entity_name), convert_depth),
      # Convert Secchi depth from meters to centimeters if necessary
      df_data = pmap(list(df_data, Secchi_unit, Data_entity_name), convert_secchi)
    )

  # Return a list of processed data entities and the EDI ID
  ndf_edi_ent$df_data %>%
    set_names(ndf_edi_ent$Data_entity) %>%
    append(list("edi_id" = edi_id_curr), after = 0)
}


# Data documentation ------------------------------------------------------

# Generate information for updating data dimensions for data documentation
# >>> used internally in documentation helpers
get_data_dims <- function(data_clean) {
  num_rows <- prettyNum(nrow(data_clean), big.mark = ',')
  inform(c(
    "i" = "Update data documentation in 'R/data.R':",
    "*" = glue::glue("@format a tibble with {num_rows} rows and {ncol(data_clean)} variables")
  ))
}

# Generate information from EDI that's needed to update documentation
# >>> used internally in document_helper_edi and update_edi_metadata
get_edi_doc_info <- function(edi_id) {
  # Citation for EDI data package
  edi_cit_raw <- read_data_package_citation(edi_id, access = FALSE)

  # DOI for EDI data package
  edi_doi <- read_data_package_doi(edi_id)

  # URL for EDI data package
  edi_url <- paste0("https://portal.edirepository.org/nis/metadataviewer?packageid=", edi_id)

  lst(edi_cit_raw, edi_doi, edi_url)
}

# Documentation helper for datasets derived from EDI data packages
document_helper_edi <- function(edi_id, data_clean) {
  # Generate EDI info
  ls_edi_info <- get_edi_doc_info(edi_id)

  # Citation for README:
  edi_cit_README <- paste0(
    stringr::str_remove(ls_edi_info$edi_cit_raw, '(?<=Environmental Data Initiative\\.\\s)https.+'),
    "[", ls_edi_info$edi_doi, "](", ls_edi_info$edi_url, ")"
  )
  inform(c(
    "i" = "Update citation in 'README.Rmd':",
    "*" = paste(edi_cit_README, "\n")
  ))

  # Data documentation:
  get_data_dims(data_clean)
  inform(c("*" = paste("URL:", ls_edi_info$edi_url, "\n")))

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  inform(c(
    "i" = "Update 'Data_source' in 'publication/data_objects/Delta_Integrated_WQ_metadata.csv':",
    "*" = paste(ls_edi_info$edi_url, "\n")
  ))

  # metadata_templates/methods.docx
  inform(c(
    "i" = "Update '5. Data Sources' in 'publication/metadata_templates/methods.docx':",
    "*" = paste(ls_edi_info$edi_cit_raw, "\n")
  ))

  # metadata_templates/provenance.txt
  inform(c(
    "i" = paste(
      "Update 'dataPackageID' in 'publication/metadata_templates/provenance.txt':", edi_id)
  ))
}

# Documentation helper for datasets derived from sources other than EDI data
  # packages
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

# Update EDI data package metadata table with latest revision
update_edi_metadata <- function(survey, edi_id) {
  # Generate EDI info
  ls_edi_info <- get_edi_doc_info(edi_id)

  # Update EDI data package metadata table
  fp_edi_meta <- "data-raw/01_Global/EDI_data_package_metadata.csv"
  df_edi_meta <- read_csv(fp_edi_meta, col_types = "cddcccc")

  df_edi_meta_surv <- df_edi_meta %>% dplyr::filter(Survey == survey)

  df_edi_meta_surv$Revision_last <- as.numeric(stringr::str_extract(edi_id, "(?<=\\.)\\d+$"))
  df_edi_meta_surv$Data_pack_id_full_last <- edi_id
  df_edi_meta_surv$Citation <- ls_edi_info$edi_cit_raw
  df_edi_meta_surv$DOI <- ls_edi_info$edi_doi
  df_edi_meta_surv$URL <- ls_edi_info$edi_url

  inform(c("i" = paste0("Updated record for ", survey, " in '", fp_edi_meta, "'")))

  df_edi_meta %>%
    anti_join(df_edi_meta_surv, by = "Survey") %>%
    bind_rows(df_edi_meta_surv) %>%
    arrange(Survey) %>%
    write_csv(fp_edi_meta)
}

