# Global helper functions to help with data import and structure checking

library(dplyr)
library(readr)
library(tidyr)
library(purrr)

# Install EDIutils if its not installed already
if (!requireNamespace("EDIutils", quietly = TRUE)) {
  install.packages("EDIutils")
}
library(EDIutils)

# Install jsonlite if its not installed already
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}


# Internal functions ------------------------------------------------------

# Generate information for updating data dimensions for data documentation
int_get_data_dims <- function(data_clean) {
  num_rows <- prettyNum(nrow(data_clean), big.mark = ',')
  message(
    "Update data documentation in 'R/data.R':\n",
    "@format ",
    glue::glue("a tibble with {num_rows} rows and {ncol(data_clean)} variables")
  )
}

# Generate information from EDI that's needed to update documentation
int_get_edi_doc_info <- function(edi_rev_id) {
  # Citation for EDI data package
  edi_cit_raw <- read_data_package_citation(edi_rev_id, access = FALSE)

  # DOI for EDI data package
  edi_doi <- read_data_package_doi(edi_rev_id)

  # URL for EDI data package
  edi_url <- paste0("https://portal.edirepository.org/nis/metadataviewer?packageid=", edi_rev_id)

  tibble::lst(edi_cit_raw, edi_doi, edi_url)
}


# Data download -----------------------------------------------------------

# Get ID for most current revision of EDI publication and check if it differs
  # from the revision used in the last discretewq update
get_latest_edi_id <- function(survey) {
  # Import EDI data package metadata table and filter to survey
  df_edi_meta <-
    read_csv("data-raw/01_Global/EDI_data_package_metadata.csv", show_col_types = FALSE) %>%
    dplyr::filter(Survey == survey)

  edi_scope <- "edi"
  edi_package_id <- df_edi_meta$Data_pack_id
  latest_rev <- list_data_package_revisions(
    scope = edi_scope,
    identifier = df_edi_meta$Data_pack_id,
    filter = "newest"
  )

  latest_id <- paste(edi_scope, edi_package_id, latest_rev, sep = ".")
  message(
    "The latest revision for data package ",
    edi_package_id, " is ", latest_id
  )

  if (latest_rev == df_edi_meta$Revision_last) {
    message(
      "The EDI data package hasn't been updated since the last discretewq update\n",
      "There is no need to update this data set"
    )
  } else {
    message(
      "The EDI data package has been updated since the last discretewq update\n",
      "Proceed with updating this data set"
    )
  }

  return(latest_id)
}

# Get data entity names for specified EDI ID
get_edi_data_entities <- function(edi_rev_id) {
  df_data_ent <- read_data_entity_names(edi_rev_id)
  message(
    "Data entities for ", edi_rev_id, " include:\n",
    paste(df_data_ent$entityName, collapse = "\n"),
    "\nSubset vector to the data entities you want to use"
  )
  return(df_data_ent$entityName)
}

# Download specified data entities from an EDI package and save raw bytes files
  # to a temporary directory
get_edi_data <- function(edi_rev_id, entity_names) {
  df_data_ent <- read_data_entity_names(edi_rev_id)
  df_data_ent_filt <- df_data_ent %>% dplyr::filter(entityName %in% entity_names)

  ls_data_raw <- map(
    df_data_ent_filt$entityId,
    \(x) read_data_entity(edi_rev_id, entityId = x)
  ) %>%
  set_names(df_data_ent_filt$entityName)

  temp_dir <- tempdir()
  for (i in 1:length(ls_data_raw)) {
    file_raw <- file.path(temp_dir, glue::glue("{names(ls_data_raw)[i]}.bin"))
    con <- file(file_raw, "wb")
    writeBin(ls_data_raw[[i]], con)
    close(con)
  }
}

# Download discrete lab data from CNRA data portal and save csv file to a
  # temporary directory
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
    tibble::tibble() %>%
    unnest_wider(1) %>%
    # remove the expensive full_text column
    select(!tidyselect::any_of("_full_text"))

  # Save data to temporary directory
  df_data_lab %>% write_excel_csv(
    file = file.path(tempdir(), paste0(station_num, "_lab_data.csv"))
  )
}

# Download discrete field measurement data from CNRA data portal and save csv
  # file to a temporary directory
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
    tibble::tibble() %>%
    unnest_wider(1) %>%
    # remove the expensive full_text column
    select(!tidyselect::any_of("_full_text"))

  # Save data to temporary directory
  df_data_field %>% write_excel_csv(
    file = file.path(tempdir(), paste0(station_num, "_field_data.csv"))
  )
}


# Import and clean data ---------------------------------------------------

# Import raw data while running checks for column names and types, then apply
  # standardized formatting
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
  message(
    "Attempting to import: ",
    stringr::str_extract(file, "(?<=/).+\\.(bin|csv)$")
  )

  if (all(df_col_check$col_name_check)) {
    message("All column names are correct. Proceeding with import.")
  } else {
    df_col_check_F <- df_col_check %>% dplyr::filter(!col_name_check)
    stop(
      "The following expected columns are NOT present in the data frame:\n",
      paste(df_col_check_F$Col_name_exp, collapse = "\n"),
      "\nUpdate expected column names in Data_column_metadata.csv before proceeding",
      call. = FALSE
    )
  }

  # Select columns specified in data column metadata table
  df_data_c <- df_data_raw %>% select(all_of(df_col_meta$Col_name_exp))

  # Convert specified columns to numeric if there are any
  if (any(df_col_meta$Col_type == "numeric")) {
    col_numeric <- df_col_meta %>%
      dplyr::filter(Col_type == "numeric") %>%
      pull(Col_name_exp)

    df_data_c <- df_data_c %>% mutate(across(tidyselect::all_of(col_numeric), as.numeric))

    # Check for parsing errors when converting columns to numeric
    df_parse_check <-
      map(
        list(df_data_raw, df_data_c),
        \(df) summarize(df, across(tidyselect::all_of(col_numeric), \(x) sum(is.na(x)))) %>%
          pivot_longer(tidyselect::everything(), names_to = "col_name", values_to = "num_NA")
      ) %>%
      reduce(\(x, y) left_join(x, y, by = join_by(col_name), suffix = c("_raw", "_clean"))) %>%
      mutate(parse_check = num_NA_raw == num_NA_clean)

    # Generate message for results of parsing check
    if (all(df_parse_check$parse_check)) {
      message("All numeric columns parsed correctly.")
    } else {
      df_parse_check_F <- df_parse_check %>% dplyr::filter(!parse_check)
      message(
        "The following columns did NOT parse as numeric correctly:\n",
        paste(df_parse_check_F$col_name, collapse = "\n")
      )
      print(df_parse_check_F, n = 100)
      stop(
        "Data NOT imported. Fix problem underlying parsing error before proceeding",
        call. = FALSE
      )
    }
  }

  # Rename columns according to new names in data column metadata table
  names(df_data_c) <- df_col_meta$Col_name_new
  message("Data import complete.")
  return(df_data_c)
}

# Standardize analyte names for data structured in long format
standardize_analytes <- function(df, survey, type = c("Field", "Lab")) {
  type <- rlang::arg_match(type)

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

    message(
      "The following expected analytes are NOT present in the dataset:\n",
      paste(df_analytes_miss$Analyte_exp, collapse = "\n")
    )
    print(df_analytes_check, n = 100)
    stop(
      "Update expected analyte names and units in Analytes_metadata.csv before proceeding",
      call. = FALSE
    )
  } else {
    message("All analyte names are correct. Proceeding with standardizing names.")
  }

  # Generate message for results of check - removal of unwanted analytes
  if (any(is.na(df_analytes_check$Analyte_exp))) {
    df_analytes_rm <- df_analytes_check %>%
      dplyr::filter(is.na(Analyte_exp)) %>%
      mutate(Analyte = if_else(is.na(Units), Analyte, paste0(Analyte, " (", Units, ")")))

    message(
      "The following analytes were removed from the dataset:\n",
      paste(df_analytes_rm$Analyte, collapse = "\n")
    )
  } else {
    message("No analytes removed from dataset.")
  }

  # Proceed with standardizing analyte names in df
  df %>%
    left_join(df_analytes, by = analytes_join) %>%
    drop_na(Analyte_std) %>%
    select(-c(Analyte, Units))
}


# Data documentation ------------------------------------------------------

# Documentation helper for datasets derived from EDI data packages
document_helper_edi <- function(edi_rev_id, data_clean) {
  # Generate EDI info
  ls_edi_info <- int_get_edi_doc_info(edi_rev_id)

  # Citation for README:
  edi_cit_README <- paste0(
    stringr::str_remove(ls_edi_info$edi_cit_raw, '(?<=Environmental Data Initiative\\.\\s)https.+'),
    "[", ls_edi_info$edi_doi, "](", ls_edi_info$edi_url, ")"
  )
  message("Update citation in 'README.Rmd':\n", edi_cit_README, "\n")

  # Data documentation:
  message(
    int_get_data_dims(data_clean),
    "URL: ", ls_edi_info$edi_url, "\n"
  )

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  message(
    "Update 'Data_source' in 'publication/data_objects/Delta_Integrated_WQ_metadata.csv':\n",
    ls_edi_info$edi_url, "\n"
  )

  # metadata_templates/methods.docx
  message(
    "Update '5. Data Sources' in 'publication/metadata_templates/methods.docx':\n",
    ls_edi_info$edi_cit_raw, "\n"
  )

  # metadata_templates/provenance.txt
  message(
    "Update 'dataPackageID' in 'publication/metadata_templates/provenance.txt': ",
    edi_rev_id
  )
}

# Documentation helper for datasets derived from sources other than EDI data
  # packages
document_helper_other <- function(data_clean) {
  dataset_yr <- max(lubridate::year(data_clean$Date))

  # Citation for README:
  message(
    "Update citation in 'README.Rmd':\n",
    "Year of dataset is ", dataset_yr, "\n"
  )

  # Data documentation:
  message(
    int_get_data_dims(data_clean),
    "Check URL for metadata and information on methods in @details section, ",
    "and update if necessary\n"
  )

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  message(
    "Check 'Data_source' in 'publication/data_objects/Delta_Integrated_WQ_metadata.csv', ",
    "and update if necessary\n"
  )

  # metadata_templates/methods.docx
  message(
    "Update '5. Data Sources' in 'publication/metadata_templates/methods.docx':\n",
    "Year of dataset is ", dataset_yr, "\n"
  )

  # metadata_templates/provenance.txt
  message(
    "Check information in 'publication/metadata_templates/provenance.txt', ",
    "and update if necessary"
  )
}

# Update EDI data package metadata table with latest revision
update_edi_metadata <- function(survey, edi_rev_id) {
  # Generate EDI info
  ls_edi_info <- int_get_edi_doc_info(edi_rev_id)

  # Update EDI data package metadata table
  fp_edi_meta <- "data-raw/01_Global/EDI_data_package_metadata.csv"
  df_edi_meta <- read_csv(fp_edi_meta, col_types = "cddcccc")

  df_edi_meta_surv <- df_edi_meta %>% dplyr::filter(Survey == survey)

  df_edi_meta_surv$Revision_last <- as.numeric(stringr::str_extract(edi_rev_id, "(?<=\\.)\\d+$"))
  df_edi_meta_surv$Data_pack_id_full_last <- edi_rev_id
  df_edi_meta_surv$Citation <- ls_edi_info$edi_cit_raw
  df_edi_meta_surv$DOI <- ls_edi_info$edi_doi
  df_edi_meta_surv$URL <- ls_edi_info$edi_url

  message("Updated record for ", survey, " in '", fp_edi_meta, "'")

  df_edi_meta %>%
    anti_join(df_edi_meta_surv, by = "Survey") %>%
    bind_rows(df_edi_meta_surv) %>%
    arrange(Survey) %>%
    write_excel_csv(fp_edi_meta)
}

