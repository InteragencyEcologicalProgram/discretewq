# Global helper functions to help with data import and structure checking

library(EDIutils)
library(glue)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(stringr)


# Get ID for most current revision of EDI publication and check if it differs
  # from the revision used in the last discretewq update
get_latest_edi_id <- function(edi_last_update_id) {
  edi_scope <- "edi"
  edi_package_id <- floor(edi_last_update_id)
  latest_rev <- list_data_package_revisions(
    scope = edi_scope,
    identifier = edi_package_id,
    filter = "newest"
  )

  latest_id <- paste(edi_package_id, latest_rev, sep = ".")
  message(
    "The latest revision for data package ",
    floor(edi_package_id), " is ", latest_id
  )

  if (latest_id == edi_last_update_id) {
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

  return(paste(edi_scope, latest_id, sep = "."))
}

# Get data entity names for specified EDI ID
get_edi_data_entities <- function(edi_rev_id) {
  df_data_ent <- read_data_entity_names(edi_rev_id)
  message(
    "Data entities for ", edi_rev_id,
    " include:\n", paste(df_data_ent$entityName, collapse = "\n"),
    "\nSubset vector to the data entities you want to use"
  )
  return(df_data_ent$entityName)
}

# Download specified data entities from an EDI package and save raw bytes files
  # to a temporary directory
get_edi_data <- function(edi_rev_id, entity_names) {
  df_data_ent <- read_data_entity_names(edi_rev_id)
  df_data_ent_filt <- df_data_ent %>% filter(entityName %in% entity_names)

  ls_data_raw <- map(
    df_data_ent_filt$entityId,
    \(x) read_data_entity(edi_rev_id, entityId = x)
  ) %>%
  set_names(df_data_ent_filt$entityName)

  temp_dir <- tempdir()
  for (i in 1:length(ls_data_raw)) {
    file_raw <- file.path(temp_dir, glue("{names(ls_data_raw)[i]}.bin"))
    con <- file(file_raw, "wb")
    writeBin(ls_data_raw[[i]], con)
    close(con)
  }
}

# Import raw data while running checks for column names and types, then apply
  # standardized formatting
import_raw_data <- function(file, survey, entity) {
  # Import data with all columns as character
  df_data_raw <- read_csv(file, col_types = list(.default = "c"))

  # Import data column metadata table and filter to survey and entity
  df_col_meta <-
    read_csv("data-raw/01_Global/Data_column_metadata.csv", show_col_types = FALSE) %>%
    filter(Survey == survey, Data_entity == entity)

  # Check if expected columns in df_check exist in df_data_raw
  df_col_check <- df_col_meta %>%
    mutate(col_name_check = map_lgl(Col_name_exp, \(x) any(names(df_data_raw) == x)))

  # Generate message for results of check
  message("Attempting to import: ", str_extract(file, "(?<=/).+\\.bin$"))
  if (all(df_col_check$col_name_check)) {
    message("All column names are correct. Proceeding with import.")
  } else {
    col_name_F <- df_col_check %>% filter(!col_name_check) %>% pull(Col_name_exp)
    for (i in col_name_F) {
      message("Column '", i, "' NOT present in data frame")
    }
    stop("Update expected column names in Data_column_metadata.csv before proceeding", call. = FALSE)
  }

  # Select columns specified in data column metadata table and convert specified
    # columns to numeric
  col_numeric <- df_col_meta %>% filter(Col_type == "numeric") %>% pull(Col_name_exp)
  df_data_c <- df_data_raw %>%
    select(all_of(df_col_meta$Col_name_exp)) %>%
    mutate(across(all_of(col_numeric), as.numeric))

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
    message("All numeric columns parsed correctly. Data import complete.")
  } else {
    parse_F <- df_parse_check %>% filter(!parse_check) %>% pull(col_name)
    for (i in parse_F) {
      message("Column '", i, "' did NOT parse as numeric correctly")
    }
    print(df_parse_check %>% filter(!parse_check))
    stop("Data NOT imported. Fix problem underlying parsing error before proceeding", call. = FALSE)
  }

  # Rename columns according to new names in data column metadata table
  names(df_data_c) <- df_col_meta$Col_name_new
  return(df_data_c)
}

# Generate information from EDI that's needed to update documentation
get_edi_update_info <- function(edi_rev_id, data_clean) {
  # Citation for EDI data package
  edi_cit_raw <- read_data_package_citation(edi_rev_id, access = FALSE)

  # DOI for EDI data package
  edi_doi <- read_data_package_doi(edi_rev_id)

  # URL for EDI data package
  edi_url <- paste0("https://portal.edirepository.org/nis/metadataviewer?packageid=", edi_rev_id)

  # Citation for README:
  edi_cit_README <- paste0(
    str_remove(edi_cit_raw, '(?<=Environmental Data Initiative\\.\\s)https.+'),
    "[", edi_doi, "](", edi_url, ")"
  )
  message("Update citation in 'README.Rmd':\n", edi_cit_README, "\n")

  # Data documentation:
  message(
    "Update data documentation in 'R/data.R':\n",
    "@format ", glue("a tibble with {nrow(data_clean)} rows and {ncol(data_clean)} variables"),
    "\nURL: ", edi_url, "\n"
  )

  # Publication Files:
  # data_objects/Delta_Integrated_WQ_metadata.csv
  message(
    "Update 'Data_source' in 'publication/data_objects/Delta_Integrated_WQ_metadata.csv':\n",
    edi_url, "\n"
  )

  # metadata_templates/methods.docx
  message(
    "Update '5. Data Sources' in 'publication/metadata_templates/methods.docx':\n",
    edi_cit_raw, "\n"
  )

  # metadata_templates/provenance.txt
  message(
    "Update 'dataPackageID' in 'publication/metadata_templates/provenance.txt':\n",
    edi_rev_id
  )
}

