# Code to prepare `DJFMP` dataset
library(dplyr)
library(purrr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "DJFMP"
tzone_1976_2001 <- "America/Los_Angeles"
tzone_2002_curr <- "America/Los_Angeles"

# Run standardized workflow to import data and process it
edi_metadata <- get_edi_data(survey)
ls_DJFMP <- import_proc_data(survey, df_files = edi_metadata$df_edi_files)

# Combine and finish cleaning data
DJFMP <- list(ls_DJFMP$df_data_1976_2001, ls_DJFMP$df_data_2002_curr) %>%
  # Create Datetime column from Date and Time columns
  map2(c(tzone_1976_2001, tzone_2002_curr), \(x, y) combine_datetime(x, timezone = y)) %>%
  list_rbind() %>%
  # Remove conductivity data from dates before it was standardized >
  # Methods in metadata say they do not know if their data were corrected for temperature before
    # May 3 or 17 2019 so we will not use conductivity data before June 2019
  mutate(Conductivity = if_else(Date < "2019-06-01", NA_real_, Conductivity)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove duplicated rows
  distinct(Station, Datetime, .keep_all = TRUE) %>%
  # Add station coordinates
  left_join(ls_DJFMP$df_stations, by = join_by(Station)) %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Datetime) %>%
  add_update_info(edi_metadata$edi_id)

usethis::use_data(DJFMP, overwrite = TRUE)
document_helper_edi(edi_metadata$edi_id, DJFMP)
