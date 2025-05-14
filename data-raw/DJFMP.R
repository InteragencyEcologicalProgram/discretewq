# Code to prepare `DJFMP` dataset
library(dplyr)

source("data-raw/01_Global/data_raw_helpers.R")

survey <- "DJFMP"

# Run standardized workflow to import data from EDI and process it
ls_DJFMP <- import_proc_edi_data(survey)

# Combine and finish cleaning data
DJFMP <-
  bind_rows(ls_DJFMP$df_data_1976_2001, ls_DJFMP$df_data_2002_curr) %>%
  # Remove conductivity data from dates before it was standardized >
  # Methods in  metadata say they do not know if their data were corrected for temperature before
  # May 3 or 17 2019 so we will not use conductivity data before June 2019
  mutate(Conductivity = if_else(Date < "2019-06-01", NA_real_, Conductivity)) %>%
  # Remove rows where all measurements are NA
  rm_rows_all_miss_data() %>%
  # Remove duplicated rows
  distinct(Station, Datetime, .keep_all = TRUE) %>%
  left_join(ls_DJFMP$df_stations, by = join_by(Station)) %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Datetime)

usethis::use_data(DJFMP, overwrite = TRUE)

document_helper_edi(ls_DJFMP$edi_id, DJFMP)
update_edi_metadata(survey, ls_DJFMP$edi_id)
