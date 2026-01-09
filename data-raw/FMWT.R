# Code to prepare `FMWT` dataset
library(dplyr)
library(lubridate)
library(stringr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "FMWT"
tzone <- "America/Los_Angeles"

# Run standardized workflow to import data and process it
# Workflow includes importing sample table from FMWT database hosted on FTP site to add in
  # Secchi_estimated info because this isn't currently available in the EDI data publication
edi_metadata <- get_edi_data(survey)
ls_FMWT <- import_proc_data(survey, df_files = edi_metadata$df_edi_files)

# Prepare sample table to be joined with data table
df_sample_c <- ls_FMWT$df_sample %>%
  mutate(Secchi_estimated = case_match(Secchi_estimated, 0 ~ FALSE, 1 ~ TRUE)) %>%
  # Remove duplicates
  distinct() %>%
  arrange(desc(Secchi_estimated)) %>%
  distinct(Station, Date, .keep_all = TRUE) %>%
  arrange(Date, Station) %>%
  # Adjust dates to one day later for a couple samples to match with data table correctly
  # Visually confirmed these have the same WQ measurements
  mutate(
    Date = if_else(
      Date == "2021-09-13" & Station %in% c("913", "915"),
      Date + days(1),
      Date
    )
  )

# Finish cleaning up data
FMWT <- ls_FMWT$df_data %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = tzone) %>%
  mutate(
    # Standardize stations to 3 digit codes
    Station = if_else(str_length(Station) == 2, paste0("0", Station), Station),
    # Reassign some Microcystis values
    Microcystis = if_else(Microcystis == 6, 2, Microcystis)
  ) %>%
  # Add Secchi_estimated
  left_join(df_sample_c, by = join_by(Station, Date)) %>%
  # Resolve field and standard coordinates
  resolve_lat_long() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Date, Station) %>%
  add_update_info(edi_metadata$edi_id)

usethis::use_data(FMWT, overwrite = TRUE)
document_helper_edi(edi_metadata$edi_id, FMWT)
