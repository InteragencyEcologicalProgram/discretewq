# Code to prepare `STN` dataset
library(dplyr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "STN"
tzone <- "America/Los_Angeles"

# Run standardized workflow to import data and process it
edi_metadata <- get_edi_data(survey)
ls_STN <- import_proc_data(survey, df_files = edi_metadata$df_edi_files)

# Start cleaning df_data
STN <- ls_STN$df_data %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = tzone) %>%
  # Remove rows where all measurements are NA
  rm_rows_all_miss_data() %>%
  # Keep only the first tow of each day (first defined by time of day then by tow number if all
    # times in the group are NA)
  arrange(Date, Station, Datetime, TowNum) %>%
  distinct(Station, Date, .keep_all = TRUE) %>%
  # Add station coordinates
  left_join(ls_STN$df_stations, by = join_by(Station), suffix = c("_field", "")) %>%
  resolve_lat_long() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  add_update_info(edi_metadata$edi_id)

usethis::use_data(STN, overwrite = TRUE)
document_helper_edi(edi_metadata$edi_id, STN)
