# Code to prepare `SLS` dataset
library(dplyr)
library(tidyr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "SLS"
tzone <- "America/Los_Angeles"

# Run standardized workflow to import data and process it
edi_metadata <- get_edi_data(survey)
ls_SLS <- import_proc_data(survey, df_files = edi_metadata$df_edi_files)

# Prepare station table before joining it to data tables
df_stations <- ls_SLS$df_stations %>%
  separate_lat_long(delim_chr = " ", coord_comp = "DMS") %>%
  convert_lat_long(coord_comp = "DMS")

# Join data entities and finish cleaning data
SLS <- ls_SLS$df_water_info %>%
  left_join(ls_SLS$df_tow, by = join_by(Date, Station)) %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = tzone) %>%
  # Add station coordinates
  left_join(df_stations, by = join_by(Station)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  add_update_info(edi_metadata$edi_id)

usethis::use_data(SLS, overwrite = TRUE)
document_helper_edi(edi_metadata$edi_id, SLS)
