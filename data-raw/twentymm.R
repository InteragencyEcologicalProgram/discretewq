# Code to prepare `twentymm` dataset
library(dplyr)
library(lubridate)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "20mm"
tzone <- "America/Los_Angeles"

# Run standardized workflow to import data and process it
edi_metadata <- get_edi_data(survey)
ls_20mm <- import_proc_data(survey, df_files = edi_metadata$df_edi_files)

# Join data entities and finish cleaning data
twentymm <- ls_20mm$df_data %>%
  left_join(ls_20mm$df_survey, by = join_by(SurveyID)) %>%
  left_join(ls_20mm$df_tow, by = join_by(StationID)) %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = tzone) %>%
  # Correct a few erroneous times (most likely not recorded in military time format)
  mutate(Datetime = if_else(hour(Datetime) %in% 1:2, Datetime + hours(12), Datetime)) %>%
  # Only keep info for the first tow of each day (first defined by time then by tow number for ties)
  group_by(StationID) %>% # StationID really is SampleID
  filter(Datetime == min(Datetime, na.rm = TRUE)) %>%
  filter(TowNum == min(TowNum)) %>%
  ungroup() %>%
  # Add station coordinates
  left_join(ls_20mm$df_stations, by = join_by(Station), suffix = c("_field", "")) %>%
  resolve_lat_long() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Datetime) %>%
  add_update_info(edi_metadata$edi_id)

usethis::use_data(twentymm, overwrite = TRUE)
document_helper_edi(edi_metadata$edi_id, twentymm)
