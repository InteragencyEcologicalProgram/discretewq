# Code to prepare `DOP` dataset
library(dplyr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "DOP"
tzone <- "America/Los_Angeles"

# Run standardized workflow to import data and process it
edi_metadata <- get_edi_data(survey)
ls_DOP <- import_proc_data(survey, df_files = edi_metadata$df_edi_files)

# Finish cleaning up data
# Notes:
  # 1) We're not including Microcystis because they use a different method
  # 2) Turbidity is measured with a YSI EXO2 sonde according to the DOP methods - units are FNU
DOP <- ls_DOP$df_data %>%
  # Remove Channel Deep and Oblique samples. Channel Deep measurements are taken at the bottom third
    # to half of the water column and therefore aren't comparable to bottom samples from other
    # surveys. The WQ measurements for the Oblique tows are either all NA or they are identical to
    # either the Channel Surface or Channel Deep samples collected at the same location.
  filter(!Habitat %in% c("Channel Deep", "Oblique")) %>%
  # Combine Station_Code and Habitat columns to make the Station column to preserve habitat info for
    # each station
  mutate(
    Station = paste(Station_Code, Habitat),
    Field_coords = TRUE
  ) %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = tzone) %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove replicate tows with identical WQ values - select earliest Datetime
  arrange(Datetime) %>%
  distinct(
    Station, Date, Secchi, Temperature, Salinity, Conductivity, DissolvedOxygen, pH,
    TurbidityFNU, Chlorophyll,
    .keep_all = TRUE
  ) %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Date, Station) %>%
  add_update_info(edi_metadata$edi_id)

usethis::use_data(DOP, overwrite = TRUE)
document_helper_edi(edi_metadata$edi_id, DOP)
