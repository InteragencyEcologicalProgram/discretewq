# Code to prepare `EDSM` dataset
# Note about the depth units for the 20mm gear:
# Metadata in EDI publication states the depth units for 20mm are now meters, but the data matches
  # prior revisions that were reported in feet. Contacted EDSM data managers about this on
  # 10/7/2025 - confirmed that the units should be feet and will be fixed in next EDI revision

library(dplyr)
library(purrr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "EDSM"
tzone_20mm <- "America/Los_Angeles"
tzone_KDTR <- "America/Los_Angeles"

# Run standardized workflow to import data and process it
edi_metadata <- get_edi_data(survey)
ls_EDSM <- import_proc_data(survey, df_files = edi_metadata$df_edi_files)

# Combine and finish cleaning data
EDSM <- ls_EDSM %>%
  # Create Datetime column from Date and Time columns
  map2(c(tzone_20mm, tzone_KDTR), \(x, y) combine_datetime(x, timezone = y)) %>%
  list_rbind() %>%
  mutate(
    # Standardize tide codes
    Tide = case_match(Tide, "HS" ~ "High Slack", "LS" ~ "Low Slack", .default = Tide),
    Station = paste(Station, Date),
    Field_coords = TRUE,
    # Remove conductivity data from dates before it was standardized >
    # Methods in metadata say they do not know if their data were corrected for temperature before
      # May 3 or 17 2019 so we will not use conductivity data before June 2019
    Conductivity = if_else(Date < "2019-06-01", NA_real_, Conductivity)
  ) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove replicate samples with the same Datetime and location
  distinct(Station, Date, Datetime, Latitude, Longitude, .keep_all = TRUE) %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Datetime) %>%
  add_update_info(edi_metadata$edi_id)

usethis::use_data(EDSM, overwrite = TRUE)
document_helper_edi(edi_metadata$edi_id, EDSM)
