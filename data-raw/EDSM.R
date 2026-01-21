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
edi_pack_id <- 415

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Define data entities for download from EDI and their regex patterns
ent_regex <- c(
  "df_data_20mm" = "EDSM_20mm",
  "df_data_KDTR" = "EDSM_KDTR"
)

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data(edi_id_latest, ent_regex)

# Run standardized workflow to import data and process it
ls_EDSM <- edi_fp %>%
  map(import_raw_data) %>%
  map2(
    map(names(.), \(x) import_col_meta(survey, x)),
    standardize_col_meta
  )

# Prepare tables before joining them together
df_data_20mm <- ls_EDSM$df_data_20mm %>%
  convert_date("Ymd") %>%
  convert_time("HMS") %>%
  combine_datetime(timezone = "America/Los_Angeles") %>%
  convert_depth("feet") %>%
  convert_secchi("meters")

df_data_KDTR <- ls_EDSM$df_data_KDTR %>%
  convert_date("Ymd") %>%
  convert_time("HM") %>%
  combine_datetime(timezone = "America/Los_Angeles") %>%
  convert_depth("meters") %>%
  convert_secchi("meters")

# Combine and finish cleaning data
EDSM <- bind_rows(df_data_20mm, df_data_KDTR) %>%
  mutate(
    # Standardize tide codes
    Tide = case_match(
      Tide,
      "HS" ~ "High Slack",
      "LS" ~ "Low Slack",
      .default = Tide
    ),
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
  add_update_info(edi_id_latest)

usethis::use_data(EDSM, overwrite = TRUE)
document_helper_edi(edi_id_latest, EDSM)
