# Code to prepare `DOP` dataset
library(dplyr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "DOP"
edi_pack_id <- 1187

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Define data entities for download from EDI and their regex patterns
ent_regex <- c("df_data" = "TowData")

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data(edi_id_latest, ent_regex)

# Run standardized workflow to import data and process it
df_data <-
  import_raw_data(edi_fp) %>%
  standardize_col_meta(import_col_meta(survey, names(ent_regex)))

# Finish cleaning up data
# Notes:
# 1) We're not including Microcystis because they use a different method
# 2) Turbidity is measured with a YSI EXO2 sonde according to the DOP methods - units are FNU
DOP <- df_data %>%
  convert_date("Ymd") %>%
  convert_time("HMS") %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = "America/Los_Angeles") %>%
  convert_depth("feet") %>%
  convert_secchi("cm") %>%
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
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove replicate tows with identical WQ values - select earliest Datetime
  arrange(Datetime) %>%
  distinct(
    Station,
    Date,
    Secchi,
    Temperature,
    Salinity,
    Conductivity,
    DissolvedOxygen,
    pH,
    TurbidityFNU,
    Chlorophyll,
    .keep_all = TRUE
  ) %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Date, Station) %>%
  add_update_info(edi_id_latest)

usethis::use_data(DOP, overwrite = TRUE)
document_helper_edi(edi_id_latest, DOP)
