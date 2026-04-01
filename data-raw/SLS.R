# Code to prepare `SLS` dataset
library(dplyr)
library(tidyr)
library(purrr)

# Source helper functions
source("R/data_raw_helpers.R")

# Define settings for dataset
survey <- "SLS"
edi_pack_id <- 534

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Define data entities for download from EDI and their regex patterns
ent_regex <- c(
  "df_stations" = "^Station",
  "df_tow" = "^TowInfo",
  "df_water_info" = "^WaterInfo"
)

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data(edi_id_latest, ent_regex)

# Run standardized workflow to import data and process it
ls_SLS <- edi_fp %>%
  map(import_raw_data) %>%
  map2(
    map(names(.), \(x) import_col_meta(survey, x)),
    standardize_col_meta
  )

# Prepare tables before joining them together
df_stations <- ls_SLS$df_stations %>%
  separate_lat_long(delim_chr = " ", coord_comp = "DMS") %>%
  convert_lat_long(coord_comp = "DMS")

df_tow <- ls_SLS$df_tow %>%
  convert_date("Ymd") %>%
  convert_time("HM") %>%
  convert_depth("feet") %>%
  standardize_tide_code()

df_water_info <- ls_SLS$df_water_info %>%
  convert_date("Ymd") %>%
  convert_secchi("cm")

# Join data entities and finish cleaning data
SLS <- df_water_info %>%
  left_join(df_tow, by = join_by(Date, Station)) %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = "America/Los_Angeles") %>%
  # Add station coordinates
  left_join(df_stations, by = join_by(Station)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  add_update_info(edi_id_latest)

usethis::use_data(SLS, overwrite = TRUE)
document_helper_edi(edi_id_latest, SLS)
