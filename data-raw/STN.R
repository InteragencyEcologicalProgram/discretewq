# Code to prepare `STN` dataset
library(dplyr)
library(purrr)

# Source helper functions
source("R/data_raw_helpers.R")

# Define settings for dataset
survey <- "STN"
edi_pack_id <- 1413

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Define data entities for download from EDI and their regex patterns
ent_regex <- c("df_data" = "CatchPerTow")

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data(edi_id_latest, ent_regex)

# Run standardized workflow to import data and process it
ls_STN <-
  c(edi_fp, "df_stations" = "data-raw/STN/luStation.csv") %>%
  map(import_raw_data) %>%
  map2(
    map(names(.), \(x) import_col_meta(survey, x)),
    standardize_col_meta
  )

# Prepare stations table to be joined with data table
df_stations <- convert_lat_long(ls_STN$df_stations, coord_comp = "DMS")

# Finish cleaning up data
STN <- ls_STN$df_data %>%
  convert_date("mdY") %>%
  convert_time("HM") %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = "America/Los_Angeles") %>%
  convert_depth("feet") %>%
  convert_secchi("cm") %>%
  standardize_tide_code() %>%
  convert_lat_long(coord_comp = "DMS") %>%
  # Remove rows where all measurements are NA
  rm_rows_all_miss_data() %>%
  # Keep only the first tow of each day (first defined by time of day then by tow number if all
  # times in the group are NA)
  arrange(Date, Station, Datetime, TowNum) %>%
  distinct(Station, Date, .keep_all = TRUE) %>%
  # Add station coordinates
  left_join(df_stations, by = join_by(Station), suffix = c("_field", "")) %>%
  resolve_lat_long() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  add_update_info(edi_id_latest)

usethis::use_data(STN, overwrite = TRUE)
document_helper_edi(edi_id_latest, STN)
