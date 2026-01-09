# Code to prepare `SKT` dataset
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "SKT"
tzone <- "America/Los_Angeles"

# Run standardized workflow to import data and process it
ls_SKT <- import_proc_data(survey)

# Join data entities and finish cleaning data
SKT <- ls_SKT$df_data %>%
  # Remove Temperature and Conductivity records acquired from CDEC
  mutate(
    across(
      c(Temperature, Conductivity),
      \(x) if_else(str_detect(Notes, "from CDEC") & !is.na(Notes), NA_real_, x)
    )
  ) %>%
  # Clean up field lat-long coordinates
  mutate(
    across(c(Latitude, Longitude), \(x) na_if(x, "0")),
    Longitude = if_else(Longitude == "121-29.41.7", "121-29-41.7", Longitude),
    across(c(Latitude, Longitude), \(x) str_remove(x, '".*')),
    across(c(Latitude, Longitude), \(x) str_remove(x, "'.*")),
    across(c(Latitude, Longitude), \(x) str_remove(x, "[:alpha:]"))
  ) %>%
  separate_lat_long(delim_chr = "-", coord_comp = "DMS") %>%
  convert_lat_long(coord_comp = "DMS") %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = tzone) %>%
  # Convert one time stamp from midnight to noon
  mutate(
    Datetime = if_else(
      Date == "2018-03-06" & Station == "519",
      ymd_hms("2018-03-06 12:00:00", tz = "America/Los_Angeles"),
      Datetime
    )
  ) %>%
  # Add station coordinates
  left_join(ls_SKT$df_stations, by = join_by(Station), suffix = c("_field", "")) %>%
  resolve_lat_long() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove replicate tows with identical WQ values - select earliest Datetime
  arrange(Datetime) %>%
  distinct(
    Date, Station, Secchi, Conductivity, Temperature, TurbidityNTU, TurbidityFNU,
    .keep_all = TRUE
  ) %>%
  # Standardize column order
  standardize_col_order() %>%
  add_update_info()

usethis::use_data(SKT, overwrite = TRUE)
document_helper_other(SKT)
