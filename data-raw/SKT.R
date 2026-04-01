# Code to prepare `SKT` dataset
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)

# Source helper functions
source("R/data_raw_helpers.R")

# Define settings for dataset
survey <- "SKT"

# Obtain current dataset information
current_info <- get_update_info(survey)

# Define data entities for import and their regex patterns
ent_regex <- c(
  "df_stations" = "StationsSKT",
  "df_data" = "tblSample"
)

# List files in data-raw/SKT folder
skt_data_files <- list.files("data-raw/SKT", full.names = TRUE)

# Run standardized workflow to import data and process it
ls_SKT <-
  map(ent_regex, \(x) subset_data_entity(skt_data_files, x)) %>%
  map(import_raw_data) %>%
  map2(
    map(names(.), \(x) import_col_meta(survey, x)),
    standardize_col_meta
  )

# Prepare stations table to join to data table
df_stations <- convert_lat_long(ls_SKT$df_stations, coord_comp = "DMS")

# Join data entities and finish cleaning data
SKT <- ls_SKT$df_data %>%
  convert_date("mdY HMS") %>%
  convert_time("mdY HMS") %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = "America/Los_Angeles") %>%
  # Convert one time stamp from midnight to noon
  mutate(
    Datetime = if_else(
      Date == "2018-03-06" & Station == "519",
      ymd_hms("2018-03-06 12:00:00", tz = "America/Los_Angeles"),
      Datetime
    )
  ) %>%
  convert_depth("feet") %>%
  convert_secchi("cm") %>%
  standardize_tide_code() %>%
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
  # Add station coordinates
  left_join(df_stations, by = join_by(Station), suffix = c("_field", "")) %>%
  resolve_lat_long() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove replicate tows with identical WQ values - select earliest Datetime
  arrange(Datetime) %>%
  distinct(
    Date,
    Station,
    Secchi,
    Conductivity,
    Temperature,
    TurbidityNTU,
    TurbidityFNU,
    .keep_all = TRUE
  ) %>%
  # Standardize column order
  standardize_col_order() %>%
  add_update_info()

usethis::use_data(SKT, overwrite = TRUE)
document_helper_other(SKT)
