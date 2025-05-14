# Code to prepare `SKT` dataset
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

survey <- "SKT"

df_stations <- import_raw_data("data-raw/SKT/StationsSKT.csv", survey, "df_stations")
df_data <- import_raw_data("data-raw/SKT/tblSample.csv", survey, "df_data")

# Prepare station table before joining it to data table
SKT_stations <- df_stations %>%
  mutate(
    Latitude = LatDeg + LatMin / 60 + LatSec / 3600,
    Longitude = (LongDec + LongMin / 60 + LongSec / 3600) * -1,
    .keep = "unused"
  ) %>%
  drop_na()

# Finish cleaning data
SKT <- df_data %>%
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
  separate_wider_delim(
    Latitude, delim = "-", names = c("LatD", "LatM", "LatS"), cols_remove = TRUE
  ) %>%
  separate_wider_delim(
    Longitude, delim = "-", names = c("LonD", "LonM", "LonS"), cols_remove = TRUE
  ) %>%
  mutate(across(c("LatD", "LatM", "LatS", "LonD", "LonM", "LonS"), as.numeric)) %>%
  mutate(
    Latitude_field = LatD + LatM / 60 + LatS / 3600,
    Longitude_field = (LonD + LonM / 60 + LonS / 3600) * -1,
    .keep = "unused"
  ) %>%
  # Parse Date and Time columns
  mutate(
    Date = str_extract(Date, ".+(?=\\s)"),
    Time = str_extract(Time, "(?<=12/30/1899\\s).+")
  ) %>%
  convert_datetime(date_format = "mdY", time_format = "HMS", timezone = "America/Los_Angeles") %>%
  # Convert one time stamp from midnight to noon
  mutate(
    Datetime = if_else(
      Date == "2018-03-06" & Station == "519",
      ymd_hms("2018-03-06 12:00:00", tz = "America/Los_Angeles"),
      Datetime
    )
  ) %>%
  mutate(
    Tide = recode(
      as.character(Tide),
      `4` = "Flood", `3` = "Low Slack", `2` = "Ebb", `1` = "High Slack", `0` = NA_character_
    )
  ) %>%
  # Convert Depth from feet to meters
  convert_depth(depth_unit = "feet") %>%
  left_join(SKT_stations, by = join_by(Station)) %>%
  mutate(
    Field_coords = case_when(
      is.na(Latitude) & !is.na(Latitude_field) ~ TRUE,
      is.na(Longitude) & !is.na(Longitude_field) ~ TRUE,
      TRUE ~ FALSE
    ),
    Latitude = if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude = if_else(is.na(Longitude), Longitude_field, Longitude),
    .keep = "unused"
  ) %>%
  # Remove rows where all measurements are NA
  rm_rows_all_miss_data() %>%
  # Remove replicate tows with identical WQ values - select earliest Datetime
  arrange(Datetime) %>%
  distinct(
    Date, Station, Secchi, Conductivity, Temperature, TurbidityNTU, TurbidityFNU,
    .keep_all = TRUE
  ) %>%
  add_source_col(survey) %>%
  standardize_col_order()

usethis::use_data(SKT, overwrite = TRUE)

document_helper_other(SKT)
