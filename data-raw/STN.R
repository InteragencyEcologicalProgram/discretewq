# Code to prepare `STN` dataset
library(dplyr)
library(lubridate)
library(tidyr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

survey <- "STN"

# Import data tables
fp_stn <- "data-raw/STN"

df_stations <- import_raw_data(file.path(fp_stn, "luStation.csv"), survey, "df_stations")
df_sample <- import_raw_data(file.path(fp_stn, "Sample.csv"), survey, "df_sample")
df_tow <- import_raw_data(file.path(fp_stn, "TowEffort.csv"), survey, "df_tow")

# Prepare tables before joining them together
df_stations_c <- df_stations %>%
  mutate(
    Latitude = Lat_Deg + Lat_Min / 60 + Lat_Sec / 3600,
    Longitude = (Long_Deg + Long_Min / 60 + Long_Sec / 3600) * -1,
    .keep = "unused"
  ) %>%
  drop_na()

df_tow_c <- df_tow %>%
  # Use the time of the first tow
  mutate(Time = mdy_hms(Time)) %>%
  drop_na() %>%
  summarize(Time = min(Time), .by = SampleRowID)

# Join tables together and finish cleaning data
STN <- df_sample %>%
  mutate(
    Latitude = Lat_Deg + Lat_Min / 60 + Lat_Sec / 3600,
    Longitude = (Long_Deg + Long_Min / 60 + Long_Sec / 3600) * -1,
    .keep = "unused"
  ) %>%
  left_join(df_tow_c, by = join_by(SampleRowID)) %>%
  mutate(
    Time = format(Time, "%H:%M:%S"),
    Date = str_extract(Date, ".+(?=\\s)")
  ) %>%
  convert_datetime(date_format = "mdY", time_format = "HMS", timezone = "America/Los_Angeles") %>%
  # Standardize tide codes
  mutate(Tide = case_match(Tide, 4 ~ "Flood", 3 ~ "Low Slack", 2 ~ "Ebb", 1 ~ "High Slack")) %>%
  # Convert Depth from feet to meters
  convert_depth(depth_unit = "feet") %>%
  left_join(df_stations_c, by = join_by(Station), suffix = c("_field", "")) %>%
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
  # Remove Field_coords column because they are all FALSE - this will be automated
  select(-Field_coords) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  standardize_col_order()

usethis::use_data(STN, overwrite = TRUE)

document_helper_other(STN)
