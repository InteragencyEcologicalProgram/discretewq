# Code to prepare `baystudy` dataset
library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)
library(stringr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

survey <- "Baystudy"

# Import data tables
fp_baystudy <- "data-raw/Baystudy"

df_stations <- import_raw_data(
  file.path(fp_baystudy, "Bay Study_Station Coordinates for Distribution_04May2020.xlsx"),
  survey, "df_stations", read_excel, col_types = "text"
)

df_tide_codes <- import_raw_data(
  file.path(fp_baystudy, "TideCodes_LookUp.csv"), survey, "df_tide_codes"
)

df_boat_station <- import_raw_data(
  file.path(fp_baystudy, "BoatStation.csv"), survey, "df_boat_station"
)

df_sal_wt <- import_raw_data(file.path(fp_baystudy, "SalinTemp.csv"), survey, "df_sal_wt")
df_boat_tow <- import_raw_data(file.path(fp_baystudy, "BoatTow.csv"), survey, "df_boat_tow")

# Prepare tables before joining them together
df_stations_c <- df_stations %>%
  separate_wider_delim(Latitude, delim = "°", names = c("Lat_Deg", "Lat_Min")) %>%
  separate_wider_delim(Longitude, delim = "°", names = c("Long_Deg", "Long_Min")) %>%
  mutate(across(starts_with(c("Lat_", "Long_")), as.numeric)) %>%
  mutate(
    Latitude = Lat_Deg + Lat_Min / 60,
    Longitude = Long_Deg - Long_Min / 60,
    .keep = "unused"
  ) %>%
  filter(Station != "211E")%>%
  mutate(Station = if_else(Station == "211W", "211", Station))

df_boat_tow_c <- df_boat_tow %>%
  # Just keep tide and time-of-day records at the time of the first tow
  mutate(Time = mdy_hms(Time)) %>%
  group_by(Year, Survey, Station) %>%
  filter(Time == min(Time)) %>%
  ungroup() %>%
  left_join(df_tide_codes, by = join_by(Tide)) %>%
  rename(Tide_tow = Description) %>%
  select(-Tide)

df_data <- df_boat_station %>%
  left_join(df_tide_codes, by = join_by(Tide)) %>%
  rename(Tide_station = Description) %>%
  select(-Tide) %>%
  left_join(df_sal_wt, by = join_by(Year, Survey, Station))

# Join tables together and finish cleaning data
baystudy <- df_boat_tow_c %>%
  left_join(df_data, by = join_by(Year, Survey, Station)) %>%
  mutate(
    Time = paste(hour(Time), minute(Time), sep = ":"),
    Date = str_extract(Date, ".+(?=\\s)")
  ) %>%
  convert_datetime(date_format = "mdY", time_format = "HM", timezone = "America/Los_Angeles") %>%
  # Add field coordinates for the stations without lat-long coordinates
  # From May 1981- April 1990, units were degrees, minutes and seconds; from May 1990 on, degrees
    # and decimal minutes
  mutate(
    Lat_Deg = str_sub(Latitude, start = 1, end = 2),
    Lat_Min = case_when(
      is.na(Latitude) ~ NA_character_,
      Date < "1990-05-01" ~ str_sub(Latitude, start = 3, end = 4),
      TRUE ~ paste0(str_sub(Latitude, start = 3, end = 4), ".", str_sub(Latitude, start = 5))
    ),
    Lat_Sec = if_else(Date < "1990-05-01", str_sub(Latitude, start = 5), NA_character_),
    Long_Deg = str_sub(Longitude, start = 1, end = 3),
    Long_Min = case_when(
      is.na(Longitude) ~ NA_character_,
      Date < "1990-05-01" ~ str_sub(Longitude, start = 4, end = 5),
      TRUE ~ paste0(str_sub(Longitude, start = 4, end = 5), ".", str_sub(Longitude, start = 6))
    ),
    Long_Sec = if_else(Date < "1990-05-01", str_sub(Longitude, start = 6), NA_character_)
  ) %>%
  mutate(across(starts_with(c("Lat_", "Long_")), as.numeric)) %>%
  mutate(
    Latitude = if_else(
      Date < "1990-05-01",
      Lat_Deg + Lat_Min / 60 + Lat_Sec / 3600,
      Lat_Deg + Lat_Min / 60
    ),
    Longitude = if_else(
      Date < "1990-05-01",
      (Long_Deg + Long_Min / 60 + Long_Sec / 3600) * -1,
      (Long_Deg + Long_Min / 60) * -1
    )
  ) %>%
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
  # If tide data were not collected at the time of the tow, use the value from the overall station
    # visit
  mutate(Tide = if_else(is.na(Tide_tow), Tide_station, Tide_tow), .keep = "unused") %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove a few duplicated records
  distinct(Station, Datetime, .keep_all = TRUE) %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Datetime)

usethis::use_data(baystudy, overwrite = TRUE)

document_helper_other(baystudy)
