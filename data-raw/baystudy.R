# Code to prepare `baystudy` dataset
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "Baystudy"
tzone <- "America/Los_Angeles"

# Run standardized workflow to import data and process it
# edi_id <- get_edi_data(survey)
ls_baystudy <- import_proc_data(survey)

# Prepare tables before joining them together
df_stations_c <- ls_baystudy$df_stations %>%
  separate_lat_long(delim_chr = "°", coord_comp = "DM") %>%
  convert_lat_long(coord_comp = "DM") %>%
  filter(Station != "211E")%>%
  mutate(Station = if_else(Station == "211W", "211", Station))

df_boat_tow_c <- ls_baystudy$df_boat_tow %>%
  left_join(ls_baystudy$df_tide_codes, by = join_by(Tide)) %>%
  rename(Tide_tow = Description) %>%
  select(-Tide)

df_boat_station_c <- ls_baystudy$df_boat_station %>%
  left_join(ls_baystudy$df_tide_codes, by = join_by(Tide)) %>%
  rename(Tide_station = Description) %>%
  select(-Tide)

# Join tables together and finish cleaning data
baystudy <- list(df_boat_tow_c, df_boat_station_c, ls_baystudy$df_sal_wt) %>%
  reduce(\(x, y) full_join(x, y, by = join_by(Year, Survey, Station))) %>%
  combine_datetime(timezone = tzone) %>%
  # Just keep tide and time-of-day records at the time of the first tow
  group_by(Year, Survey, Station) %>%
  filter(Datetime == min(Datetime)) %>%
  ungroup() %>%
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
  resolve_lat_long() %>%
  # If tide data were not collected at the time of the tow, use the value from the overall station
    # visit
  mutate(Tide = if_else(is.na(Tide_tow), Tide_station, Tide_tow), .keep = "unused") %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove a few duplicated records
  distinct(Station, Datetime, .keep_all = TRUE) %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Datetime) %>%
  add_update_info()

usethis::use_data(baystudy, overwrite = TRUE)
document_helper_other(baystudy)
