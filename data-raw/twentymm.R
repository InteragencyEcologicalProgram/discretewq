# Code to prepare `twentymm` dataset
library(dplyr)
library(lubridate)

source("data-raw/01_Global/data_raw_helpers.R")

survey <- "20mm"

# Run standardized workflow to import data from EDI and process it
ls_20mm <- import_proc_edi_data(survey)

# Prepare individual data entities before joining them together
df_stations <- ls_20mm$df_stations %>%
  mutate(
    Latitude = LatD + LatM / 60 + LatS / 3600,
    Longitude = (LonD + LonM / 60 + LonS / 3600) * -1,
    .keep = "unused"
  ) %>%
  drop_na()

df_data <- ls_20mm$df_data %>%
  mutate(
    Latitude_field = StartLatDeg + StartLatMin / 60 + StartLatSec / 3600,
    Longitude_field = (StartLonDeg + StartLonMin / 60 + StartLonSec / 3600) * -1,
    .keep = "unused"
  )

df_tow <- ls_20mm$df_tow %>%
  mutate(
    Time = ymd_hms(Time, tz = "America/Los_Angeles"),
    # Correct a few erroneous times (most likely not recorded in military time format)
    Time = if_else(hour(Time) %in% 1:2, Time + hours(12), Time),
    # Standardize tide codes
    Tide = case_match(Tide, 4 ~ "Flood", 3 ~ "Low Slack", 2 ~ "Ebb", 1 ~ "High Slack")
  )

# Join data entities and finish cleaning data
twentymm <- df_data %>%
  left_join(ls_20mm$df_survey, by = join_by(SurveyID)) %>%
  left_join(df_tow, by = join_by(StationID)) %>%
  # Only keep info for the first tow of each day (first defined by time then by tow number for ties)
  group_by(StationID) %>% # StationID really is SampleID
  filter(Time == min(Time, na.rm = TRUE)) %>%
  filter(TowNum == min(TowNum)) %>%
  ungroup() %>%
  mutate(Time = as.character(paste(hour(Time), minute(Time), sep = ":"))) %>%
  convert_datetime(date_format = "Ymd", time_format = "HM", timezone = "America/Los_Angeles") %>%
  left_join(df_stations, by = join_by(Station)) %>%
  mutate(
    Field_coords = case_when(
      is.na(Latitude) & !is.na(Latitude_field) ~ TRUE,
      is.na(Longitude) & !is.na(Longitude_field) ~ TRUE,
      TRUE ~ FALSE
    ),
    Latitude = if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude = if_else(is.na(Longitude), Longitude_field, Longitude)
  ) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Datetime)

usethis::use_data(twentymm, overwrite = TRUE)

document_helper_edi(ls_20mm$edi_id, twentymm)
update_edi_metadata(survey, ls_20mm$edi_id)
