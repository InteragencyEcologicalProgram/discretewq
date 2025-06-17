# Code to prepare `SLS` dataset
library(dplyr)
library(tidyr)
library(lubridate)

source("data-raw/01_Global/data_raw_helpers.R")

survey <- "SLS"

# Run standardized workflow to import data from EDI and process it
ls_SLS <- import_proc_edi_data(survey)

# Prepare station table before joining it to data tables
df_stations <- ls_SLS$df_stations %>%
  separate_wider_delim(Latitude, delim = " ", names = c("Lat_Deg", "Lat_Min", "Lat_Sec")) %>%
  separate_wider_delim(Longitude, delim = " ", names = c("Long_Deg", "Long_Min", "Long_Sec")) %>%
  mutate(across(starts_with(c("Lat_", "Long_")), as.numeric)) %>%
  mutate(
    Latitude = Lat_Deg + Lat_Min / 60 + Lat_Sec / 3600,
    Longitude = (Long_Deg + Long_Min / 60 + Long_Sec / 3600) * -1,
    .keep = "unused"
  )

# Join data entities and finish cleaning data
SLS <- ls_SLS$df_water_info %>%
  left_join(ls_SLS$df_tow, by = join_by(Date, Station)) %>%
  # Parse Date and Time columns
  mutate(Time = str_extract(Time, "(?<=\\s).+")) %>%
  convert_datetime(date_format = "Ymd", time_format = "HMS", timezone = "America/Los_Angeles") %>%
  # Standardize tide codes
  mutate(Tide = case_match(Tide, 4 ~ "Flood", 3 ~ "Low Slack", 2 ~ "Ebb", 1 ~ "High Slack")) %>%
  left_join(df_stations, by = join_by(Station)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  standardize_col_order()

usethis::use_data(SLS, overwrite = TRUE)

document_helper_edi(ls_SLS$edi_id, SLS)
update_edi_metadata(survey, ls_SLS$edi_id)
