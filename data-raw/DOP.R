# Code to prepare `DOP` dataset
library(readr)
library(dplyr)
library(lubridate)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

source("data-raw/01_Global/data_raw_helpers.R")

survey <- "DOP"

# Check if there is a more recent EDI update
edi_id_curr <- get_latest_edi_id(survey)

# Compile and subset data entities for EDI data package
edi_data_ent_all <- get_edi_data_entities(edi_id_curr)
edi_data_ent_sub <- grep("^DOP_ICF_TowData", edi_data_ent_all, value = TRUE)

# Download data entities to temporary directory
get_edi_data(edi_id_curr, edi_data_ent_sub)

# Determine file paths for data entities on temporary directory
temp_files <- list.files(tempdir(), full.names = TRUE)
file_data <- grep("DOP_ICF_TowData", temp_files, value = TRUE)

# Import data and perform checks and minor processing
DOP_orig <- import_raw_data(file_data, "DOP", "Data")

# Clean up data
# Notes:
  # We're not including Microcystis because they use a different method
  # Turbidity is measured with a YSI EXO2 sonde according to the DOP methods - units are FNU
DOP <- DOP_orig %>%
  # Remove Channel Deep and Oblique samples. Channel Deep measurements are taken
    # at the bottom third to half of the water column and therefore aren't
    # comparable to bottom samples from other surveys. The WQ measurements for the
    # Oblique tows are either all NA or they are identical to either the Channel
    # Surface or Channel Deep samples collected at the same location.
  filter(!Habitat %in% c("Channel Deep", "Oblique")) %>%
  mutate(Source = "DOP", .before = 1) %>%
  # Combine Station_Code and Habitat columns to make the Station column to
    # preserve habitat info for each station
  mutate(Station = paste(Station_Code, Habitat), .keep = "unused", .before = Latitude) %>%
  mutate(Field_coords = TRUE, .after = Longitude) %>%
  # Parse date and create a date-time column
  mutate(
    Date = ymd(Date, tz = "America/Los_Angeles"),
    Datetime = ymd_hms(
      if_else(is.na(Start_Time), NA_character_, paste(Date, Start_Time)),
      tz = "America/Los_Angeles"
    ),
    .keep = "unused", .after = Date
  ) %>%
  # Convert feet to meters
  mutate(Depth = Start_Depth * 0.3048, .keep = "unused", .before = Secchi) %>%
  # Remove replicate tows with identical WQ values - select earliest Datetime
  arrange(Datetime) %>%
  distinct(
    Station,
    Date,
    Secchi,
    Temperature,
    Salinity,
    Conductivity,
    DissolvedOxygen,
    pH,
    TurbidityFNU,
    Chlorophyll,
    .keep_all = TRUE
  ) %>%
  arrange(Date, Station)

usethis::use_data(DOP, overwrite = TRUE)

document_helper_edi(edi_id_curr, DOP)
update_edi_metadata(survey, edi_id_curr)
