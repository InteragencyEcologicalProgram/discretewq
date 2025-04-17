## code to prepare `DJFMP` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(hms)

source("data-raw/01_Global/data_raw_helpers.R")

# Define previous EDI ID used for this data set
edi_id_prev <- 244.13

# Check if there is a more recent EDI update
edi_id_curr <- get_latest_edi_id(edi_id_prev)

# Compile and subset data entities for EDI data package
edi_data_ent_all <- get_edi_data_entities(edi_id_curr)
edi_data_ent_sub <- grep("DJFMP_(trawl|Site)", edi_data_ent_all, value = TRUE)

# Download data entities to temporary directory
get_edi_data(edi_id_curr, edi_data_ent_sub)

# Determine file paths for data entities on temporary directory
temp_files <- list.files(tempdir(), full.names = TRUE)
file_data_1976_2001 <- grep("1976.+DJFMP_trawl", temp_files, value = TRUE)
file_data_2002_curr <- grep("2002.+DJFMP_trawl", temp_files, value = TRUE)
file_stations <- grep("DJFMP_Site_Locations", temp_files, value = TRUE)

# Import data and perform checks and minor processing
DJFMP_stations <- import_raw_data(file_stations, "DJFMP", "Stations")
DJFMP_1976_2001 <- import_raw_data(file_data_1976_2001, "DJFMP", "Data_1976-2001")
DJFMP_2002_curr <- import_raw_data(file_data_2002_curr, "DJFMP", "Data_2002-curr")

# Combine and finish cleaning data
DJFMP <- bind_rows(DJFMP_1976_2001, DJFMP_2002_curr) %>%
  mutate(
    # Convert Secchi to cm
    Secchi = Secchi * 100,
    Source = "DJFMP",
    Date = parse_date_time(Date, "mdy", tz = "America/Los_Angeles"),
    Datetime = ymd_hms(
      if_else(is.na(SampleTime), NA_character_, paste(Date, SampleTime)),
      tz = "America/Los_Angeles"
    ),
    # Removing conductivity data from dates before it was standardized >
    # Methods in  metadata say they do not know if their data were corrected for
    # temperature before May 3 or 17 2019 so we will not use conductivity data before
    # June 2019
    Conductivity = if_else(Date < "2019-06-01", NA_real_, Conductivity),
    .keep = "unused"
  ) %>%
  distinct(Station, Datetime, .keep_all = TRUE) %>%
  left_join(DJFMP_stations, by = join_by(Station)) %>%
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    Secchi,
    Temperature,
    Conductivity,
    DissolvedOxygen,
    TurbidityNTU
  )

usethis::use_data(DJFMP, overwrite = TRUE)

get_edi_update_info(edi_id_curr, DJFMP)
