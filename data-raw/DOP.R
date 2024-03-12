# Code to prepare DOP data goes here
require(readr)
require(dplyr)
require(lubridate)
require(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# read in dataset
download.file(
  "https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.4&entityid=0dbed7163901c12df414da0762d28a86",
  file.path(tempdir(), "DOP_ICF_TowData_2017-2022.csv"),
  mode = "wb",
  method = "libcurl"
)

DOP_orig <- read_csv(
  file.path(tempdir(), "DOP_ICF_TowData_2017-2022.csv"),
  col_types = cols_only(
    Date = "c",
    Latitude = "d",
    Longitude = "d",
    Station_Code = "c",
    Habitat = "c",
    Conductivity = "d",
    Temperature = "d",
    Secchi = "d",
    Start_Time = "c",
    pH = "d",
    Chl_a = "d",
    Start_Depth = "d",
    Salinity = "d",
    Turbidity = "d",
    DO = "d"
  )
)

# Clean up data
# Note - We're not including Microcystis because they use a different method
DOP <- DOP_orig %>%
  transmute(
    Source = "DOP",
    # Combine Station_Code and Habitat columns to make the Station column to
    # preserve habitat info for each station
    Station = paste(Station_Code, Habitat),
    Habitat,
    Latitude,
    Longitude,
    Field_coords = TRUE,
    Date = ymd(Date, tz = "America/Los_Angeles"),
    # Make a date-time column
    Datetime = ymd_hms(if_else(is.na(Start_Time), NA_character_, paste(Date, Start_Time)), tz = "America/Los_Angeles"),
    # Convert feet to meters
    Depth = Start_Depth * 0.3048,
    Secchi,
    Temperature,
    Salinity,
    Conductivity,
    DissolvedOxygen = DO,
    pH,
    # Turbidity is measured with a YSI EXO2 sonde according to the DOP methods - units are FNU
    TurbidityFNU = Turbidity,
    Chlorophyll = Chl_a
  ) %>%
  # Remove Channel Deep and Oblique samples. Channel Deep measurements are taken
  # at the bottom third to half of the water column and therefore aren't
  # comparable to bottom samples from other surveys. The WQ measurements for the
  # Oblique tows are either all NA or they are identical to either the Channel
  # Surface or Channel Deep samples collected at the same location.
  filter(!Habitat %in% c("Channel Deep", "Oblique")) %>%
  select(-Habitat) %>%
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

