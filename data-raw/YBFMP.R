## code to prepare `YBFMP` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(tidyr)

# Function to calculate mode
# Modified from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode to remove NAs
Mode <- function(x) {
  x<-na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.2&entityid=f0a145a59e6659c170988fa6afa3f232",
              file.path(tempdir(), "Zoop_data_20211205.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.2&entityid=89146f1382d7dfa3bbf3e4b1554eb5cc",
              file.path(tempdir(), "Stations.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=4488201fee45953b001f70acf30f7734",
              file.path(tempdir(), "event.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=89146f1382d7dfa3bbf3e4b1554eb5cc",
              file.path(tempdir(), "station.csv"), mode="wb",method="libcurl")

YBFMP_stations<-read_csv(file.path(tempdir(), "Stations.csv"),
                         col_types=cols_only(StationCode="c", Latitude="d", Longitude="d"))%>%
  rename(Station=StationCode)%>%
  bind_rows(
    read_csv(file.path(tempdir(), "station.csv"),
             col_types=cols_only(StationCode="c", Latitude="d", Longitude="d"))%>%
      rename(Station=StationCode))%>%
  distinct()

YBFMP_zoop <-
  read_csv(
    file.path(tempdir(), "Zoop_data_20211205.csv"),
    col_types = cols_only(
      Date = "c",
      Datetime = "c",
      StationCode = "c",
      Tide = "c",
      WaterTemperature = "d",
      Secchi = "d",
      Conductivity = "d",
      SpCnd = "d",
      MicrocystisVisualRank = "d",
      DO="d",
      pH="d",
      FieldComments = "c"
    )
  ) %>%
  transmute(
    Date = ymd(Date, tz = "America/Los_Angeles"),
    Datetime = ymd_hms(Datetime, tz = "America/Los_Angeles"),
    Station = StationCode,
    Tide = recode(Tide, High="High Slack", Low="Low Slack"),
    Temperature = WaterTemperature,
    # Convert Secchi to cm
    Secchi = Secchi * 100,
    # Convert Conductivity to Specific Conductance, see suisun.R for info
    Conductivity = if_else(
      is.na(SpCnd),
      Conductivity / (1 + 0.019 * (Temperature - 25)),
      SpCnd
    ),
    Microcystis = MicrocystisVisualRank,
    DissolvedOxygen=DO,
    pH=pH,
    Notes = FieldComments
  ) %>%
  # Remove records where all parameters are NA
  filter(!if_all(where(is.numeric), is.na)) %>%
  # Remove duplicated records across all columns
  distinct()

YBFMP_fish <-
  read_csv(
    file.path(tempdir(), "event.csv"),
    col_types = cols_only(
      Datetime = "c",
      SampleDate = "c",
      StationCode = "c",
      WaterTemp = "d",
      Secchi = "d",
      Conductivity = "d",
      SpecificConductance = "d",
      Tide = "c",
      MicrocystisRank = "d",
      DO="d",
      pH="d",
      FieldComments = "c"
    )
  ) %>%
  transmute(
    Date = mdy(SampleDate, tz = "America/Los_Angeles"),
    Datetime = mdy_hm(Datetime, tz = "America/Los_Angeles"),
    Station = StationCode,
    Tide = recode(Tide, High="High Slack", Low="Low Slack"),
    Temperature = WaterTemp,
    # Convert Secchi to cm
    Secchi = Secchi * 100,
    # Convert Conductivity to Specific Conductance, see suisun.R for info
    Conductivity = if_else(
      is.na(SpecificConductance),
      Conductivity / (1 + 0.019 * (Temperature - 25)),
      SpecificConductance
    ),
    Microcystis = MicrocystisRank,
    DissolvedOxygen=DO,
    pH=pH,
    Notes = FieldComments
  ) %>%
  # Remove records where all parameters are NA
  filter(!if_all(where(is.numeric), is.na)) %>%
  # Remove duplicated records across all columns
  distinct()

# Pull out duplicated records with identical Station-Datetime combinations from fish data
YBFMP_fish_dups <- YBFMP_fish %>%
  count(Station, Datetime) %>%
  filter(n > 1) %>%
  select(-n) %>%
  left_join(YBFMP_fish)

# Clean up the duplicated records
YBFMP_fish_dups_c <- YBFMP_fish_dups %>%
  # Most of the duplicated records are due to different values in the Notes column
  distinct(Date, Datetime, Station, Tide, Temperature, Secchi, Conductivity, DissolvedOxygen, pH, Microcystis, .keep_all = TRUE) %>%
  # Remove the one record with an NA value for Secchi since its pair has a value
  drop_na(Secchi)

# Add the cleaned up duplicate data to the original fish data set
YBFMP_fish_c <- YBFMP_fish %>%
  anti_join(YBFMP_fish_dups) %>%
  bind_rows(YBFMP_fish_dups_c) %>%
  # There are numerous records that were collected on the same day and station
  # that have identical water quality measurements. After speaking with Nicole
  # Kwan (SES Supervisor for the AES Unit), we decided to only keep the records
  # with the earliest Datetime for the groups of records that share identical
  # water quality measurements with different Datetimes.
  group_by(Date, Station, Temperature, Secchi, Conductivity, Microcystis, DissolvedOxygen, pH) %>%
  filter(Datetime == min(Datetime)) %>%
  ungroup() %>%
  # There are two records (STTD on 2017-01-23) that share identical Temperature,
  # Secchi, and Conductivity, but only one record has a Microcystis value (the
  # record with the later Datetime). For this instance, we'll keep both records
  # and convert Temperature, Secchi, and Conductivity to NA for the record
  # with a reported Microcystis value.
  mutate(
    across(c(Temperature, Secchi, Conductivity, DissolvedOxygen, pH),  ~if_else(Station == "STTD" & Datetime == "2017-01-23 13:30:00", NA_real_, .x))
  )

# Combine the zoop and fish WQ data sets
YBFMP_comb <- bind_rows(YBFMP_zoop, YBFMP_fish_c) %>%
  # Remove the duplicated records shared between the two data sets (STTD)
  distinct()

# Pull out the remaining duplicated records shared between the two data sets
YBFMP_comb_dups <- YBFMP_comb %>%
  count(Station, Datetime) %>%
  filter(n > 1) %>%
  select(-n) %>%
  left_join(YBFMP_comb)

# Clean up the remaining duplicated records
YBFMP_comb_dups_c <- YBFMP_comb_dups %>%
  # Most of the duplicated records are due to different values in the Notes column
  distinct(Date, Datetime, Station, Tide, Temperature, Secchi, Conductivity, DissolvedOxygen, pH, Microcystis, .keep_all = TRUE) %>%
  # Clean up the remaining duplicates - identical water quality measurements
  # with the exception of Microcystis - remove the pairs that have NA index
  # values
  arrange(Datetime, Microcystis) %>%
  group_by(Datetime, Tide, Temperature, Secchi, Conductivity, DissolvedOxygen, pH) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  filter(row_num == 1) %>%
  # Clean up one last duplicate pair - records have different water quality
  # measurements but same Datetime (one is from the zoop and the other is from
  # the fish data set) - average the water quality values
  group_by(Datetime) %>%
  mutate(
    across(c(Temperature, Secchi, Conductivity, DissolvedOxygen, pH), mean),
    Secchi = round(Secchi),
    row_num = row_number()
  ) %>%
  ungroup() %>%
  filter(row_num == 1) %>%
  select(-row_num)

# Add the cleaned up duplicate data to the combined data set and finish cleaning
YBFMP <- YBFMP_comb %>%
  anti_join(YBFMP_comb_dups) %>%
  bind_rows(YBFMP_comb_dups_c) %>%
  mutate(Source = "YBFMP") %>%
  left_join(YBFMP_stations, by = "Station") %>%
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    Tide,
    Microcystis,
    Secchi,
    Temperature,
    Conductivity,
    DissolvedOxygen,
    pH,
    Notes
  ) %>%
  arrange(Datetime)

usethis::use_data(YBFMP, overwrite = TRUE)
