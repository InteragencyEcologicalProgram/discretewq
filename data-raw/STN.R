## code to prepare `STN` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(tidyr)

STN_stations <-
  read_csv(
    file.path("data-raw", "STN", "luStation.csv"),
    col_types = cols_only(
      StationCodeSTN = "c",
      LatD = "d",
      LatM = "d",
      LatS = "d",
      LonD = "d",
      LonM = "d",
      LonS = "d"
    )
  ) %>%
  rename(Station = StationCodeSTN) %>%
  mutate(
    Latitude = LatD + LatM / 60 + LatS / 3600,
    Longitude = (LonD + LonM / 60 + LonS / 3600) * -1
  ) %>%
  select(Station, Latitude, Longitude) %>%
  drop_na()

STN <-
  read_csv(
    file.path("data-raw", "STN", "Sample.csv"),
    col_types = cols_only(
      SampleRowID = "i",
      SampleDate = "c",
      StationCode = "c",
      TemperatureTop = "d",
      TemperatureBottom = "d",
      Secchi = "d",
      ConductivityTop = "d",
      TideCode = "i",
      DepthBottom = "d",
      SampleComments = "c",
      Microcystis = "d"
    )
  ) %>%
  rename(
    Date = SampleDate,
    Station = StationCode,
    Temperature = TemperatureTop,
    Conductivity = ConductivityTop,
    Tide = TideCode,
    Depth = DepthBottom,
    Notes = SampleComments,
    Temperature_bottom = TemperatureBottom
  ) %>%
  mutate(
    Source = "STN",
    Date = parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles")
  ) %>%
  left_join(
    read_csv(
      file.path("data-raw", "STN", "TowEffort.csv"),
      col_types = cols_only(SampleRowID = "i", TimeStart = "c")
    ) %>%
      select(SampleRowID, Time = TimeStart) %>%
      mutate(Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles")) %>%
      drop_na() %>%
      group_by(SampleRowID) %>%
      summarise(Time = min(Time), .groups = "drop"), # Use the time of the first tow
    by = "SampleRowID"
  ) %>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz = "America/Los_Angeles")) %>%
  mutate(
    Tide = recode(as.character(Tide), `4` = "Flood", `3` = "Low Slack", `2` = "Ebb", `1` = "High Slack"),
    Depth = Depth * 0.3048
  ) %>% # Convert feet to meters
  left_join(STN_stations, by = "Station") %>%
  select(
    Source,
    Station,
    Latitude,
    Longitude,
    Date,
    Datetime,
    Depth,
    Tide,
    Microcystis,
    Secchi,
    Temperature,
    Temperature_bottom,
    Conductivity,
    Notes
  )

usethis::use_data(STN, overwrite = TRUE)
