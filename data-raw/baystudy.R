## code to prepare `baystudy` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(readxl)
require(stringr)

baystudy_stations <- read_excel(file.path("data-raw", "Baystudy", "Bay Study_Station Coordinates for Distribution_04May2020.xlsx"))%>%
  separate(Latitude, into=c("Lat_Deg", "Lat_Min"), sep = "°", convert=T)%>%
  separate(Longitude, into=c("Lon_Deg", "Lon_Min"), sep = "°", convert=T)%>%
  mutate(Latitude=Lat_Deg+Lat_Min/60,
         Longitude=Lon_Deg-Lon_Min/60)%>%
  select(Station, Latitude, Longitude)%>%
  filter(Station!="211E")%>%
  mutate(Station=recode(Station, `211W`="211"))

tidecodes_baystudy <- read_csv(file.path("data-raw", "Baystudy", "TideCodes_LookUp.csv"),
                               col_types=cols_only(Tide="i", Description="c"))


boatstation_baystudy <- read_csv(file.path("data-raw", "Baystudy", "BoatStation.csv"),
                                 col_types = cols_only(Year="i", Survey="i", Station="c",
                                                       Date="c", Depth="d", Secchi="d", Tide="i"))%>%
  mutate(Date=parse_date_time(Date, orders="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  left_join(tidecodes_baystudy, by="Tide")%>%
  select(-Tide)%>%
  rename(Tidestation=Description)%>%
  left_join(read_csv(file.path("data-raw", "Baystudy", "SalinTemp.csv"),
                     col_types = cols_only(Year="i", Survey="i", Station="c",
                                           ECSurf="d", TempSurf="d", TempBott="d")),
            by=c("Year", "Survey", "Station"))

boattow_baystudy<-read_csv(file.path("data-raw", "Baystudy", "BoatTow.csv"),
                           col_types = cols_only(Year="i", Survey="i", Station="c", Time="c", Tide="i",
                                                 StartLat = "d", StartLong = "d"))%>%
  mutate(Time=parse_date_time(Time, orders="%m/%d/%Y %H:%M:%S"))%>%
  group_by(Year, Survey, Station)%>%
  filter(Time==min(Time))%>% # Just keep tide and time-of-day records at the time of the first tow
  ungroup()%>%
  left_join(tidecodes_baystudy, by="Tide")%>%
  select(-Tide)%>%
  rename(Tidetow=Description)

baystudy <- left_join(boattow_baystudy, boatstation_baystudy, by=c("Year", "Survey", "Station"))%>%
  mutate(Tide=if_else(is.na(Tidetow), Tidestation, Tidetow), # If tide data were not collected at the time of the tow, use the value from the overall station visit
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Source="Baystudy")%>%
  # Add field coordinates for the stations without lat-long coordinates
  # From May 1981- April 1990, units were degrees, minutes and seconds; from May
  # 1990 on, degrees, minutes and decimal minutes
  mutate(
    LatDeg = as.numeric(str_sub(StartLat, start = 1, end = 2)),
    LatMin = if_else(
      Date < "1990-05-01",
      as.numeric(str_sub(StartLat, start = 3, end = 4)),
      as.numeric(paste0(str_sub(StartLat, start = 3, end = 4), ".", str_sub(StartLat, start = 5)))
    ),
    LatSec = if_else(Date < "1990-05-01", as.numeric(str_sub(StartLat, start = 5)), NA_real_),
    LongDeg = as.numeric(str_sub(StartLong, start = 1, end = 3)),
    LongMin = if_else(
      Date < "1990-05-01",
      as.numeric(str_sub(StartLong, start = 4, end = 5)),
      as.numeric(paste0(str_sub(StartLong, start = 4, end = 5), ".", str_sub(StartLong, start = 6)))
    ),
    LongSec = if_else(Date < "1990-05-01", as.numeric(str_sub(StartLong, start = 6)), NA_real_),
    Latitude = if_else(
      Date < "1990-05-01",
      LatDeg + LatMin/60 + LatSec/3600,
      LatDeg + LatMin/60
    ),
    Longitude = if_else(
      Date < "1990-05-01",
      (LongDeg + LongMin/60 + LongSec/3600) * -1,
      (LongDeg + LongMin/60) * -1
    )
  ) %>%
  left_join(baystudy_stations, by="Station", suffix = c("_field", ""))%>%
  mutate(
    Field_coords = case_when(
      is.na(Latitude) & !is.na(Latitude_field) ~ TRUE,
      is.na(Longitude) & !is.na(Longitude_field) ~ TRUE,
      TRUE ~ FALSE
    ),
    Latitude = if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude = if_else(is.na(Longitude), Longitude_field, Longitude)
  ) %>%
  select(Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Depth, Tide, Secchi, Temperature=TempSurf, Temperature_bottom=TempBott, Conductivity=ECSurf)%>%
  distinct(Source, Station, Date, Datetime, .keep_all=T)


usethis::use_data(baystudy, overwrite = TRUE)
