## code to prepare `baystudy` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(readxl)

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
                           col_types = cols_only(Year="i", Survey="i", Station="c", Time="c", Tide="i"))%>%
  mutate(Time=parse_date_time(Time, orders="%m/%d/%Y %H:%M:%S"))%>%
  group_by(Year, Survey, Station)%>%
  filter(Time==min(Time))%>% # Just keep records at the time of the first tow
  ungroup()%>%
  left_join(tidecodes_baystudy, by="Tide")%>%
  select(-Tide)%>%
  rename(Tidetow=Description)

baystudy <- left_join(boattow_baystudy, boatstation_baystudy, by=c("Year", "Survey", "Station"))%>%
  mutate(Tide=if_else(is.na(Tidetow), Tidestation, Tidetow),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Source="Baystudy")%>%
  left_join(baystudy_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Secchi, Temperature=TempSurf, Temperature_bottom=TempBott, Conductivity=ECSurf)%>%
  distinct(Source, Station, Date, Datetime, .keep_all=T)


usethis::use_data(baystudy, overwrite = TRUE)
