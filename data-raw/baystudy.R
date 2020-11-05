## code to prepare `baystudy` dataset goes here

require(readr)
require(dplyr)
require(lubridate)

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
  select(Source, Station, Date, Datetime, Depth, Tide, Secchi, Temperature=TempSurf, Temperature_bottom=TempBott, Conductivity=ECSurf)%>%
  distinct(Source, Station, Date, Datetime, .keep_all=T)


usethis::use_data(baystudy, overwrite = TRUE)
