## code to prepare `FMWT` dataset goes here

library(readr)
library(dplyr)
library(lubridate)
require(tidyr)

FMWT_stations<-read_csv(file.path("data-raw", "FMWT", "StationsLookUp.csv"),
                        col_types = cols_only(StationCode="c", DD_Latitude="d", DD_Longitude="d"))%>%
  rename(Station=StationCode, Latitude=DD_Latitude, Longitude=DD_Longitude)%>%
  drop_na()

FMWT <- read_csv(file.path("data-raw", "FMWT", "Sample.csv"),
                 col_types = cols_only(StationCode="c", SampleDate="c", SampleTimeStart="c",
                                       WaterTemperature="d", Secchi="d", SecchiEstimated="l",
                                       ConductivityTop="d", TideCode="i", DepthBottom="d",
                                       Microcystis="d", BottomTemperature="d"))%>%
  rename(Station=StationCode, Date=SampleDate, Tide=TideCode, Time=SampleTimeStart,
         Depth=DepthBottom, Conductivity=ConductivityTop, Temperature=WaterTemperature,
         Secchi_estimated=SecchiEstimated, Temperature_bottom=BottomTemperature)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=if_else(hour(Time)==0, parse_date_time(NA_character_, tz="America/Los_Angeles"), Time),
         Tide=recode(Tide, `1` = "High Slack", `2` = "Ebb", `3` = "Low Slack", `4` = "Flood"),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Microcystis=if_else(Microcystis==6, 2, Microcystis),
         Temperature_bottom=na_if(Temperature_bottom, 0),
         Source="FMWT",
         Secchi=Secchi*100, #convert to cm
         Depth = Depth*0.3048)%>% # Convert to meters
  left_join(FMWT_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Microcystis, Secchi, Secchi_estimated, Temperature, Temperature_bottom, Conductivity)%>%
  distinct(Source, Station, Date, Datetime, .keep_all=T)

usethis::use_data(FMWT, overwrite = TRUE)
