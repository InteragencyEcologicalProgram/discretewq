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
                    col_types = cols_only(StationCode="c", SampleTimeStart="c", WaterTemperature="d", Secchi="d",
                                          ConductivityTop="d", TideCode="i", DepthBottom="d",
                                          Microcystis="d", BottomTemperature="d", DateID="i"))%>%
  left_join(read_csv(file.path("data-raw", "FMWT", "Date.csv"), col_types=cols_only(DateID="i", SampleDate="c"))%>%
              mutate(SampleDate=parse_date_time(SampleDate, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
              rename(Date=SampleDate),
            by="DateID")%>% # Add dates
  rename(Station=StationCode, Tide=TideCode, Time=SampleTimeStart, Depth=DepthBottom, Conductivity=ConductivityTop, Temperature=WaterTemperature,
         Temperature_bottom=BottomTemperature)%>%
  mutate(Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=if_else(hour(Time)==0, parse_date_time(NA_character_, tz="America/Los_Angeles"), Time),
         Tide=recode(Tide, `1` = "High Slack", `2` = "Ebb", `3` = "Low Slack", `4` = "Flood"),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  mutate(Microcystis=if_else(Microcystis==6, 2, Microcystis),
         Source="FMWT",
         Secchi=Secchi*100, #convert to cm
         Depth = Depth*0.3048)%>% # Convert to meters
  left_join(FMWT_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Microcystis, Secchi, Temperature, Temperature_bottom, Conductivity)%>%
  distinct(Source, Station, Date, Datetime, .keep_all=T)

usethis::use_data(FMWT, overwrite = TRUE)
