## code to prepare `twentymm` dataset goes here

require(readr)
require(dplyr)
require(lubridate)

twentymm <- read_csv(file.path("data-raw", "20mm", "Station.csv"),
                    col_types = cols_only(StationID="c", SurveyID="c", Station="c",
                                          LatDeg="d", LatMin="d", LatSec="d", LonDeg="d",
                                          LonMin="d", LonSec="d", Temp="d", TopEC="d",
                                          Secchi="d", Comments="c"))%>%
  mutate(Latitude=LatDeg+LatMin/60+LatSec/3600,
         Longitude=(LonDeg+LonMin/60+LonSec/3600)*-1)%>%
  left_join(read_csv(file.path("data-raw", "20mm", "Survey.csv"),
                     col_types = cols_only(SurveyID="c", SampleDate = "c"))%>%
              rename(Date=SampleDate)%>%
              mutate(Date = parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles")),
            by="SurveyID")%>%
  left_join(read_csv(file.path("data-raw", "20mm", "Tow.csv"),
                     col_types = cols_only(StationID="c", TowTime="c", Tide="d", BottomDepth="d", TowNum="d"))%>%
              rename(Time=TowTime)%>%
              mutate(Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
              group_by(StationID)%>%
              mutate(Retain=if_else(Time==min(Time), TRUE, FALSE))%>%
              ungroup()%>%
              filter(Retain)%>%
              select(-Retain)%>%
              group_by(StationID)%>%
              mutate(Retain=if_else(TowNum==min(TowNum), TRUE, FALSE))%>%
              ungroup()%>%
              filter(Retain)%>%
              select(-Retain, -TowNum),
            by="StationID")%>%
  select(Station, Temperature=Temp, Conductivity=TopEC, Secchi,
         Notes=Comments, Latitude, Longitude, Date,
         Time, Depth=BottomDepth, Tide)%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Source = "20mm",
         Depth = Depth*0.3048)%>% # Convert feet to meters
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Secchi, Temperature, Conductivity, Notes)


usethis::use_data(twentymm, overwrite = TRUE)
