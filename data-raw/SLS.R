## code to prepare `SLS` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(tidyr)

SLS_stations<-read_csv(file.path("data-raw", "SLS", "20mm Stations.csv"),
                       col_types=cols_only(Station="c", LatD="d", LatM="d",
                                           LatS="d", LonD="d", LonM="d", LonS="d"))%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1)%>%
  select(Station, Latitude, Longitude)%>%
  drop_na()

SLS<-read_csv(file.path("data-raw", "SLS", "Water Info.csv"),
              col_types=cols_only(Date="c", Station="c", Temp="d",
                                  TopEC="d", Secchi="d", Comments="c"))%>%
  left_join(SLS_stations, by="Station")%>%
  left_join(read_csv(file.path("data-raw", "SLS", "Tow Info.csv"),
                     col_types=cols_only(Date="c", Station="c",
                                         Time="c", Tide="d", BottomDepth="d")),
            by=c("Date", "Station"))%>%
  rename(Temperature=Temp, Conductivity=TopEC, Depth=BottomDepth, Notes=Comments)%>%
  mutate(Date=parse_date_time(Date, orders="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, orders="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(year(Date), month(Date), day(Date), hour(Time), minute(Time), sep="/")),
                                  orders="%Y/%m/%d/%H/%M"))%>%
  mutate(Source="SLS",
         Conductivity=if_else(str_detect(Notes, "CDEC"), NA_real_, Conductivity), # Remove any conductivity values that may have come from CDEC
         Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Depth = Depth*0.3048)%>% # Convert feet to meters
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Secchi, Temperature, Conductivity, Notes)



usethis::use_data(SLS, overwrite = TRUE)
