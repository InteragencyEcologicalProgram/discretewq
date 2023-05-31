## code to prepare `STN` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(tidyr)

STN_stations<-read_csv(file.path("data-raw", "STN", "luStation.csv"),
              col_types = cols_only(StationCodeSTN="c", LatD='d', LatM="d", LatS="d",
                                    LonD='d', LonM='d', LonS='d'))%>%
  rename(Station=StationCodeSTN)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1)%>%
  select(Station, Latitude, Longitude)%>%
  drop_na()

STN<-read_csv(file.path("data-raw", "STN", "Sample.csv"),
                 col_types=cols_only( `Sample Date`="c", `Station Code`="c",
                                     `Temperature Top`="d", `Temperature Bottom`="d",
                                     Secchi="d", `Conductivity Top`="d",
                                     `Tide Code`="i", `Depth Bottom`='d',
                                     Microcystis="d", `First Tow Start Time` = "c"))%>%
  rename(Date=`Sample Date`, Station=`Station Code`, Time = `First Tow Start Time`,
         Temperature= `Temperature Top`, Conductivity=`Conductivity Top`,
         Tide=`Tide Code`, Depth=`Depth Bottom`,
         Temperature_bottom=`Temperature Bottom`)%>%
  mutate(Source="STN",
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%H:%M", tz="America/Los_Angeles"))%>%

  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Depth = Depth*0.3048)%>% #Convert feet to meters
  left_join(STN_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Microcystis, Secchi, Temperature, Temperature_bottom, Conductivity)

usethis::use_data(STN, overwrite = TRUE)
