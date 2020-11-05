## code to prepare `EMP` dataset goes here

require(readr)
require(dplyr)
require(lubridate)

Download <- FALSE

if(Download){
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.3&entityid=abedcf730c2490fde0237df58c897556", file.path("data-raw", "EMP", "SACSJ_delta_water_quality_1975_2019.csv"), mode="wb")
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.3&entityid=5b439de4be6d3a5964867b0cb4a1f059", file.path("data-raw", "EMP", "EMP_Discrete_Water_Quality_Stations_1975-2019.csv"), mode="wb")
}

EMP_stations<-read_csv(file.path("data-raw", "EMP", "EMP_Discrete_Water_Quality_Stations_1975-2019.csv"),
                       col_types=cols_only(Station="c", Latitude="d", Longitude="d"))%>%
  drop_na()

EMP<-read_csv(file.path("data-raw", "EMP", "SACSJ_delta_water_quality_1975_2019.csv"), na=c("NA", "ND"),
              col_types = cols_only(Station="c", Date="c", Time="c", Chla="d",
                                    Depth="d", Secchi="d", Microcystis="d", SpCndSurface="d",
                                    WTSurface="d", WTBottom='d', NorthLat='d', WestLong='d'))%>%
  rename(Chlorophyll=Chla, Conductivity=SpCndSurface, Temperature=WTSurface,
         Temperature_bottom=WTBottom, Latitude=NorthLat, Longitude=WestLong)%>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%m/%d/%Y %H:%M", tz="America/Los_Angeles"),
         Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
         Microcystis=round(Microcystis))%>% #EMP has some 2.5 and 3.5 values
  select(-Time)%>%
  mutate(Datetime=if_else(hour(Datetime)==0, parse_date_time(NA_character_, tz="America/Los_Angeles"), Datetime))%>%
  mutate(Source="EMP",
         Tide = "High Slack",
         Depth=Depth*0.3048)%>% # Convert feet to meters
  left_join(EMP_stations, by="Station", suffix=c("_field", ""))%>%
  mutate(Field_coords=case_when(
      is.na(Latitude) & !is.na(Latitude_field) ~ TRUE,
      is.na(Longitude) & !is.na(Longitude_field) ~ TRUE,
      TRUE ~ FALSE),
    Latitude=if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude=if_else(is.na(Longitude), Longitude_field, Longitude))%>%
  select(Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Depth, Tide, Microcystis, Chlorophyll, Secchi, Temperature, Temperature_bottom, Conductivity)

usethis::use_data(EMP, overwrite = TRUE)
