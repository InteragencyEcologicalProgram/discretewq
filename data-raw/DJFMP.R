## code to prepare `DJFMP` dataset goes here

require(readr)
require(dplyr)
require(lubridate)

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=71c16ead9b8ffa4da7a52da180f601f4", file.path(tempdir(), "DJFMP_1976-2001.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=c4726f68b76c93a7e8a1e13e343ebae2", file.path(tempdir(), "DJFMP_2002-2019.csv"), mode="wb",method="libcurl")

Download_stations <- FALSE

if(Download_stations){
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=99a038d691f27cd306ff93fdcbc03b77", file.path("data-raw", "DJFMP", "DJFMP_stations.csv"), mode="wb")
}

#Methods in  metadata say they do not know if their data were corrected for temperature before May 3 or 17 2019 so I will not use conductivity data before June 2019

DJFMP_stations <- read_csv(file.path("data-raw", "DJFMP", "DJFMP_stations.csv"),
                  col_types=cols_only(StationCode="c", Latitude_location="d", Longitude_location="d"))%>%
  rename(Station=StationCode, Latitude=Latitude_location, Longitude=Longitude_location)

DJFMP <- read_csv(file.path(tempdir(), "DJFMP_1976-2001.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c", SampleTime="c",
                                           Conductivity="d", WaterTemperature="d", Secchi="d"))%>%
  bind_rows(read_csv(file.path(tempdir(), "DJFMP_2002-2019.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c", SampleTime="c",
                                           Conductivity="d", WaterTemperature="d", Secchi="d")))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime, Temperature=WaterTemperature)%>%
  mutate(Secchi=Secchi*100)%>% # convert Secchi to cm
  mutate(Source="DJFMP",
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%H:%M:%S", tz="America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Conductivity=if_else(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity))%>% # Removing conductivity data from dates before it was standardized
  select(-Time)%>%
  distinct()%>%
  group_by(Date, Station, Source)%>% # Retaining just 1 sample per day due to evidence of water quality data copied for multiple tows in one day
  summarise(Temperature=mean(Temperature), Conductivity=mean(Conductivity), Secchi=mean(Secchi), Datetime=min(Datetime, na.rm=T)+(max(Datetime, na.rm=T)-min(Datetime, na.rm=T))/2, .groups="drop")%>%
  left_join(DJFMP_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Secchi, Temperature, Conductivity)

usethis::use_data(DJFMP, overwrite = TRUE)
