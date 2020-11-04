## code to prepare `DJFMP` dataset goes here

require(readr)
require(dplyr)
require(lubridate)

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=71c16ead9b8ffa4da7a52da180f601f4", file.path(tempdir(), "DJFMP_1976-2001.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=c4726f68b76c93a7e8a1e13e343ebae2", file.path(tempdir(), "DJFMP_2002-2019.csv"), mode="wb",method="libcurl")

DJFMP <- read_csv(file.path(tempdir(), "DJFMP_1976-2001.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c",
                                           SampleTime="c", WaterTemperature="d", Secchi="d"))%>%
  bind_rows(read_csv(file.path(tempdir(), "DJFMP_2002-2019.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c",
                                           SampleTime="c", WaterTemperature="d", Secchi="d")))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime, Temperature=WaterTemperature)%>%
  mutate(Secchi=Secchi*100)%>% # convert Secchi to cm
  mutate(Source="DJFMP",
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  distinct()%>%
  group_by(Date, Station, Source)%>%
  summarise(Temperature=mean(Temperature), Secchi=mean(Secchi), Datetime=min(Datetime, na.rm=T)+(max(Datetime, na.rm=T)-min(Datetime, na.rm=T))/2, .groups="drop")%>%
  select(Source, Station, Date, Datetime, Secchi, Temperature)

usethis::use_data(DJFMP, overwrite = TRUE)
