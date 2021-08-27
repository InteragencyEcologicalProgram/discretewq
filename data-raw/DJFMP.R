## code to prepare `DJFMP` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(hms)

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=71c16ead9b8ffa4da7a52da180f601f4",
              file.path(tempdir(), "1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=0edf413c39ac8b111a576d894306a60f",
              file.path(tempdir(), "2002-2020_DJFMP_trawl_fish_and_water_quality_data.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=99a038d691f27cd306ff93fdcbc03b77",
              file.path(tempdir(), "DJFMP_Site_Locations.csv"), mode="wb")


#Methods in  metadata say they do not know if their data were corrected for temperature before May 3 or 17 2019 so I will not use conductivity data before June 2019

DJFMP_stations <- read_csv(file.path(tempdir(), "DJFMP_Site_Locations.csv"),
                  col_types=cols_only(StationCode="c", Latitude_location="d", Longitude_location="d"))%>%
  rename(Station=StationCode, Latitude=Latitude_location, Longitude=Longitude_location)

DJFMP <- read_csv(file.path(tempdir(), "1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c", SampleTime="c",
                                           Conductivity="d", WaterTemperature="d", Secchi="d"))%>%
  bind_rows(read_csv(file.path(tempdir(), "2002-2020_DJFMP_trawl_fish_and_water_quality_data.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c", SampleTime="c",
                                           Conductivity="d", WaterTemperature="d", Secchi="d")))%>%
  rename(Station=StationCode, Date=SampleDate, Temperature=WaterTemperature)%>%
  mutate(Secchi=Secchi*100)%>% # convert Secchi to cm
  mutate(Source="DJFMP",
         Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(SampleTime), NA_character_, paste(Date, SampleTime)), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
         Conductivity=if_else(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity))%>% # Removing conductivity data from dates before it was standardized
  select(-SampleTime)%>%
  distinct(Station, Datetime, .keep_all = TRUE)%>%
  left_join(DJFMP_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Secchi, Temperature, Conductivity)

usethis::use_data(DJFMP, overwrite = TRUE)
