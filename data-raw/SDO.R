## code to prepare `SDO` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(stringr)

# Temporarily removing microcystis until I can figure out how to convert it to a common scale with other surveys

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.276.2&entityid=e91e91c52a24d61002c8287ab30de3fc",
              file.path(tempdir(), "IEP_DOSDWSC_1997_2018.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.276.2&entityid=172a4b6e794eb14e8e5ccb97fef435a1",
              file.path(tempdir(), "IEP_DOSDWSC_site_locations_latitude_and_longitude.csv"), mode="wb",method="libcurl")

SDO_stations<-read_csv(file.path(tempdir(), "IEP_DOSDWSC_site_locations_latitude_and_longitude.csv"),
                       col_types=cols_only(StationID="c", Latitude="d", Longitude="d"))

SDO<-read_csv(file.path(tempdir(), "IEP_DOSDWSC_1997_2018.csv"),
              col_types=cols_only(Date="c", Time="c", StationID="c",
                                  WTSurface="d", WTBottom="d", SpCndSurface="d",
                                  Secchi="d"#, Microcystis="c"
                                  ))%>%
  left_join(SDO_stations, by="StationID")%>%
  rename(Station=StationID, Temperature=WTSurface, Temperature_bottom=WTBottom,
         Conductivity=SpCndSurface)%>%
  mutate(Source="SDO",
         Date=parse_date_time(Date, orders="%m/%d/%Y", tz="America/Los_Angeles"),
         Time=stringr::str_pad(Time, width=4, side="left", pad="0"),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), orders="%Y-%m-%d %H%M", tz="America/Los_Angeles"))%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Secchi, Temperature, Temperature_bottom, Conductivity)


usethis::use_data(SDO, overwrite = TRUE)
