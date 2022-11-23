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
                                  SpCndBottom = "d", Secchi="d", Microcystis="c",
                                  DOSurface="d", DOBottom="d",
                                  pHSurface="d", pHBottom="d"))%>%
  left_join(SDO_stations, by="StationID")%>%
  rename(Station=StationID, Temperature=WTSurface, Temperature_bottom=WTBottom,
         Conductivity=SpCndSurface, Conductivity_bottom = SpCndBottom, DissolvedOxygen=DOSurface, DissolvedOxygen_bottom=DOBottom,
         pH=pHSurface, pH_bottom=pHBottom)%>%
  mutate(Source="SDO",
         Date=parse_date_time(Date, orders="%m/%d/%Y", tz = "Etc/GMT+8"), # SDO reports time in PST throughout the year
         Time=stringr::str_pad(Time, width=4, side="left", pad="0"),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), orders="%Y-%m-%d %H%M", tz = "Etc/GMT+8"),
         Datetime = with_tz(Datetime, tzone = "America/Los_Angeles"), # Convert from PST to local time to correspond with the other surveys
         Microcystis=recode(Microcystis, present=NA_character_, absent=NA_character_),
         Microcystis=round(as.numeric(Microcystis)))%>%
  select(Source, Station, Latitude,
         Longitude, Date, Datetime,
         Microcystis, Secchi, Temperature,
         Temperature_bottom, Conductivity, Conductivity_bottom,
         DissolvedOxygen, DissolvedOxygen_bottom,
         pH, pH_bottom)


usethis::use_data(SDO, overwrite = TRUE)
