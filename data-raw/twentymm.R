## code to prepare `twentymm` dataset goes here

#Issue
# 1) Now tide data anymore in EDI dataset

require(readr)
require(dplyr)
require(lubridate)
require(tidyr)


download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.535.2&entityid=d0172df5cb8e9e7b43d017765c9a815b",
              file.path(tempdir(), "qry_EDI_20mm-catch_10122020.csv"), mode="wb")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.535.2&entityid=1c8ab3bcd3aec0ed06bf86e696cdfa6a",
              file.path(tempdir(), "20mm_Station_file.csv"), mode="wb")




twentymm_stations<-read_csv(file.path(tempdir(), "20mm_Station_file.csv"),
                            col_types=cols_only(Station="c", Lat="c", Long="c"))%>%
  separate(Lat, into=c("LatD", "LatM", "LatS"), sep=" ", convert=T)%>%
  separate(Long, into=c("LonD", "LonM", "LonS"), sep=" ", convert=T)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1)%>%
  select(Station, Latitude, Longitude)%>%
  drop_na()

twentymm <- read_csv(file.path(tempdir(), "qry_EDI_20mm-catch_10122020.csv"),
                     col_types = cols_only(SampleDate="c", TowTime="c", Station="c",
                                           Latitude="c", Longitude="c", TowNum="i", BottomDepth="d",
                                           Temp="d", TopEC="d", Secchi="d"))%>%
  separate(Latitude, into=c("LatD", "LatM", "LatS"), sep=" ", convert=T)%>%
  separate(Longitude, into=c("LonD", "LonM", "LonS"), sep=" ", convert=T)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1)%>%
  rename(Date=SampleDate)%>%
  mutate(Datetime=parse_date_time(if_else(is.na(TowTime), NA_character_, paste(Date, TowTime)), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
         Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"))%>%
  group_by(Date, Station)%>% # StationID really is sampleID
  mutate(Retain=if_else(Datetime==min(Datetime), TRUE, FALSE))%>% # Only keep bottom depth, tide, and time info for the first tow of each day (defined by time or tow number below)
  filter(Retain)%>%
  select(-Retain)%>%
  mutate(Retain=if_else(TowNum==min(TowNum), TRUE, FALSE))%>%
  ungroup()%>%
  filter(Retain)%>%
  select(Date, Datetime, Station, Depth=BottomDepth,
         Temperature=Temp, Conductivity=TopEC, Secchi,
         Latitude, Longitude)%>%
  mutate(Source = "20mm",
         Depth = Depth*0.3048)%>% # Convert feet to meters
  left_join(twentymm_stations, by="Station", suffix=c("_field", ""))%>%
  mutate(Field_coords=case_when(
    is.na(Latitude) & !is.na(Latitude_field) ~ TRUE,
    is.na(Longitude) & !is.na(Longitude_field) ~ TRUE,
    TRUE ~ FALSE),
    Latitude=if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude=if_else(is.na(Longitude), Longitude_field, Longitude))%>%
  select(Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Depth, Secchi, Temperature, Conductivity)


usethis::use_data(twentymm, overwrite = TRUE)
