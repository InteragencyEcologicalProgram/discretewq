## code to prepare `SKT` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(stringr)
require(tidyr)

SKT <- read_csv(file.path("data-raw", "SKT", "tblSample.csv"),
                col_types = cols_only(SampleDate="c", StationCode="c", SampleTimeStart="c", Secchi="d", ConductivityTop="d",
                                      WaterTemperature="d", DepthBottom="d", TideCode="i",
                                      SampleComments="c", Latitude="c", Longitude="c"))%>%
  select(Date=SampleDate, Station=StationCode, Time=SampleTimeStart, Secchi,
         Conductivity=ConductivityTop, Temperature=WaterTemperature, Depth=DepthBottom, Tide=TideCode,
         Notes=SampleComments, Latitude, Longitude)%>%
  mutate(Temperature=if_else(str_detect(Notes, "from CDEC") & !is.na(Notes), NA_real_, Temperature),
         Conductivity=if_else(str_detect(Notes, "from CDEC") & !is.na(Notes), NA_real_, Conductivity),
         Latitude=na_if(Latitude, "0"),
         Longitude=na_if(Longitude, "0"),
         Date = parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Source="SKT",
         Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Longitude=if_else(Longitude=="121-29.41.7", "121-29-41.7", Longitude))%>%
  mutate(Latitude = str_remove(Latitude, '".*'),
         Longitude = str_remove(Longitude, '".*'))%>%
  mutate(Latitude = str_remove(Latitude, "'.*"),
         Longitude = str_remove(Longitude, "'.*"))%>%
  mutate(Latitude = str_remove(Latitude, "[a-zA-Z]"),
         Longitude = str_remove(Longitude, "[a-zA-Z]"))%>%
  separate(Latitude, into=c("LatD", "LatM", "LatS"), sep="-", remove=TRUE, convert=TRUE)%>%
  separate(Longitude, into=c("LonD", "LonM", "LonS"), sep="-", remove=TRUE, convert=TRUE)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1)%>%
  mutate(Datetime = parse_date_time(paste0(Date, " ", hour(Time), ":", minute(Time)), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  select(-Time, -LatD, -LatM, -LatS, -LonD, -LonM, -LonS)%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Depth = Depth*0.3048)%>% # Convert feet to meters
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Secchi, Temperature, Conductivity, Notes)


usethis::use_data(SKT, overwrite = TRUE)
