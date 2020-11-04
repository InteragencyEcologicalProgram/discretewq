## code to prepare `stations` dataset goes here

## code to prepare `stations` dataset goes here
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(readxl)
require(stringr)
require(lubridate)

Download <- FALSE

if(Download){
  #DJFMP
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=99a038d691f27cd306ff93fdcbc03b77", file.path("data-raw", "DJFMP", "DJFMP_stations.csv"), mode="wb")
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.3&entityid=5b439de4be6d3a5964867b0cb4a1f059", file.path("data-raw", "EMP", "EMP_Discrete_Water_Quality_Stations_1975-2019.csv"), mode="wb")
  }

FMWT<-read_csv(file.path("data-raw", "FMWT", "StationsLookUp.csv"),
               col_types = cols_only(StationCode="c", DD_Latitude="d", DD_Longitude="d"))%>%
  rename(Station=StationCode, Latitude=DD_Latitude, Longitude=DD_Longitude)%>%
  mutate(Source="FMWT",
         StationID=paste(Source, Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

STN<-read_csv(file.path("data-raw", "STN", "luStation.csv"),
              col_types = cols_only(StationCodeSTN="c", LatD='d', LatM="d", LatS="d",
                                    LonD='d', LonM='d', LonS='d'))%>%
  rename(Station=StationCodeSTN)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1,
         Source="STN",
         StationID=paste(Source, Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

SKT<-read_csv(file.path("data-raw", "SKT", "lktblStationsSKT.csv"),
              col_types=cols_only(Station="c", LatDeg="d", LatMin="d", LatSec="d",
                                  LongDec="d", LongMin="d", LongSec="d"))%>%
  mutate(Latitude=LatDeg+LatMin/60+LatSec/3600,
         Longitude=(LongDec+LongMin/60+LongSec/3600)*-1,
         Source="SKT",
         StationID=paste(Source, Station),
         Station=as.character(Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

twentymm<-read_csv(file.path("data-raw", "20mm", "20mmStations.csv"),
                   col_types=cols_only(Station="c", LatD="d", LatM="d", LatS="d",
                                       LonD="d", LonM="d", LonS="d"))%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1,
         Source="20mm",
         StationID=paste(Source, Station),
         Station=as.character(Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

EMP<-read_csv(file.path("data-raw", "EMP", "EMP_Discrete_Water_Quality_Stations_1975-2019.csv"),
             col_types=cols_only(Station="c", Latitude="d", Longitude="d"))%>%
  mutate(Source="EMP",
         StationID=paste(Source, Station))%>%
  drop_na()

#EMP_EZ stations

EMP_EZ<-read_csv(file.path("data-raw", "EMP", "SACSJ_delta_water_quality_1975_2019.csv"), na=c("NA", "ND"),
             col_types = cols_only(Station="c", Date="c", NorthLat="d", WestLong="d"))%>%
  rename(Latitude=NorthLat, Longitude=WestLong)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
         Station=paste(Station, Date),
         Source="EMP")%>%
  select(Station, Latitude, Longitude, Source)%>%
  mutate(StationID=paste(Source, Station))%>%
  drop_na()

#Suisun Study

Suisun<-read_csv(file.path("data-raw", "Suisun", "Suisun_StationsLookUp.csv"),
                 col_types=cols_only(StationCode="c", x_WGS84="d", y_WGS84="d"))%>%
  rename(Longitude=x_WGS84, Latitude=y_WGS84, Station=StationCode)%>%
  mutate(Source="Suisun",
         StationID=paste(Source, Station))

#DJFMP

DJFMP <- read_csv(file.path("data-raw", "DJFMP", "DJFMP_stations.csv"),
                  col_types=cols_only(StationCode="c", Latitude_location="d", Longitude_location="d"))%>%
  rename(Station=StationCode, Latitude=Latitude_location, Longitude=Longitude_location)%>%
  mutate(Source="DJFMP",
         StationID=paste(Source, Station))

#Baystudy

Baystudy <- read_excel(file.path("data-raw", "Baystudy", "Bay Study_Station Coordinates for Distribution_04May2020.xlsx"))%>%
  separate(Latitude, into=c("Lat_Deg", "Lat_Min"), sep = "°", convert=T)%>%
  separate(Longitude, into=c("Lon_Deg", "Lon_Min"), sep = "°", convert=T)%>%
  mutate(Latitude=Lat_Deg+Lat_Min/60,
         Longitude=Lon_Deg-Lon_Min/60)%>%
  select(Station, Latitude, Longitude)%>%
  filter(Station!="211E")%>%
  mutate(Station=recode(Station, `211W`="211"),
         Source="Baystudy",
         StationID=paste(Source, Station))

# USBR

USBR <- read_csv(file.path("data-raw", "USBR", "USBRSiteLocations.csv"),
                 col_types=cols_only(Station="c", Lat="d", Long="d"))%>%
  rename(Latitude=Lat, Longitude=Long)%>%
  mutate(Station=str_remove(Station, "NL "),
         Station=recode(Station, PS="Pro"),
         Source="USBR",
         StationID=paste(Source, Station))

# USGS

USGS <- read_excel(file.path("data-raw", "USGS", "USGSSFBayStations.xlsx"))%>%
  select(Station, Latitude="Latitude degree", Longitude="Longitude degree")%>%
  mutate(Source="USGS",
         Station=as.character(Station),
         StationID=paste(Source, Station))%>%
  select(Source, Station, StationID, Latitude, Longitude)

stations<-bind_rows(
  FMWT,
  STN,
  EMP,
  twentymm,
  EMP_EZ,
  SKT,
  Suisun,
  DJFMP,
  Baystudy,
  USBR,
  USGS)%>%
  drop_na()

usethis::use_data(stations, overwrite = TRUE)
