## code to prepare `EDSM` dataset goes here

require(readr)
require(dplyr)
require(lubridate)


#EDSM 20mm
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.8&entityid=d468c513fa69c4fc6ddc02e443785f28",
              file.path(tempdir(), "EDSM_20mm.csv"), mode="wb")
#EDSM KDTR
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.8&entityid=4d7de6f0a38eff744a009a92083d37ae",
              file.path(tempdir(), "EDSM_KDTR.csv"), mode="wb")


#Methods in  metadata say they do not know if their data were corrected for temperature before May 3 or 17 2019 so I will not use conductivity data before June 2019
EDSM <- read_csv(file.path(tempdir(), "EDSM_20mm.csv"),
                 col_types=cols_only(SampleDate="c", LatitudeStart="d", LongitudeStart="d",
                                     StationCode="c", SpecificConductanceTop="d",
                                     WaterTempTop="d", WaterTempBottom="d", Secchi="d", SampleTime="c",
                                     Tide="c", BottomDepth="d", Comments="c",
                                     DOTop="d", DOBottom="d"))%>%
  rename(Conductivity = SpecificConductanceTop, Temperature=WaterTempTop,
         Notes = Comments, Temperature_bottom=WaterTempBottom,
         DissolvedOxygen=DOTop, DissolvedOxygen_bottom=DOBottom)%>%
  mutate(Gear="20mm")%>%
  bind_rows(read_csv(file.path(tempdir(), "EDSM_KDTR.csv"),
                     col_types=cols_only(SampleDate="c", LatitudeStart="d", LongitudeStart="d",
                                         StationCode="c", SpecificConductance="d",
                                         WaterTemp="d", Secchi="d", SampleTime="c",
                                         Tide="c", BottomDepth="d", Comments="c",
                                         DO="d"))%>%
              rename(Conductivity = SpecificConductance, Temperature=WaterTemp, Notes=Comments,
                     DissolvedOxygen=DO)%>%
              mutate(Gear="KDTR"))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime,
         Latitude=LatitudeStart, Longitude=LongitudeStart, Depth=BottomDepth)%>%
  mutate(Secchi=Secchi*100, # convert Secchi to cm
         Tide=recode(Tide, HS="High Slack", LS = "Low Slack"), #Standardize tide codes
         Station=paste(Station, Date),
         Source="EDSM",
         Field_coords=TRUE,
         Date=parse_date_time(Date, c("%Y-%m-%d", "%m/%d/%Y"), tz="America/Los_Angeles"),
         Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
         Conductivity=if_else(Date<parse_date_time("2019-06-01", "%Y-%m-%d", tz="America/Los_Angeles"), NA_real_, Conductivity), # Removing conductivity data from dates before it was standardized
         Depth = if_else(Gear=="20mm", Depth*0.3048, Depth))%>%  # Convert feet to meters for 20mm (KDTR already in meters)
  select(-Time)%>%
  distinct(Source, Station, Latitude, Longitude, Date, Datetime, .keep_all=T)%>% #Remove replicate samples with the same datetime and location. This keeps the first row. This results in no more NA values in water quality variables than if we had used summarise(mean(.x, na.rm=T))
  select(Source, Station, Latitude,
         Longitude, Field_coords, Date,
         Datetime, Depth, Tide, Secchi,
         Temperature, Temperature_bottom, Conductivity,
         DissolvedOxygen, DissolvedOxygen_bottom, Notes)


usethis::use_data(EDSM, overwrite = TRUE)
