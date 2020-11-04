## code to prepare `EDSM` dataset goes here

require(readr)
require(dplyr)
require(lubridate)

Download <- FALSE

if(Download){
  #EDSM 20mm
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.3&entityid=d468c513fa69c4fc6ddc02e443785f28", file.path("data-raw", "EDSM", "EDSM_20mm.csv"), mode="wb")
  #EDSM KDTR
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.3&entityid=4d7de6f0a38eff744a009a92083d37ae", file.path("data-raw", "EDSM", "EDSM_KDTR.csv"), mode="wb")
}

#Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
EDSM <- read_csv(file.path("data-raw", "EDSM", "EDSM_20mm.csv"),
                    col_types=cols_only(Date="c", StartLat="d", StartLong="d",
                                        TopTemp="d", BottomTemp="d", Scchi="d", Time="c",
                                        Tide="c", Depth="d", SampleComments="c"))%>%
  rename(Latitude=StartLat, Longitude=StartLong, Temperature=TopTemp, Secchi=Scchi, Notes = SampleComments, Temperature_bottom=BottomTemp)%>%
  bind_rows(read_csv(file.path("data-raw", "EDSM", "EDSM_KDTR.csv"),
                     col_types=cols_only(Date="c", StartLat="d", StartLong="d",
                                         Temp="d", Scchi="d", Time="c",
                                         Tide="c", StartDepth="d", Comments="c"))%>%
              rename(Latitude=StartLat, Longitude=StartLong, Temperature=Temp, Secchi=Scchi, Depth=StartDepth, Notes=Comments))%>%
  mutate(Secchi=Secchi*100, # convert Secchi to cm
         Tide=recode(Tide, HS="High Slack", LS = "Low Slack"),
         Tide=na_if(Tide, "n/p"))%>% #Standardize tide codes
  mutate(Station=paste(Latitude, Longitude),
         Source="EDSM",
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  distinct()%>%
  mutate(Depth = Depth*0.3048)%>% # Convert feet to meters
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Secchi, Temperature, Temperature_bottom, Notes)


usethis::use_data(EDSM, overwrite = TRUE)
