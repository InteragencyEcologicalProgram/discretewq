## code to prepare `suisun` dataset goes here

require(readr)
require(dplyr)
require(lubridate)

#Removing salinity because data do not correspond well with conductivity
suisun<-read_csv(file.path("data-raw", "Suisun", "Suisun_Sample.csv"),
                    col_types = cols_only(SampleRowID="c", StationCode="c", SampleDate="c", SampleTime="c",
                                          QADone="l", WaterTemperature="d",
                                          Secchi="d", SpecificConductance="d", TideCode="c"))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime,
         Temperature=WaterTemperature, Conductivity=SpecificConductance, Tide=TideCode)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  mutate(Tide=recode(Tide, flood="Flood", ebb="Ebb", low="Low Slack", high="High Slack", outgoing="Ebb", incoming="Flood"),
         Source="Suisun")%>%
  left_join(read_csv(file.path("data-raw", "Suisun", "Suisun_Depth.csv"),
                     col_types=cols_only(SampleRowID="c", Depth="d"))%>%
              group_by(SampleRowID)%>%
              summarise(Depth=mean(Depth, na.rm=T), .groups="drop"),
            by="SampleRowID")%>%
  select(Source, Station, Date, Datetime, Depth, Tide, Secchi, Temperature, Conductivity)

usethis::use_data(suisun, overwrite = TRUE)
