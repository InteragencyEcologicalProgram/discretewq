## code to prepare `suisun` dataset goes here

# Issues
# 1) Some conductivity, Secchi, Temperature, Salinity have 0 values. Maybe these should be NA?

require(readr)
require(dplyr)
require(lubridate)

Suisun_stations<-read_csv(file.path("data-raw", "Suisun", "StationsLookUp.csv"),
                          col_types=cols_only(StationCode="c", x_WGS84="d", y_WGS84="d"))%>%
  rename(Longitude=x_WGS84, Latitude=y_WGS84, Station=StationCode)%>%
  drop_na()

#Removing salinity because data do not correspond well with conductivity
suisun<-read_csv(file.path("data-raw", "Suisun", "Sample.csv"),
                 col_types = cols_only(SampleRowID="c", StationCode="c", SampleDate="c", SampleTime="c",
                                       QADone="l", WaterTemperature="d", Secchi="d",
                                       SpecificConductance="d", ElecCond="d", TideCode="c"))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime,
         Temperature=WaterTemperature, Tide=TideCode)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Conductivity=if_else(is.na(SpecificConductance), ElecCond / (1 + 0.019 * (Temperature - 25)), SpecificConductance),
         # specific conductivity calculated from electrical conductivity Using formula from https://pubs.usgs.gov/tm/09/a6.3/tm9-a6_3.pdf
         # and alpha constant from https://www.mt.com/dam/MT-NA/pHCareCenter/Conductivity_Linear_Temp_Comensation_APN.pdf
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Tide=recode(Tide, flood="Flood", ebb="Ebb", low="Low Slack", high="High Slack", outgoing="Ebb", incoming="Flood"),
         Source="Suisun",
         across(c(Secchi, Temperature, Conductivity), ~if_else(Conductivity==0 & Temperature==0 & Secchi==0, NA_real_, .x)), # 8 rows where all 3 are 0, set to NAs
         Temperature=na_if(Temperature, 0))%>% # 1 row where just Temp is 0, set to NA
  select(-Time, -SpecificConductance, -ElecCond)%>%
  left_join(read_csv(file.path("data-raw", "Suisun", "Depth.csv"),
                     col_types=cols_only(SampleRowID="c", Depth="d"))%>%
              group_by(SampleRowID)%>%
              summarise(Depth=mean(Depth, na.rm=T), .groups="drop"), # Use the average depth for each sample
            by="SampleRowID")%>%
  left_join(Suisun_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Secchi, Temperature, Conductivity)%>%
  distinct(Source, Station, Date, Datetime, .keep_all=T)

usethis::use_data(suisun, overwrite = TRUE)
