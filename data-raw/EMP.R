## code to prepare `EMP` dataset goes here

require(readr)
require(dplyr)
require(lubridate)

Download <- FALSE

if(Download){
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.3&entityid=abedcf730c2490fde0237df58c897556", file.path("data-raw", "EMP", "SACSJ_delta_water_quality_1975_2019.csv"), mode="wb")
}

EMP<-read_csv(file.path("data-raw", "EMP", "SACSJ_delta_water_quality_1975_2019.csv"), na=c("NA", "ND"),
              col_types = cols_only(Station="c", Date="c", Time="c", Chla="d",
                                    Depth="d", Secchi="d", Microcystis="d", SpCndSurface="d",
                                    WTSurface="d", WTBottom='d'))%>%
  rename(Chlorophyll=Chla, Conductivity=SpCndSurface, Temperature=WTSurface,
         Temperature_bottom=WTBottom)%>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%m/%d/%Y %H:%M", tz="America/Los_Angeles"),
         Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
         Microcystis=round(Microcystis))%>% #EMP has some 2.5 and 3.5 values
  select(-Time)%>%
  mutate(Datetime=if_else(hour(Datetime)==0, parse_date_time(NA_character_, tz="America/Los_Angeles"), Datetime))%>%
  mutate(Source="EMP",
         Tide = "High Slack",
         Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station),
         Depth=Depth*0.3048)%>% # Convert feet to meters
  select(Source, Station, Date, Datetime, Depth, Tide, Microcystis, Chlorophyll, Secchi, Temperature, Temperature_bottom, Conductivity)

usethis::use_data(EMP, overwrite = TRUE)
