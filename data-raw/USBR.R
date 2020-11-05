## code to prepare `USBR` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(tidyr)
require(stringr)

USBR_stations <- read_csv(file.path("data-raw", "USBR", "USBRSiteLocations.csv"),
                 col_types=cols_only(Station="c", Lat="d", Long="d"))%>%
  rename(Latitude=Lat, Longitude=Long)%>%
  mutate(Station=str_remove(Station, "NL "),
         Station=recode(Station, PS="Pro"))

USBR <- read_csv(file.path("data-raw", "USBR", "YSILongTermSites_AllDepths.csv"),
                    col_types=cols_only(Station="c", DateTime.PT="c", Depth.feet="d",
                                        Temp.C="d", SpCond.uS="d", Chl.ug.L="d", Date="c"))%>%
  rename(Datetime=DateTime.PT, Sample_depth=Depth.feet, Temperature=Temp.C, Conductivity=SpCond.uS, Chlorophyll=Chl.ug.L)%>%
  mutate(Datetime=parse_date_time(Datetime, orders="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
         Date=parse_date_time(Date, orders="%Y-%m-%d", tz="America/Los_Angeles"))%>%
  group_by(Station, Date)%>%
  mutate(Depth_bin=case_when(
    Sample_depth==min(Sample_depth) & Sample_depth<3 ~ "surface",
    Sample_depth==max(Sample_depth) & Sample_depth>3 ~ "bottom",
    TRUE ~ "Middle"
  ),
  Datetime=min(Datetime))%>%
  ungroup()%>%
  filter(Depth_bin%in%c("surface", "bottom"))%>%
  pivot_wider(names_from=Depth_bin, values_from=c(Sample_depth, Conductivity, Chlorophyll, Temperature),
              values_fn=list(Sample_depth=mean, Conductivity=mean, Chlorophyll=mean, Temperature=mean))%>% # There are 2 duplicated rows this should take care of
  left_join(read_csv(file.path("data-raw", "USBR", "USBRSiteLocations.csv"),
                     col_types=cols_only(Station="c", `Depth (m)`="d"))%>%
              mutate(Station=str_remove(Station, "NL "))%>%
              mutate(Station=recode(Station, PS="Pro"))%>%
              rename(Depth="Depth (m)"),
            by="Station")%>%
  mutate(Source="USBR",
         Sample_depth_surface = Sample_depth_surface*0.3048,
         Sample_depth_bottom = Sample_depth_bottom*0.3048)%>% # Convert to meters
  left_join(USBR_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Sample_depth_surface, Sample_depth_bottom, Chlorophyll=Chlorophyll_surface,
         Temperature=Temperature_surface, Temperature_bottom, Conductivity=Conductivity_surface)


usethis::use_data(USBR, overwrite = TRUE)
