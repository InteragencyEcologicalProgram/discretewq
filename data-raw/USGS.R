## code to prepare `USGS` dataset goes here

require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(purrr)
require(readxl)

USGS_stations <- read_excel(file.path("data-raw", "USGS", "USGSSFBayStations.xlsx"))%>%
  select(Station, Latitude="Latitude degree", Longitude="Longitude degree")%>%
  mutate(Station=as.character(Station))

USGSfiles <- list.files(path = file.path("data-raw", "USGS"), full.names = T, pattern="SanFranciscoBayWaterQualityData.csv")

USGS <- map_dfr(USGSfiles, ~read_csv(., col_types = cols_only(Date="c", Time="c", Station_Number="d", Depth="d",
                                                                 Calculated_Chlorophyll="d", Salinity="d", Temperature="d")))%>%
  rename(Station=Station_Number, Chlorophyll=Calculated_Chlorophyll)%>%
  mutate(Time=paste0(str_sub(Time, end=-3), ":", str_sub(Time, start=-2)))%>%
  bind_rows(read_csv(file.path("data-raw", "USGS", "1969_2015USGS_SFBAY_22APR20.csv"),
                     col_types = cols_only(Date="c", Time="c", Station="d", `Depth`="d", `Calculated Chl-a`="d",
                                           Salinity="d", `Temperature`="d"))%>%
              rename(Chlorophyll=starts_with("Calculated")))%>%
  filter(!is.na(Date))%>%
  mutate(Date=parse_date_time(Date, orders=c("%m/%d/%Y", "%m/%d/%y"), tz="America/Los_Angeles"),
         Time=hm(Time),
         Station=as.character(Station))%>%
  mutate(Datetime=parse_date_time(paste0(Date, " ", hour(Time), ":", minute(Time)), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Source="USGS")%>%
  select(-Time)%>%
  rename(Sample_depth=Depth)%>%
  group_by(Station, Date)%>%
  mutate(Depth_bin=case_when(
    Sample_depth==min(Sample_depth) & Sample_depth<2~ "surface",
    Sample_depth==max(Sample_depth) & Sample_depth>2~ "bottom",
    TRUE ~ "Middle"
  ),
  Datetime=min(Datetime)+(max(Datetime)-min(Datetime))/2)%>%
  ungroup()%>%
  filter(Depth_bin%in%c("surface", "bottom"))%>%
  pivot_wider(names_from=Depth_bin, values_from=c(Sample_depth, Salinity, Chlorophyll, Temperature),
              values_fn=list(Sample_depth=mean, Salinity=mean, Chlorophyll=mean, Temperature=mean))%>% # This will just average out multiple measurements at same depth
  left_join(USGS_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Sample_depth_surface, Sample_depth_bottom, Chlorophyll=Chlorophyll_surface,
         Temperature=Temperature_surface, Temperature_bottom, Salinity=Salinity_surface)


usethis::use_data(USGS, overwrite = TRUE)
