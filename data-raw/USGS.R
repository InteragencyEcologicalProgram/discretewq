## code to prepare `USGS` dataset goes here

require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(purrr)
require(readxl)
require(stringr)

#Latest USGS data from: https://sfbay.wr.usgs.gov/water-quality-database/

USGS_stations <- read_excel(file.path("data-raw", "USGS", "USGSSFBayStations.xlsx"))%>%
  select(Station, Latitude="Latitude degree", Longitude="Longitude degree")%>%
  mutate(Station=as.character(Station))

USGSfiles_pub <- list.files(path = file.path("data-raw", "USGS"), full.names = T, pattern="SanFranciscoBayWaterQualityData.csv")
USGSfiles_web<-list.files(path = file.path("data-raw", "USGS"), full.names = T, pattern="wqdata")

USGS <- map_dfr(USGSfiles_pub, ~read_csv(., col_types = cols_only(Date="c", Time="c", Station_Number="d", Depth="d",
                                                              Calculated_Chlorophyll="d", Salinity="d", Temperature="d",
                                                             `Nitrate_+_Nitrite` = 'd', Ammonium = 'd', Phosphate = 'd', Silicate = 'd')))%>%
  rename(Station=Station_Number, Chlorophyll=Calculated_Chlorophyll, DissNitrateNitrite=`Nitrate_+_Nitrite`, DissAmmonia = Ammonium, DissOrthophos = Phosphate,
         DissSilica = Silicate) %>%
  mutate(Time=paste0(str_sub(Time, end=-3), ":", str_sub(Time, start=-2)))%>%

  bind_rows(read_csv(file.path("data-raw", "USGS", "1969_2015USGS_SFBAY_22APR20.csv"),
                     col_types = cols_only(Date="c", Time="c", Station="d", `Depth`="d", `Calculated Chl-a`="d",
                                           Salinity="d", `Temperature`="d", NO32='d', NH4='d', PO4='d', Si='d'))%>%
              rename(Chlorophyll=starts_with("Calculated"), DissNitrateNitrite = NO32, DissAmmonia = NH4, DissOrthophos = PO4, DissSilica = Si)) %>%

  bind_rows(map_dfr(USGSfiles_web, ~read_csv(., col_types = cols_only(Date="c", Time="c", Station="d", `Depth (m)`="d",
                                                                      `Calculated Chlorophyll-a (micrograms/L)`="d",
                                                                      Salinity="d", `Temperature (Degrees Celsius)`="d", `NO32 (Micromolar)` = 'd',
                                                                      `NH4 (Micromolar)` = 'd', `PO4 (Micromolar)` = 'd', `Si (Micromolar)` = 'd')))%>%
  rename(Depth=`Depth (m)`, Chlorophyll=`Calculated Chlorophyll-a (micrograms/L)`, Temperature=`Temperature (Degrees Celsius)`, DissNitrateNitrite=`NO32 (Micromolar)`,
         DissAmmonia = `NH4 (Micromolar)`, DissOrthophos = `PO4 (Micromolar)`, DissSilica = `Si (Micromolar)`))%>%
  filter(!is.na(Date))%>%
  mutate(Date=parse_date_time(Date, orders=c("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d"), tz="America/Los_Angeles"),
         Time=hm(Time),
         Station=as.character(Station))%>%
  mutate(Datetime=parse_date_time(paste0(Date, " ", hour(Time), ":", minute(Time)), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Source="USGS")%>%
  select(-Time)%>%
  rename(Sample_depth=Depth)%>%
  group_by(Station, Date)%>%
  mutate(
    Depth_bin=case_when(
      Sample_depth==min(Sample_depth) & Sample_depth<2~ "surface",
      Sample_depth==max(Sample_depth) & Sample_depth>2~ "bottom",
      TRUE ~ "Middle"),
    Datetime=min(Datetime)+(max(Datetime)-min(Datetime))/2)%>% # Keep average sample time across all depths
  ungroup()%>%
  filter(Depth_bin%in%c("surface", "bottom"))%>%
  pivot_wider(names_from=Depth_bin, values_from=c(Sample_depth, Salinity, Chlorophyll, Temperature),
              values_fn=list(Sample_depth=mean, Salinity=mean, Chlorophyll=mean, Temperature=mean))%>% # This will just average out multiple measurements at same depth
  left_join(USGS_stations, by="Station")%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Sample_depth_surface, Sample_depth_bottom, Chlorophyll=Chlorophyll_surface,
         Temperature=Temperature_surface, Temperature_bottom, Salinity=Salinity_surface, DissNitrateNitrite, DissAmmonia, DissOrthophos, DissSilica)


# convert units
USGS$DissNitrateNitrite <- sapply(USGS$DissNitrateNitrite, function(x) x*(14/(10^3))) # molar mass of N
USGS$DissAmmonia <- sapply(USGS$DissAmmonia, function(x) x*(14/(10^3))) # molar mass of N
USGS$DissOrthophos <- sapply(USGS$DissOrthophos, function(x) x*(30.974/(10^3))) # molar mass of P
USGS$DissSilica <- sapply(USGS$DissSilica, function(x) x*(60.08/(10^3))) # molar mass of SiO2

usethis::use_data(USGS, overwrite = TRUE)

# COMMENT (Sarah): didn't average measurements for nutrients
# COMMENT (Sarah): didn't include NO2 or DissOxygen b/c need to confirm some things about the data first
