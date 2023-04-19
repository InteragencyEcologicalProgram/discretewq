## code to prepare `EMP` dataset goes here
require(readr)
require(dplyr)
require(tidyr)
require(lubridate)

# import data from EDI to temp dir
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.8&entityid=cf231071093ac2861893793517db26f3",
              file.path(tempdir(), "EMP_DWQ_1975_2022.csv"), mode="wb")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.8&entityid=86dd696bc3f8407ff52954094e1e9dcf",
              file.path(tempdir(), "EMP_DWQ_Stations_1975-2022"), mode="wb")

# read in station data
EMP_stations <-
  read_csv(
    file.path(tempdir(), "EMP_DWQ_Stations_1975-2022"),
    col_types = cols_only(Station="c", Latitude="d", Longitude="d")
  ) %>%
  drop_na()

# read in EMP data (two turbidity units, will change for 2023 to all FNU)
EMP <- read_csv(file.path(tempdir(), "EMP_DWQ_1975_2022.csv"),
                col_types = cols_only(
                  Station="c", Date="c", Time="c", FieldNotes='c',
                  Chla_Sign="c", Chla="d", Depth="d", Secchi="d", Microcystis="d",
                  SpCndSurface="d", SpCndBottom='d', DOSurface='d', DOBottom='d',
                  DOpercentSurface='d', DOpercentBottom='d', WTSurface="d", WTBottom='d',
                  pHSurface = 'd', pHBottom = 'd',
                  TurbiditySurface_NTU = 'd', TurbidityBottom_NTU = 'd', TurbiditySurface_FNU = 'd', TurbidityBottom_FNU = 'd',
                  NorthLat='d', WestLong='d',
                  Pheophytin_Sign="c", Pheophytin="d", TotAlkalinity_Sign="c", TotAlkalinity="d",
                  TotAmmonia_Sign="c", TotAmmonia="d", DissAmmonia_Sign="c", DissAmmonia="d",
                  DissBromide_Sign="c", DissBromide="d", DissCalcium_Sign="c", DissCalcium="d",
                  TotChloride_Sign="c", TotChloride="d", DissChloride_Sign="c",DissChloride="d",
                  DissNitrateNitrite_Sign="c", DissNitrateNitrite="d", DOC_Sign="c", DOC="d",
                  TOC_Sign="c", TOC="d", DON_Sign="c", DON="d",
                  TON_Sign="c", TON="d", DissOrthophos_Sign="c", DissOrthophos="d",
                  TotPhos_Sign="c", TotPhos="d", DissSilica_Sign="c", DissSilica="d",
                  TDS_Sign="c", TDS="d", TSS_Sign="c", TSS="d",
                  VSS_Sign="c", VSS="d", TKN_Sign="c", TKN="d"
                )
)

# clean data
EMP <- EMP %>%
  rename(
    Notes=FieldNotes, Chlorophyll=Chla, Chlorophyll_Sign=Chla_Sign,
    Conductivity=SpCndSurface, Conductivity_bottom=SpCndBottom,
    Temperature=WTSurface, Temperature_bottom=WTBottom,
    DissolvedOxygen=DOSurface, DissolvedOxygen_bottom=DOBottom,
    DissolvedOxygenPercent=DOpercentSurface, DissolvedOxygenPercent_bottom=DOpercentBottom,
    TurbidityFNU = TurbiditySurface_FNU, TurbidityFNU_bottom = TurbidityBottom_FNU,
    TurbidityNTU = TurbiditySurface_NTU, TurbidityNTU_bottom = TurbidityBottom_NTU,
    pH=pHSurface, pH_bottom = pHBottom,
    Latitude=NorthLat, Longitude=WestLong
    ) %>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%Y-%m-%d %H:%M:%S", tz="Etc/GMT+8"), # EMP only reports time in PST, which corresponds to Etc/GMT+8 see https://stackoverflow.com/questions/53076575/time-zones-etc-gmt-why-it-is-other-way-round
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Microcystis=round(Microcystis)) %>% #EMP has some 2.5 and 3.5 values
  select(-Time) %>%
  mutate(Datetime=if_else(hour(Datetime)==0, parse_date_time(NA_character_, tz="Etc/GMT+8"), Datetime) )%>%
  mutate(Datetime=with_tz(Datetime, tz="America/Los_Angeles")) %>% # Since EMP records only in PST, convert to local time to correspond with the other surveys
  mutate(Source="EMP",
         Tide = "High Slack",
         Depth=Depth*0.3048) %>% # Convert feet to meters
  left_join(EMP_stations, by="Station", suffix=c("_field", '')) %>%
  mutate(Field_coords=case_when(
    is.na(Latitude) & !is.na(Latitude_field) ~ TRUE,
    is.na(Longitude) & !is.na(Longitude_field) ~ TRUE,
    TRUE ~ FALSE),
    Latitude=if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude=if_else(is.na(Longitude), Longitude_field, Longitude))

# select cols
EMP <- EMP %>% select(
  Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Notes, Depth, Tide,
  Microcystis, Chlorophyll_Sign, Chlorophyll, Secchi,
  Temperature, Temperature_bottom, Conductivity, Conductivity_bottom,
  DissolvedOxygen, DissolvedOxygen_bottom, DissolvedOxygenPercent, DissolvedOxygenPercent_bottom,
  pH, pH_bottom, TurbidityNTU, TurbidityNTU_bottom, TurbidityFNU, TurbidityFNU_bottom,
  Pheophytin_Sign, Pheophytin,
  TotAlkalinity_Sign, TotAlkalinity, TotAmmonia_Sign, TotAmmonia,
  DissAmmonia_Sign, DissAmmonia, DissBromide_Sign, DissBromide,
  DissCalcium_Sign, DissCalcium, TotChloride_Sign, TotChloride,
  DissChloride_Sign, DissChloride, DissNitrateNitrite_Sign, DissNitrateNitrite,
  DOC_Sign, DOC, TOC_Sign, TOC,
  DON_Sign, DON, TON_Sign, TON,
  DissOrthophos_Sign, DissOrthophos, TotPhos_Sign, TotPhos,
  DissSilica_Sign, DissSilica, TDS_Sign, TDS,
  TSS_Sign, TSS, VSS_Sign, VSS, TKN_Sign, TKN)

usethis::use_data(EMP, overwrite = TRUE)
