## code to prepare `EMP` dataset goes here
require(readr)
require(dplyr)
require(tidyr)
require(lubridate)

# import data from EDI to temp dir
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.6&entityid=dfeaee030be901ae00b8c0449ea39e9c",
              file.path(tempdir(), "SACSJ_delta_water_quality_1975_2021.csv"), mode="wb")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.6&entityid=ecf241d54a8335a49f8dfc8813d75609",
              file.path(tempdir(), "EMP_Discrete_Water_Quality_Stations_1975-2021.csv"), mode="wb")

# read in station data
EMP_stations <-
  read_csv(
    file.path(tempdir(), "EMP_Discrete_Water_Quality_Stations_1975-2021.csv"),
    col_types = cols_only(Station="c", Latitude="d", Longitude="d")
    ) %>%
  drop_na()

# read in EMP data
# data without sign (ie. all = to 2021): TotAlkalinity, TotAmmonia, TotChloride, DissChloride, TON, TDS
EMP <- read_csv(file.path(tempdir(), "SACSJ_delta_water_quality_1975_2021.csv"),
                col_types = cols_only(Station="c", Date="c", Time="c", FieldNotes='c', Chla_Sign="c", Chla="d", Depth="d", Secchi="d",
                          Microcystis="d", SpCndSurface="d", SpCndBottom='d', DOSurface='d', DOBottom='d',
                          DOpercentSurface='d', DOpercentBottom='d', WTSurface="d", WTBottom='d', pHSurface = 'd',
                          pHBottom = 'd', NorthLat='d', WestLong='d', Pheophytin_Sign="c", Pheophytin="d", TotAlkalinity="d",
                          TotAmmonia="d", DissAmmonia_Sign="c", DissAmmonia="d", DissBromide_Sign="c", DissBromide="d",
                          DissCalcium_Sign="c", DissCalcium="d", TotChloride="d", DissChloride="d",
                          DissNitrateNitrite_Sign="c", DissNitrateNitrite="d", DOC_Sign="c", DOC="d",
                          TOC_Sign="c", TOC="d", DON_Sign="c",DON="d", TON="d", DissOrthophos_Sign="c",
                          DissOrthophos="d", TotPhos_Sign="c", TotPhos="d", DissSilica_Sign="c", DissSilica="d",
                          TDS="d", TSS_Sign="c", TSS="d", VSS_Sign="c", VSS="d", TKN_Sign="c", TKN="d"))

# clean data
EMP <- EMP %>%
  rename(Notes=FieldNotes, Chlorophyll=Chla, Chlorophyll_Sign=Chla_Sign, Conductivity=SpCndSurface, Conductivity_bottom=SpCndBottom, Temperature=WTSurface, Temperature_bottom=WTBottom, DissolvedOxygen=DOSurface, DissolvedOxygen_bottom=DOBottom,
         DissolvedOxygenPercent=DOpercentSurface, DissolvedOxygenPercent_bottom=DOpercentBottom, pH=pHSurface,
         pH_bottom = pHBottom, Latitude=NorthLat, Longitude=WestLong) %>%
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

# average old data (collected multiple times per day)
EMP <- EMP %>%
  group_by(across(c(-VSS, -TSS, -Chlorophyll, -Secchi, -Temperature, -TotChloride, -DissNitrateNitrite, -DON,
                    -Conductivity, -DissolvedOxygen, -pH, -TotAmmonia, -TON, -DissSilica, -TotPhos, -TDS)))%>%
  summarize(TSS = mean(TSS, na.rm = TRUE),
            VSS = mean(VSS, na.rm = TRUE),
            DissolvedOxygen = mean(DissolvedOxygen, na.rm = TRUE),
            pH = mean(pH, na.rm = TRUE),
            Chlorophyll = mean(Chlorophyll, na.rm = TRUE),
            Secchi = mean(Secchi, na.rm = TRUE),
            Conductivity = mean(Conductivity, na.rm = TRUE),
            Temperature = mean(Temperature, na.rm = TRUE),
            TotChloride = mean(TotChloride, na.rm = TRUE),
            DissNitrateNitrite = mean(DissNitrateNitrite, na.rm = TRUE),
            DON = mean(DON, na.rm = TRUE),
            TotAmmonia = mean(TotAmmonia, na.rm = TRUE),
            TON = mean(TON, na.rm = TRUE),
            DissSilica = mean(DissSilica, na.rm = TRUE),
            TotPhos = mean(TotPhos, na.rm = TRUE),
            TDS = mean(TDS, na.rm = TRUE),
            .groups = 'drop')

# select cols
EMP <- EMP %>% select(Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Notes, Depth, Tide,
                      Microcystis, Chlorophyll_Sign, Chlorophyll, Secchi, Temperature, Temperature_bottom,
                      Conductivity, Conductivity_bottom, DissolvedOxygen, DissolvedOxygen_bottom,
                      DissolvedOxygenPercent, DissolvedOxygenPercent_bottom, pH, pH_bottom, TotAlkalinity,
                      TotAmmonia, DissAmmonia_Sign, DissAmmonia, DissBromide_Sign, DissBromide, DissCalcium_Sign,
                      DissCalcium, TotChloride, DissChloride, DissNitrateNitrite_Sign, DissNitrateNitrite, DOC_Sign,
                      DOC, TOC_Sign, TOC, DON_Sign, DON, TON, DissOrthophos_Sign, DissOrthophos, TotPhos_Sign,
                      TotPhos, DissSilica_Sign, DissSilica, TDS, TSS_Sign, TSS, VSS_Sign, VSS, TKN_Sign, TKN)

usethis::use_data(EMP, overwrite = TRUE)
