## code to prepare `EMP` dataset goes here

require(readr)
require(dplyr)
require(tidyr)
require(lubridate)

# import data
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=98b400de8472d2a3d2e403141533a2cc",
              file.path(tempdir(), "SACSJ_delta_water_quality_1975_2020.csv"), mode="wb")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af",
              file.path(tempdir(), "EMP_Discrete_Water_Quality_Stations_1975-2020.csv"), mode="wb")

# read in station data
EMP_stations<-read_csv(file.path(tempdir(), "EMP_Discrete_Water_Quality_Stations_1975-2020.csv"),
                       col_types=cols_only(Station="c", Latitude="d", Longitude="d"))%>%
  drop_na()

# read in RL data
df_all_RLs <- read_csv(file.path('data-raw', 'EMP', 'emp_historic_RLs.csv'))
df_TA_RLs <- read_csv(file.path('data-raw', 'EMP', 'emp_ta_RLs.csv')) %>%
  pivot_wider(id_cols = c('Year','Month','Station'), names_from = Analyte, values_from = RL)

# read in EMP data -- can't have ND be NA
EMP <- read_csv(file.path(tempdir(), "SACSJ_delta_water_quality_1975_2020.csv"), na=c('NA'),
              col_types = cols_only(Station="c", Date="c", Time="c", FieldNotes='c', Chla="c",
                                    Depth="d", Secchi="d", Microcystis="d", SpCndSurface="d", SpCndBottom='d', DOSurface='d', DOBottom='d', DOpercentSurface='d', DOpercentBottom='d',
                                    WTSurface="d", WTBottom='d', pHSurface = 'd', pHBottom = 'd', NorthLat='d', WestLong='d',
                                    TotAlkalinity="c", TotAmmonia="c", DissAmmonia="c", DissBromide="c", DissCalcium="c", TotChloride="c", DissChloride="c", DissNitrateNitrite="c",
                                    DOC="c", TOC="c", DON="c", TON="c", DissOrthophos="c", TotPhos="c",
                                    DissSilica="c", TDS="c", TSS="c", VSS="c", TKN="c"))

# remove NDs and convert to numeric for cols w/o RLs
cols <- c('Chla','TotAlkalinity','TotAmmonia','DissBromide','DissCalcium','TotChloride','DissChloride','DOC','TOC','DON','TON','TotPhos','DissSilica','TDS','TSS','VSS','TKN')
EMP[cols][EMP[cols] == 'ND'] <- NA
EMP[cols] <- sapply(EMP[cols], as.numeric)

# clean data w/o RLs
EMP <- EMP %>%
  rename(Notes=FieldNotes, Chlorophyll=Chla, Conductivity=SpCndSurface, Conductivity_bottom=SpCndBottom, Temperature=WTSurface, Temperature_bottom=WTBottom, DissolvedOxygen=DOSurface, DissolvedOxygen_bottom=DOBottom,
         DissolvedOxygenPercent=DOpercentSurface, DissolvedOxygenPercent_bottom=DOpercentBottom, pH=pHSurface,
         pH_bottom = pHBottom, Latitude=NorthLat, Longitude=WestLong) %>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%m/%d/%Y %H:%M", tz="Etc/GMT+8"), # EMP only reports time in PST, which corresponds to Etc/GMT+8 see https://stackoverflow.com/questions/53076575/time-zones-etc-gmt-why-it-is-other-way-round
         Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
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

# -- RLs --
# add sign cols
EMP$DissAmmonia_Sign <- ifelse(is.na(EMP$DissAmmonia) | EMP$DissAmmonia != 'ND', '=', '<')
EMP$DissNitrateNitrite_Sign <- ifelse(is.na(EMP$DissNitrateNitrite) | EMP$DissNitrateNitrite != 'ND', '=', '<')
EMP$DissOrthophos_Sign <- ifelse(is.na(EMP$DissOrthophos) | EMP$DissOrthophos != 'ND', '=', '<')
EMP$Year <- lubridate::year(EMP$Date)
EMP$Month <- lubridate::month(EMP$Date)

# merge RL cols
EMP <- left_join(EMP, df_all_RLs, by = c('Year', 'Month'))
EMP <- left_join(EMP, df_TA_RLs, by = c('Year', 'Month','Station'))
EMP$DissAmmonia <- ifelse(EMP$DissAmmonia == 'ND', EMP$DissAmmonia_RL, EMP$DissAmmonia)
EMP$DissNitrateNitrite <- ifelse(EMP$DissNitrateNitrite == 'ND', EMP$DissNitrateNitrite_RL, EMP$DissNitrateNitrite)
EMP$DissNitrateNitrite <- ifelse(EMP$DissNitrateNitrite == 'ND', EMP$DissNitrateNitrite_TA_RL, EMP$DissNitrateNitrite)
EMP$DissOrthophos <- ifelse(EMP$DissOrthophos == 'ND', EMP$DissOrthophos_RL, EMP$DissOrthophos)
EMP$DissOrthophos <- ifelse(EMP$DissOrthophos == 'ND', EMP$DissOrthophos_TA_RL, EMP$DissDissOrthophos)

# convert cols to numeric
EMP$DissAmmonia <- as.numeric(EMP$DissAmmonia)
EMP$DissOrthophos <- as.numeric(EMP$DissOrthophos)
EMP$DissNitrateNitrite <- as.numeric(EMP$DissNitrateNitrite)

# select cols
EMP <- EMP %>% select(Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Notes, Depth, Tide, Microcystis, Chlorophyll, Secchi, Temperature, Temperature_bottom, Conductivity, Conductivity_bottom,
       DissolvedOxygen, DissolvedOxygen_bottom, DissolvedOxygenPercent, DissolvedOxygenPercent_bottom, pH, pH_bottom, TotAlkalinity, TotAmmonia, DissAmmonia_Sign, DissAmmonia, DissBromide,
       DissCalcium, TotChloride, DissChloride, DissNitrateNitrite_Sign, DissNitrateNitrite, DOC, TOC, DON, TON, DissOrthophos_Sign, DissOrthophos, TotPhos,
       DissSilica, TDS, TSS, VSS, TKN)

usethis::use_data(EMP, overwrite = TRUE)
