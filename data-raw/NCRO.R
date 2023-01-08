## code to prepare `NCRO` dataset goes here
require(readr)
require(readxl)
require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)

#this dataset just has the chlorophyll and pheophytin in it. I need to request the rest at some point.

#read in the data files
#NCRO1 = read_csv("data-raw/NCRO/WQDataReport.SDelta_2000-2021_ChlaPheo.csv",
#                 col_types = cols_only(LongStationName="c", ShortStationName="c", SampleCode="c",
#                                       CollectionDate="c", Analyte="c", Result="c",
#                                       SampleType="c", Notes="c"))

NCRO1 = read_excel("data-raw/NCRO/WQES Central Delta and Rock Slough Sample Results 2001-2021.xlsx")

NCRO2 = read_excel("data-raw/NCRO/WQES North Delta Sample Results 2015 - 2021.xlsx")

NCRO3 = read_excel("data-raw/NCRO/WQES South Delta Sample Results 1999-2021.xlsx")

NCROX = bind_rows(NCRO1, NCRO2, NCRO3)

#Why do some values have no reporting limits?

test = filter(NCROX, is.na(`Rpt Limit`), Analyte %in% c("Total Kjeldahl Nitrogen", "Dissolved Organic Nitrogen"))

# #station coordinates
# SDelta_Station_lat_long <- read_csv("data-raw/NCRO/SDelta_Station_lat_long.csv",
#                                     col_types = cols_only(LongStationName="c", `Latitude (WGS84)`="d",
#                                                           `Longitude (WGS84)`="d")) %>%
#   rename(Station = LongStationName)
#
# ND = read_excel("data-raw/NCRO/WDL ND Period of Record Metadata.xlsx") %>%
#   select(Station, `WDL Station Code`, `CDEC Name`)
# SD = read_excel("data-raw/NCRO/WDL SD Period of Record Metadata.xlsx") %>%
#   select(Station, `WDL Station Code`)
# CDRS = read_excel("data-raw/NCRO/WDL CD and RS Period of Record Metadata.xlsx") %>%
#   select(Station, `CDEC Name`)
# stations = bind_rows(ND, SD, CDRS) %>%
#   left_join(SDelta_Station_lat_long)
#
# write.csv(stations, "data-raw/NCRO/stationLatLongs.csv")

stations = read_csv("data-raw/NCRO/stationLatLongs.csv")
analytes = read_csv("data-raw/NCRO/Analytes.csv")

#Upload secchi depth and MIcrocystis, which were in a differen data frame
NCRO_secchi = read_excel("data-raw/NCRO/All WQES Station HAB Obs and Secchi 2017-2021.xlsx")

#Use the numeric codes for Microcystis
secHABs = mutate(NCRO_secchi, Microcystis = case_when(FldObsWaterHabs %in% c("Not Visible", "Absent") ~ 1,
                                                      FldObsWaterHabs == "Low" ~ 2,
                                                      FldObsWaterHabs == "Medium" ~ 3,
                                                      FldObsWaterHabs == "High" ~ 4,
                                                      FldObsWaterHabs == "High" ~ 5),
                 #convert secchi depth to cm
                 Secchi = FldObsSecchi*100,
                 Date = date(DeploymentEnd),
                 Source = "DWR_NCRO") %>%
  left_join(stations, c("StationCode" = "Station")) %>%
  rename(Station = StationCode) %>%
  select(Source, Station, Date, Secchi, Microcystis, Latitude, Longitude) %>%
  group_by(Source, Station, Date, Latitude, Longitude) %>%
  summarize(Secchi = mean(Secchi, na.rm = T), Microcystis = mean(Microcystis, na.rm = T))



#Fix the "below reporting limit values" and attach coordinates
NCRO <- mutate(NCROX,
                sign = if_else(str_detect(Result, "<"), "<", "="),
                Result = as.numeric(case_when(
                  str_detect(Result, "<") & !is.na(Result) ~ str_remove(Result, "<"),
                  suppressWarnings(!is.na(as.numeric(Result))) ~ Result,
                  TRUE ~ NA_character_)),
                # Since NCRO records only in PST, convert to local time to correspond with the other surveys
                Datetime = with_tz(mdy_hm(`Collection Date`, tz = "Etc/GMT+8"), tzone = "America/Los_Angeles"),
                Date = date(Datetime)) %>%
  filter(`Sample Type` == "Normal Sample") %>%
  left_join(stations) %>%
  rename(StationName = `Long Station Name`) %>%
  left_join(analytes) %>%
  filter(!is.na(Result), UseYN == "Y")%>%
   group_by(Station, StationName, Latitude, Longitude, Date, Datetime, Analyte, Abbreviation) %>% #Average the one sample taken at the same date, time, and station
  summarize(Result = mean(Result, na.rm = T), sign = first(sign), .groups="drop") %>%
 pivot_wider(id_cols = c(Station, Date, Datetime, `Latitude`, `Longitude`),
              names_from = Abbreviation, values_from = c(Result, sign), values_fn = first)%>%
  mutate(Source = "DWR_NCRO") %>%
rename_with(~gsub("Result_", "", .x, fixed = TRUE)) %>%
  rename_with( ~ paste0(str_remove(.x, "^sign_"), "_Sign"), starts_with("sign_")) %>%
  rename(Conductivity = Conductivity_top) %>%
  filter(!is.na(Latitude), !is.na(Datetime)) %>%
  select(-Conductivity_top_Sign, -Temperature_Sign, -DissolvedOxygen_Sign, -DissolvedOxygen_bottom_Sign)


NCRO = left_join(NCRO, secHABs)

usethis::use_data(NCRO, overwrite = TRUE)
