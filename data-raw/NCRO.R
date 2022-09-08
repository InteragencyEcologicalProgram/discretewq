## code to prepare `NCRO` dataset goes here
require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)

#this dataset just has the chlorophyll and pheophytin in it. I need to request the rest at some point.

#read in the data file
NCRO1 = read_csv("data-raw/NCRO/WQDataReport.SDelta_2000-2021_ChlaPheo.csv",
                 col_types = cols_only(LongStationName="c", ShortStationName="c", SampleCode="c",
                                       CollectionDate="c", Analyte="c", Result="c",
                                       SampleType="c", Notes="c"))

#station coordinates
SDelta_Station_lat_long <- read_csv("data-raw/NCRO/SDelta_Station_lat_long.csv",
                                    col_types = cols_only(LongStationName="c", `Latitude (WGS84)`="d",
                                                          `Longitude (WGS84)`="d"))

#Fix the "below reporting limit values" and attach coordinates
NCRO <- mutate(NCRO1,
                sign = if_else(str_detect(Result, "<"), "<", "="),
                Result = as.numeric(case_when(
                  str_detect(Result, "<") & !is.na(Result) ~ str_remove(Result, "<"),
                  suppressWarnings(!is.na(as.numeric(Result))) ~ Result,
                  TRUE ~ NA_character_)),
                # Since NCRO records only in PST, convert to local time to correspond with the other surveys
                Datetime = with_tz(mdy_hm(CollectionDate, tz = "Etc/GMT+8"), tzone = "America/Los_Angeles"),
                Date = date(Datetime)) %>%
  filter(SampleType == "Normal Sample") %>%
  left_join(SDelta_Station_lat_long, by="LongStationName") %>%
  filter(!is.na(Result))%>%
  pivot_wider(id_cols = c(LongStationName, ShortStationName, SampleCode, CollectionDate, Notes, Date, Datetime, `Latitude (WGS84)`, `Longitude (WGS84)`),
              names_from = Analyte, values_from = c(Result, sign))%>%
  rename(Chlorophyll=`Result_Chlorophyll a`, Chlorophyll_Sign=`sign_Chlorophyll a`,
         Pheophytin=`Result_Pheophytin a`, Pheophytin_Sign=`sign_Pheophytin a`,
         Latitude=`Latitude (WGS84)`, Longitude=`Longitude (WGS84)`, Station = ShortStationName) %>%
  mutate(Source = "DWR_NCRO") %>%
  select(Source, Station,  Latitude, Longitude, Date, Datetime, Notes, Chlorophyll_Sign, Chlorophyll) %>%
  group_by(Source, Station, Latitude, Longitude, Date, Datetime, Notes) %>% #Average the one sample taken at the same date, time, and station
  summarize(Chlorophyll_Sign = first(Chlorophyll_Sign), Chlorophyll = mean(Chlorophyll, na.rm = T), .groups="drop")

usethis::use_data(NCRO, overwrite = TRUE)
