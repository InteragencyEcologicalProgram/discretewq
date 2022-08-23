## code to prepare `NCRO` dataset goes here
require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)

#this dataset just has the chlorophyll and pheophytin in it. I need to request the rest at some point.

#read in the data file
NCRO1 = read_csv("data-raw/NCRO/WQDataReport.SDelta_2000-2021_ChlaPheo.csv")

#station coordinates
SDelta_Station_lat_long <- read_csv("data-raw/NCRO/SDelta_Station_lat_long.csv")

#Fix the "below reporting limit values" and attach coordinates
NCRO1 = mutate(NCRO1, Result2 = as.numeric(str_remove(Result, "<")), sign = case_when(str_detect(Result, "<") ~ "<",
                                                                   TRUE ~ "="),
               Datetime = mdy_hm(CollectionDate),
               Date = date(Datetime)) %>%
  filter(SampleType == "Normal Sample") %>%
  left_join(SDelta_Station_lat_long) %>%
  filter(!is.na(`Latitude (WGS84)`))

#Long to wide

NCROwide = pivot_wider(NCRO1, id_cols = c(LongStationName, ShortStationName, SampleCode, CollectionDate, Notes, Date, Datetime, `Latitude (WGS84)`, `Longitude (WGS84)`),
                       names_from = Analyte, values_from = Result2)
NCROwide2 = pivot_wider(NCRO1, id_cols = c(LongStationName, ShortStationName, SampleCode, CollectionDate, Notes, Date, Datetime),
                        names_from = Analyte, values_from = sign, names_prefix = "Sign")

NCROwide3 = left_join(NCROwide, NCROwide2)

#now rename things
NCROx = NCROwide3 %>%
rename(Chlorophyll=`Chlorophyll a`, Chlorophyll_Sign=`SignChlorophyll a`,
       Latitude=`Latitude (WGS84)`, Longitude=`Longitude (WGS84)`, Station = ShortStationName) %>%
  mutate(Field_coords= F, Source = "DWR_NCRO") %>%
  select(Source, Station,  Latitude, Longitude, Field_coords, Date, Datetime, Notes, Chlorophyll_Sign, Chlorophyll) %>%
  mutate(ID=paste(Source, Station, Date, Datetime, Latitude, Longitude))

#Average samples taken at the same date, time, and station
NCRO = group_by(NCROx, ID, Source, Station,  Latitude, Longitude, Field_coords, Date, Datetime, Notes) %>%
  summarize(Chlorophyll = mean(Chlorophyll, na.rm = T), Chlorophyll_Sign = first(Chlorophyll_Sign)) %>%
  ungroup()

usethis::use_data(NCRO, overwrite = TRUE)
