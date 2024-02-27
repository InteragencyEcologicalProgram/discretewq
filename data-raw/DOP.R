#Code to prepare DOP data goes here
library(lubridate)
library(tidyverse)

#read in dataset
DOPx = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.4&entityid=0dbed7163901c12df414da0762d28a86",
                col_types=cols_only(Date="c", Latitude="d", Longitude="d",
                                    Station_Code="c", Conductivity="d",
                                    Temperature="d",  Secchi="d", Start_Time="c",
                                      NO3 = "d", NH4 = "d",pH = "d",
                               Chl_a = "d", PO4 = "d", Start_Depth = "d",
                               Salinity = "d", Turbidity = "d", DOC = "d",
                                    DO="d"))

DOP= DOPx %>%
  #standardize colom names
  rename(Depth = Start_Depth,
         Station = Station_Code,
         Chlorophyll = Chl_a,
         DissolvedOxygen = DO,
         DissNitrateNitrite = NO3,
         DissAmmonia = NH4,
         DissOrthophos = PO4) %>%
  #make a date-time column, convert feet to meters
  mutate(        Date=parse_date_time(Date, c("%Y-%m-%d", "%m/%d/%Y"), tz="America/Los_Angeles"),
                 Datetime = parse_date_time(if_else(is.na(Start_Time), NA_character_, paste(Date, Start_Time)), "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),

         Field_coords = TRUE, Source = "DOP", Depth = Depth*0.3048) %>%

  #select relevent columns
  #note - i'm not including microcystis because they use a different method
  select(Source, Station, Latitude, Longitude,
         Field_coords, Date, Datetime,
          Depth,  Salinity,
         Temperature, Turbidity, Conductivity,
         pH, Chlorophyll, DissolvedOxygen, Secchi,
        DOC,  DissNitrateNitrite, DissAmmonia, DissOrthophos)


usethis::use_data(DOP, overwrite = TRUE)

