## code to prepare `SDO` dataset goes here

require(readr)
require(dplyr)
require(lubridate)
require(stringr)
require(tidyr)
require(readxl)

# Temporarily removing microcystis until I can figure out how to convert it to a common scale with other surveys

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.276.2&entityid=e91e91c52a24d61002c8287ab30de3fc",
              file.path(tempdir(), "IEP_DOSDWSC_1997_2018.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.276.2&entityid=172a4b6e794eb14e8e5ccb97fef435a1",
              file.path(tempdir(), "IEP_DOSDWSC_site_locations_latitude_and_longitude.csv"), mode="wb",method="libcurl")

SDO_stations<-read_csv(file.path(tempdir(), "IEP_DOSDWSC_site_locations_latitude_and_longitude.csv"),
                       col_types=cols_only(StationID="c", Latitude="d", Longitude="d"))

# Prepare data from EDI publication collected from 1997-2018
SDO_1997_2018 <- read_csv(file.path(tempdir(), "IEP_DOSDWSC_1997_2018.csv"),
              col_types=cols_only(Date="c", Time="c", StationID="c",
                                  WTSurface="d", WTBottom="d", SpCndSurface="d",
                                  SpCndBottom = "d", Secchi="d", Microcystis="c",
                                  DOSurface="d", DOBottom="d",
                                  pHSurface="d", pHBottom="d"))%>%
  left_join(SDO_stations, by="StationID")%>%
  rename(Station=StationID, Temperature=WTSurface, Temperature_bottom=WTBottom,
         Conductivity=SpCndSurface, Conductivity_bottom = SpCndBottom, DissolvedOxygen=DOSurface, DissolvedOxygen_bottom=DOBottom,
         pH=pHSurface, pH_bottom=pHBottom)%>%
  mutate(Source="SDO",
         # Fix one erroneous time recorded as "111" which should probably be "1111"
         Time = if_else(Time == "111", "1111", Time),
         Time=stringr::str_pad(Time, width=4, side="left", pad="0"),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), orders="%m/%d/%Y %H%M", tz = "Etc/GMT+8"), # SDO reports time in PST throughout the year
         Date=parse_date_time(Date, orders="%m/%d/%Y", tz = "America/Los_Angeles"),
         Datetime = with_tz(Datetime, tzone = "America/Los_Angeles"), # Convert from PST to local time to correspond with the other surveys
         Microcystis=recode(Microcystis, present=NA_character_, absent=NA_character_),
         Microcystis=round(as.numeric(Microcystis)))%>%
  select(Source, Station, Latitude,
         Longitude, Date, Datetime,
         Microcystis, Secchi, Temperature,
         Temperature_bottom, Conductivity, Conductivity_bottom,
         DissolvedOxygen, DissolvedOxygen_bottom,
         pH, pH_bottom) %>%
  # Remove one record with NA values for all variables
  filter(!if_all(where(is.numeric) & -c("Latitude", "Longitude"), is.na))

# Prepare data collected in 2021 by DWR-EMP (data provided by Julianna Manning on 8/18/2022)
SDO_2021 <- read_excel(file.path("data-raw", "SDO", "DiscreteDORunData2021.xlsx")) %>%
  transmute(
    Source = "SDO",
    Station = recode(
      Description,
      "Turning Basin" = "tb",
      "P8" = "lt40", # P8 is the same as lt40 according to Sept 2021 DO report
      "Light 41" = "lt41",
      "Light 43" = "lt43",
      "Light 48" = "lt48"
    ),
    Datetime = ymd_hm(paste0(Date, hour(`Time (PST)`), ":", minute(`Time (PST)`)), tz = "Etc/GMT+8"), # SDO reports time in PST throughout the year
    Datetime = with_tz(Datetime, tzone = "America/Los_Angeles"), # Convert from PST to local time to correspond with the other surveys
    Date = force_tz(Date, tzone = "America/Los_Angeles"),
    DepthBin = recode(`Depth (m)`, "1" = "top", "3" = "mid", "6" = "bottom"),
    Microcystis = `MC Score`,
    Secchi = `Secchi (cm)`,
    Temperature = `Water Temp (°C)`,
    Conductivity = `Sp Cond (µS/cm)`,
    DissolvedOxygen = `DO (mg/L)`,
    pH
  ) %>%
  # Remove middle depth samples and pivot data wider for parameters collected at top and bottom
  filter(DepthBin != "mid") %>%
  pivot_wider(names_from = DepthBin, values_from = c(Temperature, Conductivity, DissolvedOxygen, pH)) %>%
  rename_with(~ str_remove(.x, "_top$"), ends_with("_top")) %>%
  left_join(SDO_stations, by = c("Station" = "StationID"))

# Add 2021 data to earlier data
SDO <- bind_rows(SDO_1997_2018, SDO_2021)

usethis::use_data(SDO, overwrite = TRUE)
