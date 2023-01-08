## code to prepare `USGS_SFBS` dataset goes here

require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(purrr)
require(readxl)
require(stringr)

#Latest USGS_SFBS data from: https://sfbay.wr.usgs.gov/water-quality-database/

USGS_SFBS_stations <- read_excel(file.path("data-raw", "USGS_SFBS", "USGSSFBayStations.xlsx"))%>%
  select(Station, Latitude="Latitude degree", Longitude="Longitude degree")%>%
  mutate(Station=as.character(Station))

# 1969 to 2015 data: https://www.sciencebase.gov/catalog/item/5841f97ee4b04fc80e518d9f
USGS_SFBS_1969_2015 <-
  read_csv(
    file = "data-raw/USGS_SFBS/1969_2015USGS_SFBAY_22APR20.csv",
    col_types = cols_only(
      Date = "c",
      Time = "c",
      Station = "d",
      Depth = "d",
      `Discrete DO` = "d", # Use discrete DO from 1969-2015
      `Calculated Chl-a` = "d",
      Salinity = "d",
      Temperature = "d",
      NO32 = "d",
      NH4 = "d",
      PO4 = "d",
      Si = "d"
    )
  ) %>%
  rename(
    DissolvedOxygen = `Discrete DO`,
    Chlorophyll = `Calculated Chl-a`,
    DissNitrateNitrite = NO32,
    DissAmmonia = NH4,
    DissOrthophos = PO4,
    DissSilica = Si
  )

# 2016-2019 published data: https://www.sciencebase.gov/catalog/item/5966abe6e4b0d1f9f05cf551
USGS_SFBSfiles_pub <- list.files(path = file.path("data-raw", "USGS_SFBS"), full.names = T, pattern="SanFranciscoBayWaterQualityData.csv")
USGS_SFBS_pub <-
  map(
    USGS_SFBSfiles_pub,
    ~ read_csv(.x, col_types = cols(.default = "c")) %>%
      select(
        Date,
        Time,
        Station = Station_Number,
        Depth,
        Chlorophyll = Calculated_Chlorophyll,
        DissolvedOxygen = matches("^Oxygen|Calculated_Oxygen"), # Use DO from CTD sensor from 2016-onward
        Salinity,
        Temperature,
        DissNitrateNitrite = `Nitrate_+_Nitrite`,
        DissAmmonia = Ammonium,
        DissOrthophos = Phosphate,
        DissSilica = Silicate
      )
  ) %>%
  bind_rows() %>%
  mutate(
    Time = paste0(str_sub(Time, end = -3), ":", str_sub(Time, start = -2)),
    across(-c("Date", "Time"), as.numeric)
  )

# 2020-2022 data downloaded from website: https://sfbay.wr.usgs.gov/water-quality-database/
USGS_SFBSfiles_web<-list.files(path = file.path("data-raw", "USGS_SFBS"), full.names = T, pattern="wqdata")
USGS_SFBS_web <-
  map_dfr(
    USGS_SFBSfiles_web,
    ~ read_csv(
      .x,
      col_types = cols_only(
        Date = "c",
        Time = "c",
        Station = "d",
        `Depth (m)` = "d",
        `Calculated Chlorophyll-a (micrograms/L)` = "d",
        `Oxygen (mg/L)` = "d",
        `Oxygen % Saturation` = "d",
        Salinity = "d",
        `Temperature (Degrees Celsius)` = "d",
        `NO32 (Micromolar)` = "d",
        `NH4 (Micromolar)` = "d",
        `PO4 (Micromolar)` = "d",
        `Si (Micromolar)` = "d"
      )
    )
  ) %>%
  rename(
    Depth = `Depth (m)`,
    Chlorophyll = `Calculated Chlorophyll-a (micrograms/L)`,
    DissolvedOxygen = `Oxygen (mg/L)`,
    DissolvedOxygenPercent = `Oxygen % Saturation`,
    Temperature = `Temperature (Degrees Celsius)`,
    DissNitrateNitrite = `NO32 (Micromolar)`,
    DissAmmonia = `NH4 (Micromolar)`,
    DissOrthophos = `PO4 (Micromolar)`,
    DissSilica = `Si (Micromolar)`
  )

# Combine data and start with general clean up
USGS_SFBS_c <-
  bind_rows(USGS_SFBS_1969_2015, USGS_SFBS_pub, USGS_SFBS_web) %>%
  filter(!is.na(Date)) %>%
  mutate(
    Date = parse_date_time(Date, orders = c("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d"), tz = "America/Los_Angeles"),
    Time = hm(Time),
    Station = as.character(Station)
  ) %>%
  mutate(
    Datetime = parse_date_time(paste0(Date, " ", hour(Time), ":", minute(Time)), "%Y-%m-%d %H:%M", tz = "America/Los_Angeles"),
    Source = "USGS_SFBS"
  ) %>%
  select(-Time) %>%
  rename(Sample_depth = Depth)

# Define thresholds for minimum surface sample depths
wq_surf_depth <- 2
nutr_surf_depth <- 5

# Define columns containing WQ measurements
wq_param <- c(
  "Chlorophyll",
  "Salinity",
  "Temperature",
  "DissolvedOxygen",
  "DissolvedOxygenPercent"
)

# Define columns containing nutrient measurements
nutr_param <- c(
  "DissNitrateNitrite",
  "DissAmmonia",
  "DissOrthophos",
  "DissSilica"
)

# Select surface and bottom WQ samples and nutrient samples
USGS_SFBS_c1 <- USGS_SFBS_c %>%
  pivot_longer(cols = all_of(c(wq_param, nutr_param)), names_to = "Parameter", values_to = "Value") %>%
  drop_na(Value) %>%
  group_by(Date, Station, Parameter) %>%
  mutate(
    Depth_bin = case_when(
      Parameter %in% wq_param & Sample_depth < wq_surf_depth & Sample_depth == min(Sample_depth) ~ "surface",
      Parameter %in% nutr_param & Sample_depth < nutr_surf_depth & Sample_depth == min(Sample_depth) ~ "nutrient",
      Parameter %in% wq_param & Sample_depth > wq_surf_depth & Sample_depth == max(Sample_depth) ~ "bottom",
      TRUE ~"other"
    )
  ) %>%
  ungroup() %>%
  filter(
    Depth_bin != "other",
    !(Parameter == "Chlorophyll" & Depth_bin == "bottom") # exclude Chlorophyll bottom samples
  )

# Pull out duplicated records collected at the same station on the same day
USGS_SFBS_c1_dups <- USGS_SFBS_c1 %>%
  count(Date, Station, Parameter, Depth_bin) %>%
  filter(n > 1) %>%
  select(-n) %>%
  left_join(USGS_SFBS_c1)

# Clean up the duplicated records by keeping the first sample of the day
USGS_SFBS_c1_dups_c <- USGS_SFBS_c1_dups %>%
  group_by(Date, Station, Parameter, Depth_bin) %>%
  filter(Datetime == min(Datetime)) %>%
  ungroup()

# Add the cleaned up duplicate data to the original data set
USGS_SFBS_c2 <- USGS_SFBS_c1 %>%
  anti_join(USGS_SFBS_c1_dups) %>%
  bind_rows(USGS_SFBS_c1_dups_c) %>%
  # Average sample depths among each Depth_bin so they match correctly when pivoted wider
  group_by(Date, Station, Depth_bin) %>%
  mutate(Sample_depth = mean(Sample_depth)) %>%
  # Average Datetimes for each sample so that they match correctly as well
  ungroup(Depth_bin) %>%
  mutate(Datetime = mean(Datetime)) %>%
  ungroup()

# Pull out WQ parameters and restructure data frame to wide format
USGS_SFBS_c2_wq <- USGS_SFBS_c2 %>%
  filter(Depth_bin != "nutrient") %>%
  pivot_wider(names_from = Parameter, values_from = Value) %>%
  pivot_wider(names_from = Depth_bin, values_from = where(is.numeric)) %>%
  select(
    Source,
    Station,
    Date,
    Datetime,
    Sample_depth_surface,
    Sample_depth_bottom,
    Temperature = Temperature_surface,
    Temperature_bottom,
    Salinity = Salinity_surface,
    Salinity_bottom,
    Chlorophyll = Chlorophyll_surface,
    DissolvedOxygen = DissolvedOxygen_surface,
    DissolvedOxygen_bottom,
    DissolvedOxygenPercent = DissolvedOxygenPercent_surface,
    DissolvedOxygenPercent_bottom
  )

# Pull out nutrient parameters and restructure data frame to wide format
USGS_SFBS_c2_nutr <- USGS_SFBS_c2 %>%
  filter(Depth_bin == "nutrient") %>%
  pivot_wider(id_cols = -Depth_bin, names_from = Parameter, values_from = Value) %>%
  transmute(
    Source,
    Station,
    Date,
    Datetime,
    Sample_depth_nutr_surface = Sample_depth,
    # convert units
    DissNitrateNitrite = DissNitrateNitrite * (14.007 / (10^3)), # molar mass of N
    DissAmmonia = DissAmmonia * (14.007 / (10^3)), # molar mass of N
    DissOrthophos = DissOrthophos * (30.974 / (10^3)), # molar mass of P
    DissSilica = DissSilica * (60.084 / (10^3)) # molar mass of SiO2
  )

# Join WQ and nutrient data back together
USGS_SFBS <-
  full_join(USGS_SFBS_c2_wq, USGS_SFBS_c2_nutr) %>%
  left_join(USGS_SFBS_stations, by = "Station") %>%
  relocate(Latitude, Longitude, .after = Station) %>%
  arrange(Date, Station)

usethis::use_data(USGS_SFBS, overwrite = TRUE)
