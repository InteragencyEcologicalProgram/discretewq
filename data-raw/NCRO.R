## code to prepare `NCRO` dataset goes here
require(readr)
require(readxl)
require(dplyr)
require(tidyr)
require(lubridate)
require(stringr)
require(purrr)

# Import data field and laboratory provided by NCRO-WQES
NCRO_all <-
  map(
    dir("data-raw/NCRO", pattern = "^WQES.+\\.xlsx", full.names = TRUE),
    read_excel
  ) %>%
  list_rbind()

# Import Secchi depth and Microcystis data, which were in a different Excel file
NCRO_secchi_mvi <- read_excel("data-raw/NCRO/All WQES Station HAB Obs and Secchi 2017-2021.xlsx")

# Import station coordinates
# >>> This will need updating after I hear back from Tyler about the station issues
stations <- read_csv("data-raw/NCRO/stationLatLongs.csv")

# Create a table of Analytes to include and their standardized abbreviations
analytes <- tribble(
  ~ Analyte, ~ AnalyteStd,
  "Chlorophyll a", "Chlorophyll",
  "Dissolved Ammonia", "DissAmmonia",
  "Dissolved Bromide", "DissBromide",
  "Dissolved Calcium", "DissCalcium",
  "Dissolved Chloride", "DissChloride",
  "Dissolved Nitrate + Nitrite", "DissNitrateNitrite",
  "Dissolved Organic Carbon", "DOC",
  "Dissolved Organic Nitrogen", "DON",
  "Dissolved ortho-Phosphate", "DissOrthophos",
  "Field (Bottom) Dissolved Oxygen", "DissolvedOxygen_bottom",
  "Field Dissolved Oxygen", "DissolvedOxygen",
  "Field Specific Conductance", "Conductivity",
  "Field Turbidity", "Turbidity",
  "Field Water Temperature", "Temperature",
  "Field pH", "pH",
  "Pheophytin a", "Pheophytin",
  "Total Alkalinity", "TotAlkalinity",
  "Total Dissolved Solids", "TDS",
  "Total Kjeldahl Nitrogen", "TKN",
  "Total Organic Carbon", "TOC",
  "Total Phosphorus", "TotPhos",
  "Total Suspended Solids", "TSS",
  "Turbidity", "Turbidity", # this may be a lab measurement, keeping it in for now
  "Volatile Suspended Solids", "VSS"
)

# Prepare Secchi depth and Microcystis data to be joined with field and laboratory data
secHABs <- NCRO_secchi_mvi %>%
  mutate(
    # Use the numeric codes for Microcystis
    Microcystis = case_when(
      FldObsWaterHabs %in% c("Not Visible", "Absent") ~ 1,
      FldObsWaterHabs == "Low" ~ 2,
      FldObsWaterHabs == "Medium" ~ 3,
      FldObsWaterHabs == "High" ~ 4,
      FldObsWaterHabs == "High" ~ 5
    ),
    # convert secchi depth to cm
    Secchi = FldObsSecchi * 100,
    Date = date(DeploymentEnd),
    Source = "DWR_NCRO"
  ) %>%
  # Join station coordinates
  # >>> This results in duplicated records due to MRX and ORX having two records
    # in the stations table - This needs to be resolved before publishing
  left_join(stations, c("StationCode" = "Station")) %>%
  rename(Station = StationCode) %>%
  select(Source, Station, Date, Secchi, Microcystis, Latitude, Longitude) %>%
  group_by(Source, Station, Date, Latitude, Longitude) %>%
  summarize(Secchi = mean(Secchi, na.rm = T), Microcystis = mean(Microcystis, na.rm = T))

# Start cleaning the field and laboratory data
NCRO_all_c1 <- NCRO_all %>%
  # Only include Normal Samples and analytes listed in the analytes table.
  # Remove stations with no station name or number.
  filter(
    `Sample Type` == "Normal Sample",
    Analyte %in% analytes$Analyte,
    !str_detect(`Long Station Name`, "^\\(")
  ) %>%
  # Join station info from stations table - we'll want to join by WDL station
  # number once we get that fixed in the stations table
  left_join(stations, by = join_by(`Long Station Name`)) %>%
  # Add standardized analyte abbreviations
  left_join(analytes, by = join_by(Analyte)) %>%
  transmute(
    StationName = `Long Station Name`,
    StationNumber = `Station Number`,
    SampleCode = `Sample Code`,
    # Since NCRO records only in PST, convert to local time to correspond with the other surveys
    Datetime = with_tz(mdy_hm(`Collection Date`, tz = "Etc/GMT+8"), tzone = "America/Los_Angeles"),
    Date = date(Datetime),
    Analyte = AnalyteStd,
    Result,
    # add Sign variable which indicates <RL values
    Sign = if_else(str_detect(Result, "^<"), "<", "="),
    RL = `Rpt Limit`,
    Units,
    Method,
    Notes,
    Latitude,
    Longitude
  ) %>%
  # convert Result to numeric making <RL values equal to their RL
  mutate(
    Result = case_when(
      Result %in% c("N.S.", "D1") ~ NA_character_,
      Sign == "<" ~ RL,
      TRUE ~ Result
    ),
    Result = as.numeric(Result)
  ) %>%
  # remove rows with NA values in Result
  drop_na(Result) %>%
  mutate(
    # convert a few Dissolved Calcium records from ug/L to mg/L
    Result = if_else(Analyte == "DissCalcium" & Units == "ug/L", Result / 1000, Result),
    # define Turbidity measurement methods
    Analyte = case_when(
      Units == "N.T.U." ~ "TurbidityNTU",
      Units == "F.N.U." ~ "TurbidityFNU",
      TRUE ~ Analyte
    )
  ) %>%
  # Remove RL, Units, and Method variables because they're no longer needed
  select(-c(RL, Units, Method))

# Pull out duplicate records - more than 1 sample collected at a station and same DateTime
NCRO_all_dups <- NCRO_all_c1 %>%
  count(StationName, Datetime, Analyte) %>%
  filter(n > 1) %>%
  select(-n) %>%
  left_join(NCRO_all_c1, by = join_by(StationName, Datetime, Analyte))

# Clean up duplicate records
NCRO_all_dups_c <- NCRO_all_dups %>%
  # remove records that have "duplicate" in their Notes
  filter(!str_detect(Notes, regex("duplicate", ignore_case = TRUE)) | is.na(Notes)) %>%
  # Clean up the remaining duplicates by keeping only the first sample of the pair
  group_by(StationName, Datetime, Analyte) %>%
  mutate(RepNum = row_number()) %>%
  ungroup() %>%
  filter(RepNum == 1) %>%
  select(-RepNum)

# Add the data frame with the cleaned up duplicates back to the main data frame
NCRO_all_c2 <- NCRO_all_c1 %>%
  anti_join(NCRO_all_dups) %>%
  bind_rows(NCRO_all_dups_c)

# Pivot data wider by Result and Sign variables
NCRO_all_c2_wide <- NCRO_all_c2 %>%
  select(-Notes) %>%
  pivot_wider(
    names_from = Analyte,
    values_from = c(Result, Sign),
    names_glue = "{Analyte}_{.value}"
  ) %>%
  # Clean up names for the columns with results
  rename_with(~ str_remove(.x, "_Result$"), ends_with("_Result")) %>%
  # Remove unnecessary _Sign variables
  select(
    -c(
      Conductivity_Sign,
      pH_Sign,
      Temperature_Sign,
      DissolvedOxygen_Sign,
      DissolvedOxygen_bottom_Sign,
      TurbidityNTU_Sign,
      TurbidityFNU_Sign
    )
  ) %>%
  # Fill in "=" for NA values within the _Sign variables
  mutate(across(ends_with("_Sign"), ~ if_else(is.na(.x), "=", .x)))

# >>> Start here once we resolve issues with stations in both NCRO_all_c2_wide and secHABs

#reorder to match the others
NCRO <- left_join(NCRO, secHABs) %>%
  select(
    Source,
    Station,
    Date,
    Datetime,
    Latitude,
    Longitude,
    Secchi,
    Microcystis,
    Temperature,
    Conductivity,
    DissolvedOxygen,
    DissolvedOxygen_bottom,
    pH,
    Turbidity,
    TotAlkalinity_Sign,
    TotAlkalinity,
    DissAmmonia_Sign,
    DissAmmonia,
    DissChloride_Sign,
    DissChloride,
    DissCalcium_Sign,
    DissCalcium,
    Chlorophyll_Sign,
    Chlorophyll,
    Pheophytin_Sign,
    Pheophytin,
    DissBromide_Sign,
    DissBromide,
    DissNitrateNitrite_Sign,
    DissNitrateNitrite,
    DOC_Sign,
    DOC,
    DON_Sign,
    DON,
    TKN_Sign,
    TKN,
    DissOrthophos_Sign,
    DissOrthophos,
    TotPhos_Sign,
    TotPhos,
    TOC_Sign,
    TOC,
    VSS_Sign,
    VSS,
    TSS_Sign,
    TSS
  )



usethis::use_data(NCRO, overwrite = TRUE)
