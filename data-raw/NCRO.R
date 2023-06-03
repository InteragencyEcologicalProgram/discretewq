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

# Import station metadata and coordinates
stations <- read_csv("data-raw/NCRO/Stations.csv")

# Import station coordinates from CNRA Data Portal to fill in missing coordinates
stations_cnra <- read_csv("https://data.cnra.ca.gov/dataset/3f96977e-2597-4baa-8c9b-c433cea0685e/resource/24fc759a-ff0b-479a-a72a-c91a9384540f/download/stations.csv")

# Create vector of Station Numbers with missing coordinates
stations_missing <- stations %>%
  filter(is.na(Latitude)) %>%
  pull(StationNumber)

# Pull out station coordinates from CNRA Data Portal for the Station Numbers
  # with missing coordinates
stations_cnra_f <- stations_cnra %>%
  filter(station_number %in% stations_missing) %>%
  select(
    station_number,
    Latitude_cnra = latitude,
    Longitude_cnra = longitude
  )

# Fill in missing coordinates - This is the complete table of standardized
  # station numbers and names
stations_f <- stations %>%
  left_join(stations_cnra_f, by = join_by(StationNumber == station_number)) %>%
  mutate(
    Latitude = if_else(is.na(Latitude), Latitude_cnra, Latitude),
    Longitude = if_else(is.na(Longitude), Longitude_cnra, Longitude),
    across(contains("Date"), mdy)
  ) %>%
  select(!ends_with("_cnra"))

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
      FldObsWaterHabs == "Extreme" ~ 5
    ),
    # convert secchi depth to cm
    Secchi = FldObsSecchi * 100,
    # Use just the Date since the DateTimes don't completely match with the
      # field/lab data
    Date = date(DeploymentEnd)
  ) %>%
  select(-StationName) %>%
  # Join standardized station numbers and names
  left_join(stations_f, by = join_by(StationCode == WQES_StationCode)) %>%
  select(StationNumber, StationName, Date, Secchi, Microcystis) %>%
  # Remove Stations and Dates that are NA and records without Secchi and Microcystis values
  drop_na(StationName) %>%
  drop_na(Date) %>%
  filter(!if_all(c(Secchi, Microcystis), is.na)) %>%
  # Remove a few duplicated records
  distinct()

# Start cleaning the field and laboratory data
NCRO_all_c1 <- NCRO_all %>%
  # Only include Normal Samples and analytes listed in the analytes table.
  # Remove stations with no station name or number.
  filter(
    `Sample Type` == "Normal Sample",
    Analyte %in% analytes$Analyte,
    !str_detect(`Long Station Name`, "^\\(")
  ) %>%
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
    Notes
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
  # convert a few Dissolved Calcium records from ug/L to mg/L
  mutate(Result = if_else(Analyte == "DissCalcium" & Units == "ug/L", Result / 1000, Result)) %>%
  # Remove RL and Units variables because they're no longer needed
  select(-c(RL, Units))

# Standardize station names - first by pulling out all StationNumber with
  # (UserDefined) or (NONE) and substituting them with their proper number.
  # We worked with Tyler Salman from NCRO-WQES to determine these.
NCRO_unk_sta <- NCRO_all_c1 %>%
  filter(StationNumber %in% c("(UserDefined)", "(NONE)")) %>%
  mutate(
    StationNumber = case_when(
      StationName == "Holland Cut at Holland Marina" ~ "B9D75841349",
      StationName == "Middle River @ Undine Road" ~ "B9D75011230",
      StationName == "Middle River near Tracy Road" ~ "B9D75291280",
      StationName == "Old River at Bacon Island" ~ "B9D75811344",
      StationName == "Old River near Bacon Island @ USGS Pile" & StationNumber == "(UserDefined)" ~ "B9D75811344",
      StationName == "Old River near Bacon Island @ USGS Pile" & StationNumber == "(NONE)" ~ "B9D75821343A",
      StationName == "Rock Slough @ CCWD" ~ "B9522200",
      StationName == "Rock Slough @ Contra Costa WD Fish Screen" ~ "B9522200",
      StationName == "San Joaquin River at Blind Point" ~ "B9D80201431",
      TRUE ~ StationNumber
    )
  ) %>%
  # Remove remaining records with StationNumber as (UserDefined) or (NONE)
  filter(!StationNumber %in% c("(UserDefined)", "(NONE)"))

# Add data with corrected station names and numbers back to the main data frame
NCRO_all_c2 <- NCRO_all_c1 %>%
  filter(!StationNumber %in% c("(UserDefined)", "(NONE)")) %>%
  bind_rows(NCRO_unk_sta) %>%
  # Use standardized station names from stations_f table
  select(-StationName) %>%
  left_join(stations_f %>% select(StationNumber, StationName), by = join_by(StationNumber)) %>%
  relocate(StationName) %>%
  # Remove rows without station names - we're only including stations in the
    # stations_f table
  drop_na(StationName) %>%
  # Remove one set of samples from Old River near Head that wasn't collected by NCRO
  filter(!(StationName == "Old River near Head" & Date == "2018-09-20"))

# Define Turbidity measurement methods - NTU vs FNU
NCRO_all_c3 <- NCRO_all_c2 %>%
  left_join(stations_f %>% select(StationNumber, contains("Date")), by = join_by(StationNumber)) %>%
  mutate(
    Analyte = case_when(
      Analyte == "Turbidity" & Date <= LastDate_NTU ~ "TurbidityNTU",
      Analyte == "Turbidity" & Date >= FirstDate_FNU ~ "TurbidityFNU",
      TRUE ~ Analyte
    )
  ) %>%
  select(-c(LastDate_NTU, FirstDate_FNU))

# Pull out duplicate records - more than 1 sample collected at a station and same DateTime
NCRO_dt_dups <- NCRO_all_c3 %>%
  count(StationName, Datetime, Analyte) %>%
  filter(n > 1) %>%
  select(-n) %>%
  left_join(NCRO_all_c3, by = join_by(StationName, Datetime, Analyte))

# Clean up duplicate records
NCRO_dt_dups_c <- NCRO_dt_dups %>%
  # remove records that have "duplicate" in their Notes
  filter(!str_detect(Notes, regex("duplicate", ignore_case = TRUE)) | is.na(Notes)) %>%
  # Clean up the remaining duplicates by keeping only the first sample of the pair
  group_by(StationName, Datetime, Analyte) %>%
  mutate(RepNum = row_number()) %>%
  ungroup() %>%
  filter(RepNum == 1) %>%
  select(-RepNum)

# Add the data frame with the cleaned up duplicates back to the main data frame
NCRO_all_c4 <- NCRO_all_c3 %>%
  anti_join(NCRO_dt_dups) %>%
  bind_rows(NCRO_dt_dups_c)

# There are some instances where samples were collected on the same day at a
  # station, but with slightly different Datetimes. We'll clean these up by
  # removing the samples not collected by NCRO-WQES identified by Tyler Salman.
# NOTE: There are probably other samples in the data set not collected by
  # NCRO-WQES, but these will be too difficult to remove at this point.
same_date_dups_rm <- read_csv("data-raw/NCRO/duplicates_same_date.csv") %>%
  filter(KeepRecord == "no") %>%
  distinct(SampleCode) %>%
  pull(SampleCode)

NCRO_all_c5 <- NCRO_all_c4 %>% filter(!SampleCode %in% same_date_dups_rm)

# Correct a few erroneous times (most likely not recorded in military time format)
NCRO_all_c6 <- NCRO_all_c5 %>%
  mutate(
    Datetime = case_when(
      hour(Datetime) %in% 0:2 ~ Datetime + hours(12),
      StationName == "Holland Cut at Holland Marina" & Datetime == "2007-06-05 03:00:00" ~ Datetime + hours(12),
      # The two other samples collected in the 3:00 hour are probably off by 4
        # hours looking at Datetimes of the other samples collected that day
      hour(Datetime) == 3 ~ Datetime + hours(4),
      TRUE ~ Datetime
    )
  )

# There are some questionable WQ measurement values based on gross ranges of
  # each parameter. NCRO-WQES staff checked and corrected/verified these values
  # from their field sheet records.
corr_wq_meas <- read_csv("data-raw/NCRO/ncro_questionable_wq_meas_Verified.csv") %>%
  select(
    SampleCode,
    Analyte,
    MeasVerified = `Measurement Verified (Yes, No, or Unknown)`,
    ResultVerified = `Verified Result`
  )

# First, remove samples not collected by NCRO-WQES indicated in corr_wq_meas by
  # NCRO-WQES staff
samples_rm <- corr_wq_meas %>%
  filter(str_detect(MeasVerified, regex("unknown", ignore_case = TRUE))) %>%
  distinct(SampleCode) %>%
  pull(SampleCode)

NCRO_all_c7 <- NCRO_all_c6 %>% filter(!SampleCode %in% samples_rm)

# Next, pull out WQ measurement values from main data frame, correct them, and
  # add them back
NCRO_corr_wq_meas <- NCRO_all_c7 %>%
  inner_join(corr_wq_meas, by = join_by(SampleCode, Analyte)) %>%
  select(-c(Result, MeasVerified)) %>%
  rename(Result = ResultVerified) %>%
  drop_na(Result)

NCRO_all_c8 <- NCRO_all_c7 %>%
  anti_join(
    corr_wq_meas %>% select(SampleCode, Analyte),
    by = join_by(SampleCode, Analyte)
  ) %>%
  bind_rows(NCRO_corr_wq_meas)

# Pivot data wider by Result and Sign variables
NCRO_all_c8_wide <- NCRO_all_c8 %>%
  select(-c(SampleCode, Notes)) %>%
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

# Add Secchi depth and Microcystis data and lat-long coordinates
NCRO <- NCRO_all_c8_wide %>%
  # There are some records in secHABs with no matches in NCRO_all_c8_wide, but
    # we'll use a left join for now
  left_join(secHABs, by = join_by(StationName, StationNumber, Date)) %>%
  left_join(
    stations_f %>% select(StationNumber, StationName, Latitude, Longitude),
    by = join_by(StationName, StationNumber)
  ) %>%
  # reorder to match all other datasets in package
  transmute(
    Source = "NCRO",
    Station = StationName,
    Latitude,
    Longitude,
    Date,
    Datetime,
    Secchi,
    Microcystis,
    Temperature,
    Conductivity,
    DissolvedOxygen,
    DissolvedOxygen_bottom,
    pH,
    TurbidityNTU,
    TurbidityFNU,
    TotAlkalinity_Sign,
    TotAlkalinity,
    DissAmmonia_Sign,
    DissAmmonia,
    DissBromide_Sign,
    DissBromide,
    DissCalcium_Sign,
    DissCalcium,
    DissChloride_Sign,
    DissChloride,
    Chlorophyll_Sign,
    Chlorophyll,
    Pheophytin_Sign,
    Pheophytin,
    DissNitrateNitrite_Sign,
    DissNitrateNitrite,
    DOC_Sign,
    DOC,
    TOC_Sign,
    TOC,
    DON_Sign,
    DON,
    DissOrthophos_Sign,
    DissOrthophos,
    TotPhos_Sign,
    TotPhos,
    TSS_Sign,
    TSS,
    VSS_Sign,
    VSS,
    TKN_Sign,
    TKN,
    TDS_Sign,
    TDS
  ) %>%
  arrange(Datetime)

usethis::use_data(NCRO, overwrite = TRUE)
