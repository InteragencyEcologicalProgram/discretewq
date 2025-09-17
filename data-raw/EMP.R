## code to prepare `EMP` dataset goes here
# >>> NOTE:
# EMP is in the process of changing their data format from wide to long. However, this isn't
# reflected in the current EDI data publication (458.12), and they are waiting for lab data results
# from June-Dec 2024 before they update the data package in EDI. In addition, 458.12 doesn't
# distinguish between the two Turbidity units (NTU and FNU). Therefore, we are waiting to update the
# EMP dataset until the EDI data package is updated with the 1975-2024 data in the new long format.
# For now, we're using the same EDI revision (458.9) that was used in the last discretewq update.
# Once the EDI data package is updated with the 1975-2024 data in the long format, we'll update the
# EMP dataset in discretewq using the standardized workflow for EDI data.

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Source helper functions
source("data-raw/01_Global//data_raw_helpers.R")

# Define settings for dataset
survey <- "EMP"
date_fmt <- "Ymd"
time_fmt <- "HMS"
tzone <- "Etc/GMT+8"
secchi_unit <- "centimeters"
depth_unit <- "feet"

# Import data from EDI to temp dir
# Compile all data entities for EDI data package 458.9
edi_data_ent_all <- get_edi_data_entities("edi.458.9")

# Subset to desired data entities
edi_data_ent_sub <- str_subset(edi_data_ent_all, "2022$")

# Download data entities to temporary directory
get_edi_data("edi.458.9", edi_data_ent_sub)

# List files in temporary directory
temp_files <- list.files(tempdir(), full.names = TRUE)

# Read in station data
EMP_stations <-
  read_csv(
    str_subset(temp_files, "Stations"),
    col_types = cols_only(Station = "c", Latitude = "d", Longitude = "d")
  ) %>%
  drop_na()

# Read in EMP data (two turbidity units)
EMP_raw <- read_csv(
  str_subset(temp_files, "EMP_DWQ_1975_2022\\.bin$"),
  col_types = cols_only(
    Station = "c", Date = "c", Time = "c", FieldNotes = "c", Chla_Sign = "c", Chla = "d",
    Depth = "d", Secchi = "d", Microcystis = "d", SpCndSurface = "d", SpCndBottom = "d",
    DOSurface = "d", DOBottom = "d", DOpercentSurface = "d", DOpercentBottom = "d", WTSurface = "d",
    WTBottom = "d", pHSurface = "d", pHBottom = "d", TurbiditySurface_NTU = "d",
    TurbidityBottom_NTU = "d", TurbiditySurface_FNU = "d", TurbidityBottom_FNU = "d",
    NorthLat = "d", WestLong = "d", Pheophytin_Sign = "c", Pheophytin = "d",
    TotAlkalinity_Sign = "c", TotAlkalinity = "d", TotAmmonia_Sign = "c", TotAmmonia = "d",
    DissAmmonia_Sign = "c", DissAmmonia = "d", DissBromide_Sign = "c", DissBromide = "d",
    DissCalcium_Sign = "c", DissCalcium = "d", TotChloride_Sign = "c", TotChloride = "d",
    DissChloride_Sign = "c", DissChloride = "d", DissNitrateNitrite_Sign = "c",
    DissNitrateNitrite = "d", DOC_Sign = "c", DOC = "d", TOC_Sign = "c", TOC = "d", DON_Sign = "c",
    DON = "d", TON_Sign = "c", TON = "d", DissOrthophos_Sign = "c", DissOrthophos = "d",
    TotPhos_Sign = "c", TotPhos = "d", DissSilica_Sign = "c", DissSilica = "d", TDS_Sign = "c",
    TDS = "d", TSS_Sign = "c", TSS = "d", VSS_Sign = "c", VSS = "d", TKN_Sign = "c", TKN = "d"
  )
)

# clean data
EMP <- EMP_raw %>%
  rename(
    Notes = FieldNotes, Chlorophyll = Chla, Chlorophyll_Sign = Chla_Sign,
    Conductivity = SpCndSurface, Conductivity_bottom = SpCndBottom, Temperature = WTSurface,
    Temperature_bottom = WTBottom, DissolvedOxygen = DOSurface, DissolvedOxygen_bottom = DOBottom,
    DissolvedOxygenPercent = DOpercentSurface, DissolvedOxygenPercent_bottom = DOpercentBottom,
    TurbidityFNU = TurbiditySurface_FNU, TurbidityFNU_bottom = TurbidityBottom_FNU,
    TurbidityNTU = TurbiditySurface_NTU, TurbidityNTU_bottom = TurbidityBottom_NTU, pH = pHSurface,
    pH_bottom = pHBottom, Latitude = NorthLat, Longitude = WestLong
  ) %>%
  convert_datetime(date_fmt, time_fmt, tzone) %>%
  mutate(
    # EMP has some 1.5, 2.5 and 3.5 values
    Microcystis = round(Microcystis),
    # EMP always collects samples at High Slack
    Tide = "High Slack"
  ) %>%
  # Convert feet to meters
  convert_depth(depth_unit) %>%
  # Add coordinates
  left_join(EMP_stations, by = join_by(Station), suffix = c("_field", "")) %>%
  mutate(
    Field_coords = case_when(
      is.na(Latitude) & !is.na(Latitude_field) ~ TRUE,
      is.na(Longitude) & !is.na(Longitude_field) ~ TRUE,
      .default = FALSE
    ),
    Latitude = if_else(is.na(Latitude), Latitude_field, Latitude),
    Longitude = if_else(is.na(Longitude), Longitude_field, Longitude),
    .keep = "unused"
  ) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Add source variable
  add_source_col(survey) %>%
  standardize_col_order()

usethis::use_data(EMP, overwrite = TRUE)
