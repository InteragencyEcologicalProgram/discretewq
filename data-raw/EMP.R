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
library(lubridate)
library(tidyr)
library(stringr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("R/data_raw_helpers.R")

# Define settings for dataset
survey <- "EMP"
edi_pack_id <- 458

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Define data entities for download from EDI and their regex patterns
ent_regex <- c(
  "df_stations" = "Stations",
  "df_data" = "EMP_DWQ_\\d{4}"
)

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data("edi.458.9", ent_regex)

# Read in station data
EMP_stations <-
  read_csv(
    edi_fp["df_stations"],
    col_types = cols_only(Station = "c", Latitude = "d", Longitude = "d")
  ) %>%
  drop_na()

# Read in EMP data (two turbidity units)
EMP_raw <-
  read_csv(
    edi_fp["df_data"],
    col_types = cols_only(
      Station = "c",
      Date = "c",
      Time = "c",
      FieldNotes = "c",
      Chla_Sign = "c",
      Chla = "d",
      Depth = "d",
      Secchi = "d",
      Microcystis = "d",
      SpCndSurface = "d",
      SpCndBottom = "d",
      DOSurface = "d",
      DOBottom = "d",
      DOpercentSurface = "d",
      DOpercentBottom = "d",
      WTSurface = "d",
      WTBottom = "d",
      pHSurface = "d",
      pHBottom = "d",
      TurbiditySurface_NTU = "d",
      TurbidityBottom_NTU = "d",
      TurbiditySurface_FNU = "d",
      TurbidityBottom_FNU = "d",
      NorthLat = "d",
      WestLong = "d",
      Pheophytin_Sign = "c",
      Pheophytin = "d",
      TotAlkalinity_Sign = "c",
      TotAlkalinity = "d",
      TotAmmonia_Sign = "c",
      TotAmmonia = "d",
      DissAmmonia_Sign = "c",
      DissAmmonia = "d",
      DissBromide_Sign = "c",
      DissBromide = "d",
      DissCalcium_Sign = "c",
      DissCalcium = "d",
      TotChloride_Sign = "c",
      TotChloride = "d",
      DissChloride_Sign = "c",
      DissChloride = "d",
      DissNitrateNitrite_Sign = "c",
      DissNitrateNitrite = "d",
      DOC_Sign = "c",
      DOC = "d",
      TOC_Sign = "c",
      TOC = "d",
      DON_Sign = "c",
      DON = "d",
      TON_Sign = "c",
      TON = "d",
      DissOrthophos_Sign = "c",
      DissOrthophos = "d",
      TotPhos_Sign = "c",
      TotPhos = "d",
      DissSilica_Sign = "c",
      DissSilica = "d",
      TDS_Sign = "c",
      TDS = "d",
      TSS_Sign = "c",
      TSS = "d",
      VSS_Sign = "c",
      VSS = "d",
      TKN_Sign = "c",
      TKN = "d"
    )
  )

# clean data
EMP <- EMP_raw %>%
  rename(
    Notes = FieldNotes,
    Chlorophyll = Chla,
    Chlorophyll_Sign = Chla_Sign,
    Conductivity = SpCndSurface,
    Conductivity_bottom = SpCndBottom,
    Temperature = WTSurface,
    Temperature_bottom = WTBottom,
    DissolvedOxygen = DOSurface,
    DissolvedOxygen_bottom = DOBottom,
    DissolvedOxygenPercent = DOpercentSurface,
    DissolvedOxygenPercent_bottom = DOpercentBottom,
    TurbidityFNU = TurbiditySurface_FNU,
    TurbidityFNU_bottom = TurbidityBottom_FNU,
    TurbidityNTU = TurbiditySurface_NTU,
    TurbidityNTU_bottom = TurbidityBottom_NTU,
    pH = pHSurface,
    pH_bottom = pHBottom,
    Latitude = NorthLat,
    Longitude = WestLong
  ) %>%
  convert_date("Ymd") %>%
  convert_time("HMS") %>%
  combine_datetime(timezone = "Etc/GMT+8") %>%
  convert_depth("feet") %>%
  convert_secchi("cm") %>%
  mutate(
    # EMP has some 1.5, 2.5 and 3.5 values
    Microcystis = round(Microcystis),
    # EMP always collects samples at High Slack
    Tide = "High Slack"
  ) %>%
  # Add coordinates
  left_join(EMP_stations, by = join_by(Station), suffix = c("_field", "")) %>%
  resolve_lat_long() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Standardize column order
  standardize_col_order() %>%
  add_update_info("edi.458.9")

usethis::use_data(EMP, overwrite = TRUE)
document_helper_edi("edi.458.9", EMP)
