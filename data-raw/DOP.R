# Code to prepare `DOP` dataset
library(dplyr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

source("data-raw/01_Global/data_raw_helpers.R")

survey <- "DOP"

# Run standardized workflow to import data from EDI and process it
ls_DOP <- import_proc_edi_data(survey)

# Finish cleaning up data
# Notes:
  # We're not including Microcystis because they use a different method
  # Turbidity is measured with a YSI EXO2 sonde according to the DOP methods - units are FNU
DOP <- ls_DOP$df_data %>%
  # Remove Channel Deep and Oblique samples. Channel Deep measurements are taken at the bottom third
  # to half of the water column and therefore aren't comparable to bottom samples from other
  # surveys. The WQ measurements for the Oblique tows are either all NA or they are identical to
  # either the Channel Surface or Channel Deep samples collected at the same location.
  filter(!Habitat %in% c("Channel Deep", "Oblique")) %>%
  # Combine Station_Code and Habitat columns to make the Station column to preserve habitat info for
    # each station
  mutate(
    Station = paste(Station_Code, Habitat),
    Field_coords = TRUE
  ) %>%
  # Remove rows where all measurements are NA
  rm_rows_all_miss_data() %>%
  # Remove replicate tows with identical WQ values - select earliest Datetime
  arrange(Datetime) %>%
  distinct(
    Station, Date, Secchi, Temperature, Salinity, Conductivity, DissolvedOxygen, pH,
    TurbidityFNU, Chlorophyll,
    .keep_all = TRUE
  ) %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Date, Station)

usethis::use_data(DOP, overwrite = TRUE)

document_helper_edi(ls_DOP$edi_id, DOP)
update_edi_metadata(survey, ls_DOP$edi_id)
