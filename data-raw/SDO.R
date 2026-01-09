# Code to prepare `SDO` dataset
library(dplyr)
library(lubridate)
library(hms)
library(stringr)
library(tidyr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "SDO"
tzone <- "Etc/GMT+8"

# Run standardized workflow to import data and process it
# Currently, SDO data from 1997-2018 is available on EDI (edi.276.2). Data collected from 2021-2024
  # was provided by Julianna Manning (DWR-EMP) on 5/8/2025. EMP is working on publishing their more
  # recent data on EDI.
edi_metadata <- get_edi_data(survey, edi_id = "edi.276.2")
ls_SDO <- import_proc_data(survey, df_files = edi_metadata$df_edi_files)

# Prepare tables before binding them together
df_data_1997_2018 <- ls_SDO$df_data_1997_2018 %>%
  mutate(
    # Fix one erroneous time recorded as "111" which should probably be "1111"
    Time = if_else(Time == "111", "1111", Time),
    Time = str_pad(Time, width = 4, side = "left", pad = "0")
  ) %>%
  combine_datetime(dt_fmt = "Ymd HM", timezone = tzone) %>%
  # Convert Microcystis to a common scale with other surveys - define "present" as NA and "absent"
    # as 1, round the numeric values to the nearest whole number
  mutate(
    Microcystis = case_match(
      Microcystis,
      "present" ~ NA_character_,
      "absent" ~ "1",
      .default = Microcystis
    ),
    Microcystis = round(as.numeric(Microcystis))
  )

df_data_2021_curr <- ls_SDO$df_data_2021_curr %>%
  # Dates and times are converted to numeric values when importing Excel files with text column type
  mutate(
    Date = as_date(Date, origin = "1899-12-30"),
    Time = as_hms(Time * 60 * 60 * 24)
  ) %>%
  combine_datetime(timezone = tzone) %>%
  # Standardize Station names
  mutate(
    Station = case_match(
      Station,
      "Turning Basin" ~ "tb",
      "P8" ~ "lt40", # P8 is the same as lt40 according to Sept 2021 DO report
      "Light 41" ~ "lt41",
      "Light 43" ~ "lt43",
      "Light 48" ~ "lt48"
    )
  ) %>%
  # Remove middle depth samples and pivot data wider for parameters collected at top and bottom
  mutate(Sample_depth = case_match(Sample_depth, 1 ~ "top", 6 ~ "bottom")) %>%
  drop_na(Sample_depth) %>%
  pivot_wider(
    names_from = Sample_depth,
    values_from = c(Temperature, Conductivity, pH, TurbidityFNU, DissolvedOxygen)
  ) %>%
  rename_with(\(x) str_remove(x, "_top$"), ends_with("_top"))

# Combine data and finish cleaning
SDO <- bind_rows(df_data_1997_2018, df_data_2021_curr) %>%
  left_join(ls_SDO$df_stations, by = join_by(Station)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  add_update_info(edi_metadata$edi_id)

usethis::use_data(SDO, overwrite = TRUE)
document_helper_edi(edi_metadata$edi_id, SDO)
