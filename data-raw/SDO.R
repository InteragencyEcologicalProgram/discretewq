# Code to prepare `SDO` dataset
library(dplyr)
library(purrr)
library(readxl)
library(lubridate)
library(hms)
library(stringr)
library(tidyr)

source("data-raw/01_Global/data_raw_helpers.R")

survey <- "SDO"

# Run standardized workflow to import data from EDI and process it - this is for data collected from
  # 1997-2018. Using static = TRUE because this data package isn't being updated with additional data
ls_SDO <- import_proc_edi_data(survey, static = TRUE)

# Import data collected in 2021-2024 by DWR-EMP (data provided by Julianna Manning on 5/8/2025)
fp_SDO <- "data-raw/SDO"

# Each year is in a separate Excel file
files_SDO <- list.files(fp_SDO, pattern = "DiscreteDORunData\\d{4}\\.xlsx$", full.names = TRUE)

df_data_2021_curr <-
  map(
    files_SDO,
    \(x) import_raw_data(
      x, survey, "df_data_2021_curr",
      import_fun = read_excel, sheet = "AllData", col_types = "text"
    )
  ) %>%
  list_rbind()

# Prepare tables before joining them together
df_data_1997_2018 <- ls_SDO$df_data_1997_2018 %>%
  mutate(
    # Fix one erroneous time recorded as "111" which should probably be "1111"
    Time = if_else(Time == "111", "1111", Time),
    Time = str_pad(Time, width = 4, side = "left", pad = "0")
  ) %>%
  convert_datetime(date_format = "mdY", time_format = "HM", timezone = "Etc/GMT+8") %>%
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

df_data_2021_curr_c <- df_data_2021_curr %>%
  # Dates and times are converted to numeric values when importing Excel files with text column type
  mutate(
    Date = as_date(as.numeric(Date), origin = "1899-12-30"),
    Time = as_hms(as.numeric(Time) * 60 * 60 * 24)
  ) %>%
  convert_datetime(date_format = "Ymd", time_format = "HMS", timezone = "Etc/GMT+8") %>%
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
  rename_with(\(x) str_remove(x, "_top$"), ends_with("_top")) %>%
  # Convert Depth from feet to meters
  convert_depth(depth_unit = "feet")

# Combine data and finish cleaning
SDO <- bind_rows(df_data_1997_2018, df_data_2021_curr_c) %>%
  left_join(ls_SDO$df_stations, by = join_by(Station)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  standardize_col_order()

usethis::use_data(SDO, overwrite = TRUE)

document_helper_edi(ls_SDO$edi_id, SDO)
