# Code to prepare `SDO` dataset
library(dplyr)
library(lubridate)
library(hms)
library(stringr)
library(tidyr)
library(purrr)

# Source helper functions
source("R/data_raw_helpers.R")

# Define settings for dataset
survey <- "SDO"
edi_pack_id <- 276

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Currently, SDO data from 1997-2018 is available on EDI (edi.276.2). Data collected from 2021-2024
# was provided by Julianna Manning (DWR-EMP) on 5/8/2025. EMP is working on publishing their more
# recent data on EDI.

# Define data entities for download from EDI and their regex patterns
ent_regex <- c(
  "df_stations" = "IEP_DOSDWSC_site_locations",
  "df_data_1997_2018" = "IEP_DOSDWSC_1997_2018"
)

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data(edi_id_latest, ent_regex)

# Run standardized workflow to import data from EDI and process it
ls_SDO <- edi_fp %>%
  map(import_raw_data) %>%
  map2(
    map(names(.), \(x) import_col_meta(survey, x)),
    standardize_col_meta
  )

# Import data collected in 2021-2024 by DWR-EMP provided in excel files
df_data_2021_curr <-
  list.files(
    "data-raw/SDO",
    pattern = "DiscreteDORunData\\d{4}\\.xlsx$",
    full.names = TRUE
  ) %>%
  map(\(x) import_raw_data(x, "read_excel")) %>%
  map(\(x) {
    standardize_col_meta(x, import_col_meta(survey, "df_data_2021_curr"))
  }) %>%
  list_rbind()

# Prepare tables before binding them together
df_data_1997_2018 <- ls_SDO$df_data_1997_2018 %>%
  convert_date("mdY") %>%
  mutate(
    # Fix one erroneous time recorded as "111" which should probably be "1111"
    Time = if_else(Time == "111", "1111", Time),
    Time = str_pad(Time, width = 4, side = "left", pad = "0")
  ) %>%
  combine_datetime(dt_fmt = "Ymd HM", timezone = "Etc/GMT+8") %>%
  convert_secchi("cm") %>%
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
    Date = as_date(Date, origin = "1899-12-30"),
    Time = as_hms(Time * 60 * 60 * 24)
  ) %>%
  combine_datetime(timezone = "Etc/GMT+8") %>%
  convert_depth("feet") %>%
  convert_secchi("cm") %>%
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
    values_from = c(
      Temperature,
      Conductivity,
      pH,
      TurbidityFNU,
      DissolvedOxygen
    )
  ) %>%
  rename_with(\(x) str_remove(x, "_top$"), ends_with("_top"))

# Combine data and finish cleaning
SDO <- bind_rows(df_data_1997_2018, df_data_2021_curr_c) %>%
  left_join(ls_SDO$df_stations, by = join_by(Station)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  add_update_info("edi.276.2")

usethis::use_data(SDO, overwrite = TRUE)
document_helper_edi("edi.276.2", SDO)
