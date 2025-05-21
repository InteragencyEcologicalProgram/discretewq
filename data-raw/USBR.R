# Code to prepare `USBR` dataset
library(dplyr)
library(stringr)
library(tidyr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

survey <- "USBR"

# Import data tables
fp_usbr <- "data-raw/USBR"

df_stations <- import_raw_data(file.path(fp_usbr, "USBRSiteLocations.csv"), survey, "df_stations")
df_data <- import_raw_data(file.path(fp_usbr, "YSILongTermSites_AllDepths.csv"), survey, "df_data")

# Prepare station table before joining it to data table
df_stations_c <- df_stations %>%
  mutate(
    Station = str_remove(Station, "NL "),
    Station = if_else(Station == "PS", "Pro", Station)
  )

# Join data entities and finish cleaning data
USBR <- df_data %>%
  convert_datetime(date_format = "Ymd", time_format = "HMS", timezone = "America/Los_Angeles") %>%
  group_by(Station, Date) %>%
  mutate(
    # Categorize sample depths
    Depth_bin = case_when(
      Sample_depth == min(Sample_depth) & Sample_depth < 3 ~ "surface",
      Sample_depth == max(Sample_depth) & Sample_depth > 3 ~ "bottom",
      TRUE ~ NA_character_
    ),
    # Keep average sample time across all depths
    Datetime = mean(Datetime)
  ) %>%
  ungroup() %>%
  # Remove samples not collected at either the surface or bottom
  drop_na(Depth_bin) %>%
  # Remove a couple of duplicate rows before pivoting data wider on sample depth
  distinct() %>%
  pivot_wider(
    names_from = Depth_bin,
    values_from = c(Sample_depth, Conductivity, Chlorophyll, Temperature)
  ) %>%
  # Rename surface columns to standardized names
  rename_with(\(x) str_remove(x, "_surface$"), ends_with("_surface") & !starts_with("Sample")) %>%
  # Convert sample depths to meters
  mutate(across(starts_with("Sample_depth"), \(x) x * 0.3048)) %>%
  left_join(df_stations_c, by = join_by(Station)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Datetime)

usethis::use_data(USBR, overwrite = TRUE)

document_helper_other(USBR)
