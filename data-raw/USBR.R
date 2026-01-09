# Code to prepare `USBR` dataset
library(dplyr)
library(stringr)
library(tidyr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "USBR"

# Run standardized workflow to import data and process it
ls_USBR <- import_proc_data(survey)

# Prepare station table before joining it to data table
df_stations <- ls_USBR$df_stations %>%
  mutate(
    Station = str_remove(Station, "NL "),
    Station = if_else(Station == "PS", "Pro", Station)
  )

# Join data entities and finish cleaning data
USBR <- ls_USBR$df_data %>%
  group_by(Station, Date) %>%
  # Categorize sample depths and average sample time across all depths
  mutate(
    Depth_bin = case_when(
      Sample_depth == min(Sample_depth) & Sample_depth < 3 ~ "surface",
      Sample_depth == max(Sample_depth) & Sample_depth > 3 ~ "bottom",
      .default = NA_character_
    ),
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
  # Add station coordinates
  left_join(df_stations, by = join_by(Station)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Datetime) %>%
  add_update_info()

usethis::use_data(USBR, overwrite = TRUE)
document_helper_other(USBR)
