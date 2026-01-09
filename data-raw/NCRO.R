# Code to prepare `NCRO` dataset
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "NCRO"

# Define start date for data update - starting earlier than 2023 to get some data not in the
  # original 1999-2022 dataset
start_date <- "2021-09-28"

# Define file path to data-raw/NCRO folder
fp_ncro_data <- "data-raw/NCRO"

# Bring in 1999-2022 data
df_data_1999_2022 <- readRDS(file.path(fp_ncro_data, "ncro_data_1999-2022.rds"))

# Import station metadata and coordinates
df_stations <- read_csv(file.path(fp_ncro_data, "ncro_stations_current.csv"))

# Create vector of Station Numbers for active stations
stations_active <- df_stations %>%
  filter(Active == "Yes") %>%
  pull(StationNumber)

# Download field and lab data to temporary directory
walk(stations_active, \(x) get_cnra_data_field(x, start_date))
walk(stations_active, \(x) get_cnra_data_lab(x, start_date))

# Run standardized workflow to import data and process it
ls_NCRO <- import_proc_data(survey)

# Prepare individual data entities before joining them together
df_data_field <- ls_NCRO$df_data_field %>%
  standardize_param(survey, "Field") %>%
  pivot_wider(names_from = Parameter_std, values_from = Result)

df_data_lab_c1 <- ls_NCRO$df_data_lab %>%
  standardize_param(survey, "Lab") %>%
  # Add Sign variable which indicates <RL values, and convert Result to numeric making <RL values
    # equal to their RL
  mutate(
    Sign = if_else(str_detect(Result, "^<"), "<", "="),
    Result = as.numeric(if_else(Sign == "<", RL, Result)),
    .keep = "unused"
  )

# Before restructuring lab data to wide format, remove laboratory duplicates by removing one at
  # random
# Define grouping variables for each unique duplicate pair
grp_lab_dups <- c("StationNumber", "SampleCode", "Datetime", "Parameter_std")

df_data_lab_dups <- df_data_lab_c1 %>%
  add_count(pick(all_of(grp_lab_dups))) %>%
  filter(n > 1) %>%
  select(-n) %>%
  slice_sample(n = 1, by = all_of(grp_lab_dups))

df_data_lab_c2 <- df_data_lab_c1 %>%
  anti_join(
    df_data_lab_dups %>% select(all_of(grp_lab_dups)),
    by = grp_lab_dups
  ) %>%
  bind_rows(df_data_lab_dups) %>%
  pivot_wider(
    names_from = Parameter_std,
    values_from = c(Result, Sign),
    names_glue = "{Parameter_std}_{.value}"
  ) %>%
  # Clean up names for the columns with results
  rename_with(\(x) str_remove(x, "_Result$"))

# Prepare Secchi depth and Microcystis data to be joined with field and laboratory data
df_data_MVI_Secchi_c <- ls_NCRO$df_data_MVI_Secchi %>%
  mutate(
    # Dates and times are converted to numeric values when importing Excel files with text column
      # type - Use just the Date since the DateTimes don't completely match with the field/lab data
    Date = as_date(floor(Datetime), origin = "1899-12-30"),
    # Use the numeric codes for Microcystis
    Microcystis = case_match(
      Microcystis,
      c("Not Visible", "Absent") ~ 1,
      "Low" ~ 2,
      "Medium" ~ 3,
      "High" ~ 4,
      "Extreme" ~ 5
    ),
    .keep = "unused"
  ) %>%
  # Standardize the Paradise Cut upstream station to PDU (code is PDUP before 2024)
  mutate(StationCode = if_else(StationCode == "PDUP", "PDU", StationCode)) %>%
  # Join standardized station numbers
  left_join(
    df_stations %>% select(StationNumber, WQES_StationCode),
    by = join_by(StationCode == WQES_StationCode)
  ) %>%
  # Remove Stations and Dates that are NA and records without Secchi and Microcystis values
  drop_na(StationNumber, Date) %>%
  filter(!if_all(c(Secchi, Microcystis), is.na)) %>%
  select(StationNumber, Date, Secchi, Microcystis) %>%
  # Remove a few duplicated records
  distinct()

# Combine field and laboratory data
df_data_curr <-
  full_join(
    df_data_field, df_data_lab_c2,
    by = join_by(StationNumber, SampleCode, Date, Datetime)
  ) %>%
  # Remove samples not collected by NCRO at YB below Lisbon station (Sample Code prefix 'ES')
  filter(!str_detect(SampleCode, "^ES")) %>%
  # Add Secchi depth and Microcystis data - there a few records without matches in the field/lab
    # data, but we'll use a left join for now
  left_join(df_data_MVI_Secchi_c, by = join_by(StationNumber, Date)) %>%
  # Add standardized station names and lat-long coordinates
  left_join(
    df_stations %>% select(StationNumber, Station, Latitude, Longitude),
    by = join_by(StationNumber)
  ) %>%
  # Fill in "=" for NA values within the _Sign variables
  mutate(across(ends_with("_Sign"), \(x) if_else(is.na(x), "=", x))) %>%
  # Add Source column
  add_source_col(survey)

# Add current data to 1999-2022 data
NCRO <- bind_rows(df_data_1999_2022, df_data_curr) %>%
  # Remove overlapping data - this won't be an issue in the future
  distinct(Station, Datetime, .keep_all = TRUE) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Datetime) %>%
  add_update_info()

usethis::use_data(NCRO, overwrite = TRUE)

# Save final data set to use in future updates
NCRO %>% saveRDS(file.path(fp_ncro_data, "ncro_data_1999-curr.rds"))

document_helper_other(NCRO)
