# Code to prepare `USGS_CAWSC` dataset
library(dplyr)
library(purrr)
library(tidyr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("R/data_raw_helpers.R")

# Define settings for dataset
survey <- "USGS_CAWSC"

# Identify stations and parameters of interest
site_ids <- c(
  "USGS-11303500",
  "USGS-11304810",
  "USGS-11311300",
  "USGS-11312672",
  "USGS-11312676",
  "USGS-11312685",
  "USGS-11312968",
  "USGS-11313240",
  "USGS-11313315",
  "USGS-11313405",
  "USGS-11313431",
  "USGS-11313433",
  "USGS-11313434",
  "USGS-11313440",
  "USGS-11313452",
  "USGS-11313460",
  "USGS-11336600",
  "USGS-11336680",
  "USGS-11336685",
  "USGS-11336790",
  "USGS-11336930",
  "USGS-11337080",
  "USGS-11337190",
  "USGS-11447650",
  "USGS-11447830",
  "USGS-11447850",
  "USGS-11447890",
  "USGS-11447903",
  "USGS-11447905",
  "USGS-11455095",
  "USGS-11455136",
  "USGS-11455139",
  "USGS-11455140",
  "USGS-11455142",
  "USGS-11455143",
  "USGS-11455146",
  "USGS-11455165",
  "USGS-11455166",
  "USGS-11455167",
  "USGS-11455276",
  "USGS-11455280",
  "USGS-11455315",
  "USGS-11455335",
  "USGS-11455338",
  "USGS-11455350",
  "USGS-11455385",
  "USGS-11455420",
  "USGS-11455478",
  "USGS-11455485",
  "USGS-11455508",
  "USGS-380631122032201",
  "USGS-380833122033401",
  "USGS-381142122015801",
  "USGS-381424121405601",
  "USGS-381614121415301",
  "USGS-382006121401601",
  "USGS-382010121402301",
  "USGS-383019121350701",
  "USGS-381944121405201"
)

# Download data to temporary directory
walk(site_ids, get_usgs_samples_data)

# Run standardized workflow to import data and process it
df_USGS_CAWSC <-
  list.files(tempdir(), pattern = "usgs_samples_data", full.names = TRUE) %>%
  map(import_raw_data) %>%
  map(\(x) standardize_col_meta(x, import_col_meta(survey, "df_data"))) %>%
  map(\(x) convert_date(x, "Ymd")) %>%
  map(\(x) convert_time(x, "HMS")) %>%
  list_rbind()

# Finish preparing the data
USGS_CAWSC <- df_USGS_CAWSC %>%
  # Correct a few mislabeled units for water temperature
  mutate(
    Units = if_else(
      Parameter == "Temperature, water" & Units == "deg F",
      "deg C",
      Units
    )
  ) %>%
  # Standardize parameter names
  standardize_param(import_param_meta(survey, "Both")) %>%
  # Remove the one DO record that is "Present Above Quantification Limit" since this is the only
  # instance of this
  filter(
    Detection_Condition != "Present Above Quantification Limit" |
      is.na(Detection_Condition)
  ) %>%
  mutate(
    # Create sign column for 'estimated' and 'less than' reporting limit values
    Sign = case_when(
      Detection_Condition == "Not Detected" ~ "<",
      Result_Type == "Estimated" ~ "~",
      .default = "="
    ),
    # Make <RL values equal to their RL
    Result = as.numeric(if_else(Sign == "<", RL, Result))
  ) %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = "America/Los_Angeles") %>%
  # Remove three Results equal to zero
  filter(Result != 0) %>%
  # Restructure data to wide format
  select(-c(Result_Type, Detection_Condition, RL)) %>%
  distinct() %>%
  pivot_wider(
    names_from = Parameter_std,
    values_from = c(Result, Sign),
    names_glue = "{Parameter_std}_{.value}",
    # choose first value for duplicates
    values_fn = first
  ) %>%
  rename_with(\(x) str_remove(x, "_Result$")) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  # Fill in "=" for the NA values in the _Sign variables
  mutate(across(ends_with("_Sign"), ~ if_else(is.na(.x), "=", .x))) %>%
  arrange(Date, Station) %>%
  add_update_info()

usethis::use_data(USGS_CAWSC, overwrite = TRUE)
document_helper_other(USGS_CAWSC)
