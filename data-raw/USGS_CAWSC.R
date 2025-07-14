# Code to prepare `USGS_CAWSC` dataset
library(dplyr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for datasets
survey <- "USGS_CAWSC"
date_fmt <- "Ymd"
time_fmt <- "HMS"
tzone <- "America/Los_Angeles"

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

parameters <- c(
  "Total ammonia (NH4+ and NH3) as nitrogen, water, filtered",
  "Nitrate plus nitrite as nitrogen, water, filtered",
  "Orthophosphate as phosphorus, water, filtered",
  "Dissolved organic carbon (DOC), water, filtered",
  "Chlorophyll a (Chl-a), phytoplankton in water, chromatographic-fluorometric method",
  "Dissolved oxygen (DO), water, unfiltered",
  "pH, water, unfiltered, field",
  "Temperature, water",
  "Specific conductance, water, unfiltered, normalized to 25 degrees Celsius"
)

# Download data to temporary directory
walk(site_ids, \(x) get_usgs_samples_data(x, parameters))

# Determine file paths for data on temporary directory
temp_files <- list.files(tempdir(), full.names = TRUE, pattern = "_usgs_samples_data\\.csv")

# Import data and perform checks and minor processing
df_data <-
  map(temp_files, \(x) import_raw_data(x, survey, "df_data")) %>%
  list_rbind()

# Finish preparing the data
USGS_CAWSC <- df_data %>%
  # Correct a few mislabeled units for water temperature
  mutate(
    Units = if_else(Parameter == "Temperature, water" & Units == "deg F", "deg C", Units)
  ) %>%
  # Standardize parameter names
  standardize_param(survey, "Both") %>%
  # Remove the one DO record that is "Present Above Quantification Limit" since this is the only
    # instance of this
  filter(
    Detection_Condition != "Present Above Quantification Limit" | is.na(Detection_Condition)
  ) %>%
  mutate(
    # Create sign column for 'estimated' and 'less than' reporting limit values
    Sign = case_when(
      Detection_Condition == "Not Detected" ~ "<",
      Result_Type == "Estimated" ~ "~",
      .default = "="
    ),
    # Convert Result to numeric making <RL values equal to their RL
    Result = as.numeric(if_else(Sign == "<", RL, Result))
  ) %>%
  convert_datetime(date_fmt, time_fmt, tzone) %>%
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
  add_source_col(survey) %>%
  standardize_col_order() %>%
  # Fill in "=" for the NA values in the _Sign variables
  mutate(across(ends_with("_Sign"), ~ if_else(is.na(.x), "=", .x))) %>%
  arrange(Date, Station)

usethis::use_data(USGS_CAWSC, overwrite = TRUE)

document_helper_other(USGS_CAWSC)
