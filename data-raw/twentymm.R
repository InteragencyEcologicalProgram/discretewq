# Code to prepare `twentymm` dataset
library(dplyr)
library(lubridate)
library(purrr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("R/data_raw_helpers.R")

# Define settings for dataset
survey <- "20mm"
edi_pack_id <- 535

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Define data entities for download from EDI and their regex patterns
ent_regex <- c(
  "df_stations" = "20mmStations",
  "df_data" = "^Station$",
  "df_survey" = "^Survey$",
  "df_tow" = "^Tow$"
)

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data(edi_id_latest, ent_regex)

# Run standardized workflow to import data and process it
ls_20mm <- edi_fp %>%
  map(import_raw_data) %>%
  map2(
    map(names(.), \(x) import_col_meta(survey, x)),
    standardize_col_meta
  )

# Prepare tables before joining them together
df_stations <- convert_lat_long(ls_20mm$df_stations, coord_comp = "DMS")
df_survey <- convert_date(ls_20mm$df_survey, "Ymd")

df_tow <- ls_20mm$df_tow %>%
  convert_time("Ymd HMS") %>%
  convert_depth("feet") %>%
  standardize_tide_code()

# Join data entities and finish cleaning data
twentymm <- ls_20mm$df_data %>%
  convert_secchi("cm") %>%
  convert_lat_long(coord_comp = "DMS") %>%
  left_join(df_survey, by = join_by(SurveyID)) %>%
  left_join(df_tow, by = join_by(StationID)) %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = "America/Los_Angeles") %>%
  # Correct a few erroneous times (most likely not recorded in military time format)
  mutate(Datetime = if_else(hour(Datetime) %in% 1:2, Datetime + hours(12), Datetime)) %>%
  # Only keep info for the first tow of each day (first defined by time then by tow number for ties)
  group_by(StationID) %>% # StationID really is SampleID
  filter(Datetime == min(Datetime, na.rm = TRUE)) %>%
  filter(TowNum == min(TowNum)) %>%
  ungroup() %>%
  # Add station coordinates
  left_join(df_stations, by = join_by(Station), suffix = c("_field", "")) %>%
  resolve_lat_long() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Datetime) %>%
  add_update_info(edi_id_latest)

usethis::use_data(twentymm, overwrite = TRUE)
document_helper_edi(edi_id_latest, twentymm)
