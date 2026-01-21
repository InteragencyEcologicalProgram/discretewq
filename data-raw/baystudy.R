# Code to prepare `baystudy` dataset
library(dplyr)
library(purrr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "Baystudy"
edi_pack_id <- 2143

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Define data entities for download from EDI and their regex patterns
ent_regex <- c(
  "df_stations" = "StationConstants",
  "df_data_station" = "BoatStation",
  "df_data_tow" = "BoatTow"
)

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data(edi_id_latest, ent_regex)

# Run standardized workflow to import data and process it
ls_baystudy <- edi_fp %>%
  map(import_raw_data) %>%
  map2(
    map(names(.), \(x) import_col_meta(survey, x)),
    standardize_col_meta
  )

# Prepare tables before joining them together
df_data_tow <- ls_baystudy$df_data_tow %>%
  convert_date("Ymd") %>%
  convert_time("HM") %>%
  rename(Tide_tow = Tide)

df_data_station <- ls_baystudy$df_data_station %>%
  convert_date("Ymd") %>%
  convert_depth("meters") %>%
  convert_secchi("cm") %>%
  rename(Tide_station = Tide)

# Join tables together and finish cleaning data
baystudy <-
  full_join(df_data_station, df_data_tow, by = join_by(Station, Date)) %>%
  combine_datetime(timezone = "America/Los_Angeles") %>%
  # Just keep tide and time-of-day records at the time of the first tow while
  # retaining records without time data
  group_by(Station, Date) %>%
  filter(Datetime == min(Datetime) | is.na(Datetime)) %>%
  ungroup() %>%
  # Resolve lat-long coordinates for stations
  left_join(
    ls_baystudy$df_stations,
    by = join_by(Station),
    suffix = c("_field", "")
  ) %>%
  resolve_lat_long() %>%
  # If tide data were not collected at the time of the tow, use the value from the overall station
  # visit, also standardize the tide codes
  mutate(
    Tide = if_else(is.na(Tide_tow), Tide_station, Tide_tow),
    Tide = case_match(
      Tide,
      1 ~ "Flood",
      2 ~ "Ebb",
      3 ~ "Low Slack",
      4 ~ "High Slack"
    ),
    .keep = "unused"
  ) %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove a few duplicated records
  distinct() %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Date, Station) %>%
  add_update_info(edi_id_latest)

usethis::use_data(baystudy, overwrite = TRUE)
document_helper_edi(edi_id_latest, baystudy)
