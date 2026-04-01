# Code to prepare `FMWT` dataset
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)

# Source helper functions
source("R/data_raw_helpers.R")

# Define settings for dataset
survey <- "FMWT"
edi_pack_id <- 1951

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Define data entities for download from EDI and their regex patterns
ent_regex <- c("df_data" = "FMWT \\d{4}-\\d{4} Catch Matrix")

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data(edi_id_latest, ent_regex)

# Run standardized workflow to import data and process it
# Workflow includes importing sample table from FMWT database hosted on FTP site to add in
# Secchi_estimated info because this isn't currently available in the EDI data publication
ls_FMWT <-
  c(edi_fp, "df_sample" = "data-raw/FMWT/Sample.csv") %>%
  map(import_raw_data) %>%
  map2(
    map(names(.), \(x) import_col_meta(survey, x)),
    standardize_col_meta
  )

# Prepare sample table to be joined with data table
df_sample_c <- ls_FMWT$df_sample %>%
  convert_date("mdY HMS") %>%
  mutate(
    Secchi_estimated = case_match(Secchi_estimated, 0 ~ FALSE, 1 ~ TRUE)
  ) %>%
  # Remove duplicates
  distinct() %>%
  arrange(desc(Secchi_estimated)) %>%
  distinct(Station, Date, .keep_all = TRUE) %>%
  arrange(Date, Station) %>%
  # Adjust dates to one day later for a couple samples to match with data table correctly
  # Visually confirmed these have the same WQ measurements
  mutate(
    Date = if_else(
      Date == "2021-09-13" & Station %in% c("913", "915"),
      Date + days(1),
      Date
    )
  )

# Finish cleaning up data
FMWT <- ls_FMWT$df_data %>%
  convert_date("Ymd") %>%
  convert_time("HM") %>%
  # Create Datetime column from Date and Time columns
  combine_datetime(timezone = "America/Los_Angeles") %>%
  convert_depth("feet") %>%
  convert_secchi("meters") %>%
  standardize_tide_code() %>%
  mutate(
    # Standardize stations to 3 digit codes
    Station = if_else(str_length(Station) == 2, paste0("0", Station), Station),
    # Reassign some Microcystis values
    Microcystis = if_else(Microcystis == 6, 2, Microcystis)
  ) %>%
  # Add Secchi_estimated
  left_join(df_sample_c, by = join_by(Station, Date)) %>%
  # Resolve field and standard coordinates
  resolve_lat_long() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Date, Station) %>%
  add_update_info(edi_id_latest)

usethis::use_data(FMWT, overwrite = TRUE)
document_helper_edi(edi_id_latest, FMWT)
