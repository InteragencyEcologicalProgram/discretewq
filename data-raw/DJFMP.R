# Code to prepare `DJFMP` dataset
library(dplyr)
library(purrr)

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "DJFMP"
edi_pack_id <- 244

# Obtain current dataset information
current_info <- get_update_info(survey)

# Check for revisions to the EDI data set
edi_id_latest <- get_latest_edi_id(edi_pack_id, current_info$edi_id)

# Define data entities for download from EDI and their regex patterns
ent_regex <- c(
  "df_stations" = "Site_Locations",
  "df_data_1976_2001" = "1976.+DJFMP_trawl",
  "df_data_2002_curr" = "2002.+DJFMP_trawl"
)

# Download EDI data to temporary directory if necessary, while saving the file paths
# to the data entities
edi_fp <- get_edi_data(edi_id_latest, ent_regex)

# Run standardized workflow to import data and process it
ls_DJFMP <- edi_fp %>%
  map(import_raw_data) %>%
  map2(
    map(names(.), \(x) import_col_meta(survey, x)),
    standardize_col_meta
  )

# Prepare tables before joining them together
df_data_1976_2001 <- ls_DJFMP$df_data_1976_2001 %>%
  convert_date("mdY") %>%
  convert_time("HMS") %>%
  combine_datetime(timezone = "America/Los_Angeles") %>%
  convert_secchi("meters")

df_data_2002_curr <- ls_DJFMP$df_data_2002_curr %>%
  convert_date("mdY") %>%
  convert_time("HMS") %>%
  combine_datetime(timezone = "America/Los_Angeles") %>%
  convert_secchi("meters")

# Combine and finish cleaning data
DJFMP <- bind_rows(df_data_1976_2001, df_data_2002_curr) %>%
  # Remove conductivity data from dates before it was standardized >
  # Methods in metadata say they do not know if their data were corrected for
  # temperature before May 3 or 17 2019 so we will not use conductivity data before
  # June 2019
  mutate(
    Conductivity = if_else(Date < "2019-06-01", NA_real_, Conductivity)
  ) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove duplicated rows
  distinct(Station, Datetime, .keep_all = TRUE) %>%
  # Add station coordinates
  left_join(ls_DJFMP$df_stations, by = join_by(Station)) %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Datetime) %>%
  add_update_info(edi_id_latest)

usethis::use_data(DJFMP, overwrite = TRUE)
document_helper_edi(edi_id_latest, DJFMP)
