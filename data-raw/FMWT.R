# Code to prepare `FMWT` dataset
library(dplyr)
library(lubridate)
library(stringr)

source("data-raw/01_Global/data_raw_helpers.R")

survey <- "FMWT"

# Run standardized workflow to import data from EDI and process it
ls_FMWT <- import_proc_edi_data(survey)

# Import sample table from FMWT database hosted on FTP site to add in Secchi_estimated info
# This info isn't in the EDI data publication
df_sample <- import_raw_data("data-raw/FMWT/Sample.csv", survey, "df_sample")

# Prepare sample table to be joined with data table
df_sample_c <- df_sample %>%
  mutate(
    Date = mdy(str_extract(Date, ".+(?=\\s)")),
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
  mutate(
    # Standardize stations to 3 digit codes
    Station = if_else(str_length(Station) == 2, paste0("0", Station), Station),
    # Standardize tide codes
    Tide = case_match(Tide, 4 ~ "Flood", 3 ~ "Low Slack", 2 ~ "Ebb", 1 ~ "High Slack"),
    # Reassign some Microcystis values
    Microcystis = if_else(Microcystis == 6, 2, Microcystis)
  ) %>%
  # Add Secchi_estimated
  left_join(df_sample_c, by = join_by(Station, Date)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Date, Station)

usethis::use_data(FMWT, overwrite = TRUE)

document_helper_edi(ls_FMWT$edi_id, FMWT)
update_edi_metadata(survey, ls_FMWT$edi_id)
