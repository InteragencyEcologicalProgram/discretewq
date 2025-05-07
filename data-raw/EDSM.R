# Code to prepare `EDSM` dataset
library(dplyr)

source("data-raw/01_Global/data_raw_helpers.R")

survey <- "EDSM"

# Run standardized workflow to import data from EDI and process it
ls_EDSM <- import_proc_edi_data(survey)

# Combine and finish cleaning data
EDSM <-
  bind_rows(ls_EDSM$df_data_20mm, ls_EDSM$df_data_KDTR) %>%
  mutate(
    # Standardize tide codes
    Tide = recode(Tide, HS = "High Slack", LS = "Low Slack"),
    Station = paste(Station, Date),
    Field_coords = TRUE,
    # Remove conductivity data from dates before it was standardized >
    # Methods in metadata say they do not know if their data were corrected for temperature before
    # May 3 or 17 2019 so we will not use conductivity data before June 2019
    Conductivity = if_else(Date < "2019-06-01", NA_real_, Conductivity)
  ) %>%
  # Remove replicate samples with the same Datetime and location. This keeps the first row. This
  # results in no more NA values in water quality variables than if we had used summarize(mean(.x,
  # na.rm = T))
  distinct(Station, Latitude, Longitude, Date, Datetime, .keep_all = TRUE) %>%
  # Remove rows where all measurements are NA
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Datetime)

usethis::use_data(EDSM, overwrite = TRUE)

document_helper_edi(ls_EDSM$edi_id, EDSM)
update_edi_metadata(survey, ls_EDSM$edi_id)
