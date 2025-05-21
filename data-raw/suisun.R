# Code to prepare `suisun` dataset
library(dplyr)
library(tidyr)
library(stringr)

# Function to calculate mode of Tide values
# Modified from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode to
  # remove missing values
Mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

source("data-raw/01_Global/data_raw_helpers.R")

survey <- "Suisun"

# Run standardized workflow to import data from EDI and process it
ls_suisun <- import_proc_edi_data(survey)

# Prepare tables before joining them together
df_stations <- ls_suisun$df_stations %>% drop_na()

df_depth <- ls_suisun$df_depth %>%
  # Use the average depth for each sample
  summarize(Depth = mean(Depth, na.rm = TRUE), .by = SampleRowID)

# Join tables together and finish cleaning data
# Notes:
  # 1) Not including salinity because data do not correspond well with conductivity
  # 2) Some water quality measurements may be copied and pasted if 2 fish samples were close in time
    # and space
suisun <- ls_suisun$df_sample %>%
  # Parse Date and Time columns
  mutate(
    Date = str_extract(Date, ".+(?=\\s)"),
    Time = str_extract(Time, "(?<=\\s).+")
  ) %>%
  convert_datetime(date_format = "mdY", time_format = "HMS", timezone = "America/Los_Angeles") %>%
  # Specific conductivity calculated from electrical conductivity using formula from
    # https://pubs.usgs.gov/tm/09/a6.3/tm9-a6_3.pdf and alpha constant from
    # https://www.mt.com/dam/MT-NA/pHCareCenter/Conductivity_Linear_Temp_Comensation_APN.pdf
  # Electrical conductivity values less than 100 are excluded based on range of specific
    # conductance values
  mutate(
    ElecCond = if_else(ElecCond < 100, NA_real_, ElecCond),
    Conductivity = if_else(
      is.na(Conductivity),
      ElecCond / (1 + 0.019 * (Temperature - 25)),
      Conductivity
    )
  ) %>%
  # Standardize tide codes
  mutate(
    Tide = case_match(
      Tide,
      c("flood", "incoming") ~ "Flood",
      c("ebb", "outgoing") ~ "Ebb",
      "low" ~ "Low Slack",
      "high" ~ "High Slack"
    )
  ) %>%
  # Add depth values
  left_join(df_depth, by = join_by(SampleRowID)) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Remove replicate tows with identical WQ values - select earliest Datetime and most common Tide
    # value. Average the depth values for each group - most of the time there is only one value per
    # group.
  summarize(
    Datetime = min(Datetime),
    Depth = mean(Depth, na.rm = TRUE),
    Tide = Mode(Tide),
    .by = c(
      Station, Date, Temperature, DissolvedOxygen, DissolvedOxygenPercent, Secchi, Conductivity
    )
  ) %>%
  # Replace NaN values in Depth column with NA
  mutate(Depth = na_if(Depth, NaN)) %>%
  # Remove replicate tows with the same Station and Datetime, but different WQ values - keep rows
    # with Depth and Tide values
  arrange(Depth) %>%
  distinct(Station, Datetime, .keep_all = TRUE) %>%
  left_join(df_stations, by = join_by(Station)) %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Datetime)

usethis::use_data(suisun, overwrite = TRUE)

document_helper_edi(ls_suisun$edi_id, suisun)
update_edi_metadata(survey, ls_suisun$edi_id)
