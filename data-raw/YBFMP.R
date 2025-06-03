# Code to prepare `YBFMP` dataset
library(dplyr)
library(stringr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for datasets
survey <- "YBFMP"

date_fmt_fish <- "mdY"
time_tmt_fish <- "HM"
tzone_fish <- "America/Los_Angeles"
secchi_unit_fish <- "meters"

date_fmt_zoop <- "Ymd"
time_tmt_zoop <- "HMS"
tzone_zoop <- "America/Los_Angeles"
secchi_unit_zoop <- "meters"

# Run standardized workflow to import data from EDI and process it
# Fish and zooplankton data sets are in separate EDI publications
# Using static = TRUE for the zooplankton data because this data package hasn't been updated with
  # additional data since Dec 2021
ls_YBFMP_fish <- import_proc_edi_data("YBFMP_fish")
ls_YBFMP_zoop <- import_proc_edi_data("YBFMP_zoop", static = TRUE)

# Prepare tables before binding them together
df_data_fish <- ls_YBFMP_fish$df_data %>%
  convert_datetime(date_fmt_fish, time_tmt_fish, tzone_fish) %>%
  mutate(
    # Standardize tide codes
    Tide = case_match(
      Tide,
      "High" ~ "High Slack",
      "Low" ~ "Low Slack",
      "OB" ~ "Overtopping",
      "slack" ~ "Slack",
      .default = Tide
    ),
    # Resolve Turbidity measurements - NTU before Oct 2016, FNU afterwards
    TurbidityNTU = if_else(Date < "2016-10-01", Turbidity, NA_real_),
    TurbidityFNU = if_else(Date >= "2016-10-01", Turbidity, NA_real_),
    # Correct a few misspelled station names
    Station = if_else(Station == "YB180", "YBI80", Station)
  ) %>%
  select(-Turbidity) %>%
  convert_secchi(secchi_unit_fish) %>%
  left_join(ls_YBFMP_fish$df_stations, by = join_by(Station))

df_data_zoop <- ls_YBFMP_zoop$df_data %>%
  convert_datetime(date_fmt_zoop, time_tmt_zoop, tzone_zoop) %>%
  # Standardize tide codes
  mutate(Tide = case_match(Tide, "High" ~ "High Slack", "Low" ~ "Low Slack",.default = Tide)) %>%
  # Resolve Turbidity measurements - NTU before late Oct 2016, FNU afterwards (impossible to
    # determine exact time for late Oct so we'll use Oct 15th as the cutoff)
  mutate(
    TurbidityNTU = if_else(Date < "2016-10-15", Turbidity, NA_real_),
    TurbidityFNU = if_else(Date >= "2016-10-15", Turbidity, NA_real_)
  ) %>%
  select(-Turbidity) %>%
  convert_secchi(secchi_unit_zoop) %>%
  left_join(ls_YBFMP_zoop$df_stations, by = join_by(Station))

# Combine data and finish cleaning
df_data_all <- bind_rows(df_data_fish, df_data_zoop) %>%
  # Convert Electric Conductivity to Specific Conductance, see suisun.R for info
  mutate(
    Conductivity = if_else(
      is.na(Conductivity),
      ElecConductivity / (1 + 0.019 * (Temperature - 25)),
      Conductivity
    )
  ) %>%
  select(-ElecConductivity) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  standardize_col_order()

# Define grouping columns for eliminating duplicates in dataset
grp_dupl_all <- str_subset(names(df_data_all), "Notes", negate = TRUE)
grp_dupl_same_day <- grp_dupl_all[grp_dupl_all != "Datetime"]

# Clean up the duplicated records
df_data_all_c <- df_data_all %>%
  # Remove duplicated records across all columns
  distinct() %>%
  # Remove duplicated records due to different values in the Notes column
  arrange(Notes) %>%
  distinct(pick(all_of(grp_dupl_all)), .keep_all = TRUE) %>%
  # Remove a duplicated record because of an NA value in the Secchi column - the other WQ
    # measurements are the same
  arrange(Secchi) %>%
  distinct(pick(all_of(grp_dupl_all) & !contains("Secchi")), .keep_all = TRUE) %>%
  # There are numerous records that were collected on the same day and station that have identical
    # water quality measurements. After speaking with Nicole Kwan (former SES Supervisor for the AES
    # Unit), we decided to only keep the records with the earliest Datetime for the groups of records
    # that share identical water quality measurements with different Datetimes.
  group_by(pick(all_of(grp_dupl_same_day))) %>%
  filter(Datetime == min(Datetime)) %>%
  ungroup() %>%
  # Remove duplicated records because of NA values in the Tide and Microcystis columns - the other
    # WQ measurements are the same
  arrange(Tide) %>%
  distinct(pick(all_of(grp_dupl_same_day) & !contains("Tide")), .keep_all = TRUE) %>%
  arrange(Microcystis) %>%
  distinct(pick(all_of(grp_dupl_same_day) & !contains("Microcystis")), .keep_all = TRUE)

# Clean up the remaining duplicated records - records have different water quality measurements but
  # same Datetime (one is from the zoop and the other is from the fish data set) - average the water
  # quality values
wq_meas <- c(
  "Microcystis", "Secchi", "Temperature", "Conductivity",
  "DissolvedOxygen", "pH", "TurbidityNTU", "TurbidityFNU"
)

df_data_all_dups <- df_data_all_c %>%
  group_by(Station, Datetime) %>%
  add_tally() %>%
  filter(n > 1) %>%
  mutate(
    across(all_of(wq_meas), \(x) mean(x, na.rm = TRUE)),
    Secchi = round(Secchi),
    row_num = row_number()
  ) %>%
  ungroup() %>%
  filter(row_num == 1) %>%
  select(-c(n, row_num)) %>%
  # Convert NaN values to NA
  mutate(across(all_of(wq_meas), \(x) na_if(x, NaN)))

YBFMP <- df_data_all_c %>%
  anti_join(df_data_all_dups, by = join_by(Station, Datetime)) %>%
  bind_rows(df_data_all_dups) %>%
  arrange(Datetime)

usethis::use_data(YBFMP, overwrite = TRUE)

document_helper_edi(ls_YBFMP_fish$edi_id, YBFMP)
document_helper_edi(ls_YBFMP_zoop$edi_id, YBFMP)
update_edi_metadata("YBFMP_fish", ls_YBFMP_fish$edi_id)
update_edi_metadata("YBFMP_zoop", ls_YBFMP_zoop$edi_id)
