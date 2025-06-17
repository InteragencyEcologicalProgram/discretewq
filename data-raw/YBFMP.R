# Code to prepare `YBFMP` dataset
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for datasets
survey <- "YBFMP"

date_fmt_fish <- "mdY"
time_fmt_fish <- "HM"
tzone_fish <- "America/Los_Angeles"
secchi_unit_fish <- "meters"

date_fmt_zoop <- "Ymd"
time_fmt_zoop <- "HMS"
tzone_zoop <- "America/Los_Angeles"
secchi_unit_zoop <- "meters"

# Run standardized workflow to import data from EDI and process it
# Fish and zooplankton data sets are in separate EDI publications
# Using static = TRUE for the zooplankton data because this data package hasn't been updated with
  # additional data since Dec 2021
ls_YBFMP_fish <- import_proc_edi_data("YBFMP_fish")
ls_YBFMP_zoop <- import_proc_edi_data("YBFMP_zoop", static = TRUE)

# Prepare tables before binding them together
# Use a nested dataframe for shared operations
ls_YBFMP <- lst(ls_YBFMP_fish, ls_YBFMP_zoop) %>%
  enframe(name = "Source") %>%
  unnest_wider(value) %>%
  add_column(
    date_fmt = c(date_fmt_fish, date_fmt_zoop),
    time_fmt = c(time_fmt_fish, time_fmt_zoop),
    tzone = c(tzone_fish, tzone_zoop),
    secchi_unit = c(secchi_unit_fish, secchi_unit_zoop)
  ) %>%
  mutate(
    df_data_c = pmap(list(df_data, date_fmt, time_fmt, tzone), convert_datetime),
    df_data_c = map(
      df_data_c,
      \(x) mutate(x,
        # Standardize tide codes
        Tide = case_match(
          Tide,
          "High" ~ "High Slack",
          "Low" ~ "Low Slack",
          "OB" ~ "Overtopping",
          "slack" ~ "Slack",
          .default = Tide
        ),
        # Convert Electric Conductivity to Specific Conductance, see suisun.R for info
        Conductivity = if_else(
          is.na(Conductivity),
          ElecConductivity / (1 + 0.019 * (Temperature - 25)),
          Conductivity
        )
      ) %>%
        select(-ElecConductivity)
    ),
    df_data_c = map2(df_data_c, secchi_unit, convert_secchi),
    df_data_c = map2(df_data_c, df_stations, \(x, y) left_join(x, y, by = join_by(Station))),
    df_data_c = map2(df_data_c, Source, add_source_col),
    # Remove rows where all measurements are NA, if they exist
    df_data_c = map(df_data_c, rm_rows_all_miss_data)
  ) %>%
  select(Source, df_data_c) %>%
  deframe()

# Resolve additional cleaning steps separately for each dataset
df_data_fish <- ls_YBFMP$ls_YBFMP_fish %>%
  mutate(
    # Resolve Turbidity measurements - after talking with AEU staff, NTU before 10/31/2016,
      # uncertain from 10/31/2016 through Nov 2016 (we will make these NA for now), FNU starting in
      # Dec 2016
    TurbidityNTU = if_else(Date < "2016-10-31", Turbidity, NA_real_),
    TurbidityFNU = if_else(Date >= "2016-12-01", Turbidity, NA_real_),
    # Correct a few misspelled station names
    Station = if_else(Station == "YB180", "YBI80", Station)
  ) %>%
  select(-Turbidity)

df_data_zoop <- ls_YBFMP$ls_YBFMP_zoop %>%
  # Resolve Turbidity measurements - NTU before late Oct 2016, FNU afterwards (after talking with
    # AEU staff, we'll use Oct 31st as the cutoff)
  mutate(
    TurbidityNTU = if_else(Date < "2016-10-31", Turbidity, NA_real_),
    TurbidityFNU = if_else(Date >= "2016-10-31", Turbidity, NA_real_),
  ) %>%
  select(-Turbidity) %>%
  # Remove duplicated records across all columns
  distinct()

# Clean up the duplicated records in the fish dataset
# Define grouping columns for eliminating duplicates
grp_dupl_all <- names(df_data_fish)[!(names(df_data_fish) %in% c("Source", "Notes"))]
grp_dupl_same_day <- grp_dupl_all[grp_dupl_all != "Datetime"]

df_data_fish_c <- df_data_fish %>%
  # Remove duplicated records across all columns
  distinct() %>%
  # Remove duplicated records due to different values in the Notes column
  arrange(Notes) %>%
  distinct(pick(all_of(grp_dupl_all)), .keep_all = TRUE) %>%
  # Remove duplicated record because of NA value in the Secchi column - the other WQ measurements
    # are the same
  arrange(Secchi) %>%
  distinct(pick(all_of(grp_dupl_all) & !all_of("Secchi")), .keep_all = TRUE) %>%
  # There are numerous records that were collected on the same day and station that have identical
    # water quality measurements. After speaking with Nicole Kwan (former SES Supervisor for the AES
    # Unit), we decided to only keep the records with the earliest Datetime for the groups of
    # records that share identical water quality measurements with different Datetimes.
  group_by(pick(all_of(grp_dupl_same_day))) %>%
  filter(Datetime == min(Datetime)) %>%
  ungroup() %>%
  # Remove duplicated records because of NA values in the Tide, Microcystis, and TurbidityNTU
    # columns - the other WQ measurements are the same
  arrange(Tide) %>%
  distinct(pick(all_of(grp_dupl_same_day) & !all_of("Tide")), .keep_all = TRUE) %>%
  arrange(Microcystis) %>%
  distinct(pick(all_of(grp_dupl_same_day) & !all_of("Microcystis")), .keep_all = TRUE) %>%
  arrange(TurbidityNTU) %>%
  distinct(pick(all_of(grp_dupl_same_day) & !all_of("TurbidityNTU")), .keep_all = TRUE) %>%
  # There are two pairs of records (STTD on 2017-01-20 and 2017-01-31) that share identical values
    # for all but one of the water quality parameters. The values in the mismatched parameter seem
    # to be from a typo during data entry. We will only keep the records with the earliest Datetime
    # for these pairs.
  arrange(Datetime) %>%
  distinct(pick(all_of(grp_dupl_same_day) & !all_of("Conductivity")), .keep_all = TRUE) %>%
  distinct(pick(all_of(grp_dupl_same_day) & !all_of("DissolvedOxygen")), .keep_all = TRUE)

# Combine data and finish cleaning up duplicates between two datasets
df_data_all <- bind_rows(df_data_fish_c, df_data_zoop) %>%
  # Remove the duplicated records shared between the two data sets (STTD)
  distinct(pick(everything() & !all_of("Source")), .keep_all = TRUE) %>%
  # Remove duplicated records due to different values in the Notes column
  arrange(Notes) %>%
  distinct(pick(all_of(grp_dupl_all)), .keep_all = TRUE) %>%
  # Remove duplicated records between the two datasets that were collected on the same day and
    # station that have identical water quality measurements. We will keep the records with the
    # earliest Datetime for the groups of records that share identical water quality measurements
    # with different Datetimes.
  group_by(pick(all_of(grp_dupl_same_day))) %>%
  filter(Datetime == min(Datetime)) %>%
  ungroup() %>%
  # Remove duplicated records because of NA values in the Microcystis, Tide, and TurbidityNTU
    # columns - the other WQ measurements are the same
  arrange(Microcystis) %>%
  distinct(pick(all_of(grp_dupl_same_day) & !all_of("Microcystis")), .keep_all = TRUE) %>%
  arrange(Tide) %>%
  distinct(pick(all_of(grp_dupl_same_day) & !all_of("Tide")), .keep_all = TRUE) %>%
  filter(
    !(Station == "STTD" & Datetime == "2015-02-17 09:26:00"),
    !(Station == "STTD" & Datetime == "2016-03-18 09:30:00")
  ) %>%
  # Clean up last three duplicate pairs - records have different water quality measurements but same
    # Datetime (one is from the zoop and the other is from the fish data set) - Remove the record
    # from the fish data set collected on 2010-02-10 11:13:00, otherwise keep the records from the
    # fish data set
  filter(!(Source == "ls_YBFMP_fish" & Station == "STTD" & Datetime == "2010-02-10 11:13:00")) %>%
  arrange(Source) %>%
  distinct(Station, Datetime, .keep_all = TRUE)

# Finalize YBFMP data set
YBFMP <- df_data_all %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Datetime)

usethis::use_data(YBFMP, overwrite = TRUE)

document_helper_edi(ls_YBFMP_fish$edi_id, YBFMP)
document_helper_edi(ls_YBFMP_zoop$edi_id, YBFMP)
update_edi_metadata("YBFMP_fish", ls_YBFMP_fish$edi_id)
update_edi_metadata("YBFMP_zoop", ls_YBFMP_zoop$edi_id)
