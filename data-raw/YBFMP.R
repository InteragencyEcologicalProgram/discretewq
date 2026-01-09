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

# Define settings for dataset
survey <- "YBFMP"

# Run standardized workflow to import data and process it
# Fish and zooplankton data sets are in separate EDI publications
# Using defined edi_id for the zooplankton data because this data package hasn't been updated with
  # additional data since Dec 2021
edi_metadata_fish <- get_edi_data(survey, data_type = "fish")
ls_YBFMP_fish <- import_proc_data(
  survey, data_type = "fish", df_files = edi_metadata_fish$df_edi_files
)

edi_metadata_zoop <- get_edi_data(survey, data_type = "zoop", edi_id = "edi.494.2")
ls_YBFMP_zoop <- import_proc_data(
  survey, data_type = "zoop", df_files = edi_metadata_zoop$df_edi_files
)

# Prepare tables before binding them together
# Use a nested dataframe for shared operations
ls_YBFMP <- lst(ls_YBFMP_fish, ls_YBFMP_zoop) %>%
  enframe(name = "Source") %>%
  unnest_wider(value) %>%
  mutate(
    df_data_c = map(
      df_data,
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
    # Add station coordinates
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
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Datetime) %>%
  add_update_info(
    edi_id = c("fish" = edi_metadata_fish$edi_id, "zoop" = edi_metadata_zoop$edi_id)
  )

usethis::use_data(YBFMP, overwrite = TRUE)
document_helper_edi(edi_metadata_fish$edi_id, YBFMP)
document_helper_edi(edi_metadata_zoop$edi_id, YBFMP)
