# Code to prepare `USGS_SFBS` dataset
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for datasets
survey <- "USGS_SFBS"
date_fmt_pub <- "mdy"
date_fmt_web <- "Ymd"
time_fmt <- "HM"
tzone <- "America/Los_Angeles"

# 1969 to 2015 data: https://www.sciencebase.gov/catalog/item/64248ee5d34e370832fe343d
# Notes: Using discrete DO from 1969-2015
sb_id_1969_2015 <- "64248ee5d34e370832fe343d"
ent_regex_1969_2015 <- c(
  "df_stations_1969_2015" = "station_locations1969",
  "df_data_1969_2015"= "WaterQualityData1969"
)

ls_USGS_SFBS_1969_2015 <- import_proc_scibase_data(survey, sb_id_1969_2015, ent_regex_1969_2015)

# Add coordinates to data and create Datetime column
df_USGS_SFBS_1969_2015 <- ls_USGS_SFBS_1969_2015$df_data_1969_2015 %>%
  left_join(ls_USGS_SFBS_1969_2015$df_stations_1969_2015, by = join_by(Station)) %>%
  convert_datetime(date_fmt_pub, time_fmt, tzone)

# 2016-2021 published data: https://www.sciencebase.gov/catalog/item/5966abe6e4b0d1f9f05cf551
# Notes: Using DO from CTD sensor from 2016-onward
sb_id_2016_2021 <- "5966abe6e4b0d1f9f05cf551"
ent_regex_2016_2021 <- c(
  "df_stations_2016_2021" = "TableofStationLocations",
  "df_data_2016_2021"= "2016.+WaterQualityData"
)

ls_USGS_SFBS_2016_2021 <- import_proc_scibase_data(survey, sb_id_2016_2021, ent_regex_2016_2021)

# Add coordinates to data and create Datetime column
# Station 34 had a change in coordinates after the 1/21/2016 sampling event
df_stations_Jan2016 <- ls_USGS_SFBS_2016_2021$df_stations_2016_2021 %>%
  filter(is.na(Comments) | Comments == "location 1/21/2016") %>%
  select(-Comments)

df_stations_after_Jan2016 <- ls_USGS_SFBS_2016_2021$df_stations_2016_2021 %>%
  filter(is.na(Comments) | Comments == "location 2/2/2016 and after") %>%
  select(-Comments)

df_USGS_SFBS_2016_2021 <- ls_USGS_SFBS_2016_2021$df_data_2016_2021 %>%
  convert_datetime(date_fmt_pub, time_fmt, tzone)

df_USGS_SFBS_2016_2021_c <- bind_rows(
  df_USGS_SFBS_2016_2021 %>%
    filter(Date <= "2016-01-21") %>%
    left_join(df_stations_Jan2016, by = join_by(Station)),
  df_USGS_SFBS_2016_2021 %>%
    filter(Date > "2016-01-21") %>%
    left_join(df_stations_after_Jan2016, by = join_by(Station))
)

# 2022-2025 data downloaded from website: https://sfbay.wr.usgs.gov/water-quality-database/
# Nutrient data available through 9/10/2024, all other data available through 3/20/2025
USGS_SFBS_files_web <- list.files(
  path = "data-raw/USGS_SFBS", full.names = TRUE, pattern = "wqdata"
)

df_USGS_SFBS_web <-
  map(USGS_SFBS_files_web, \(x) import_raw_data(x, survey, "df_data_website")) %>%
  list_rbind()

# Add coordinates to data and create Datetime column
# Use coordinates from latest publication (df_stations_after_Jan2016)
df_USGS_SFBS_web_c <- df_USGS_SFBS_web %>%
  mutate(Station = str_remove(Station, "\\.0$")) %>%
  left_join(df_stations_after_Jan2016, by = join_by(Station)) %>%
  convert_datetime(date_fmt_web, time_fmt, tzone)

# Combine data
df_USGS_SFBS_c <- bind_rows(df_USGS_SFBS_1969_2015, df_USGS_SFBS_2016_2021_c, df_USGS_SFBS_web_c)

# Define thresholds for minimum surface sample depths
wq_surf_depth <- 2
nutr_surf_depth <- 5

# Define columns containing WQ measurements
wq_param <- c(
  "Chlorophyll",
  "Salinity",
  "Temperature",
  "DissolvedOxygen",
  "DissolvedOxygenPercent"
)

# Define columns containing nutrient measurements
nutr_param <- c(
  "DissNitrateNitrite",
  "DissAmmonia",
  "DissOrthophos",
  "DissSilica"
)

# Select surface and bottom WQ samples and nutrient samples
df_USGS_SFBS_c1 <- df_USGS_SFBS_c %>%
  pivot_longer(
    cols = all_of(c(wq_param, nutr_param)),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  drop_na(Value) %>%
  group_by(Date, Station, Parameter) %>%
  mutate(
    Depth_bin = case_when(
      Parameter %in% wq_param & Sample_depth < wq_surf_depth &
        Sample_depth == min(Sample_depth) ~ "surface",
      Parameter %in% nutr_param & Sample_depth < nutr_surf_depth &
        Sample_depth == min(Sample_depth) ~ "nutrient",
      Parameter %in% wq_param & Sample_depth > wq_surf_depth &
        Sample_depth == max(Sample_depth) ~ "bottom",
      .default = "other"
    )
  ) %>%
  ungroup() %>%
  filter(
    Depth_bin != "other",
    !(Parameter == "Chlorophyll" & Depth_bin == "bottom") # exclude Chlorophyll bottom samples
  )

# Average sample depths among each Depth_bin and DateTimes for each sample so they match correctly
  # when pivoted wider
df_USGS_SFBS_c2 <- df_USGS_SFBS_c1 %>%
  group_by(Date, Station, Depth_bin) %>%
  mutate(Sample_depth = mean(Sample_depth)) %>%
  # Average Datetimes for each sample so that they match correctly as well
  ungroup(Depth_bin) %>%
  mutate(Datetime = mean(Datetime)) %>%
  ungroup()

# Pull out WQ parameters and restructure data frame to wide format
df_USGS_SFBS_c2_wq <- df_USGS_SFBS_c2 %>%
  filter(Depth_bin != "nutrient") %>%
  pivot_wider(names_from = Parameter, values_from = Value) %>%
  pivot_wider(names_from = Depth_bin, values_from = all_of(c(wq_param, "Sample_depth"))) %>%
  rename_with(\(x) str_remove(x, "_surface$"), ends_with("_surface") & !starts_with("Sample")) %>%
  select(-Chlorophyll_bottom)

# Pull out nutrient parameters and restructure data frame to wide format
df_USGS_SFBS_c2_nutr <- df_USGS_SFBS_c2 %>%
  filter(Depth_bin == "nutrient") %>%
  pivot_wider(id_cols = -Depth_bin, names_from = Parameter, values_from = Value) %>%
  rename(Sample_depth_nutr_surface = Sample_depth) %>%
  # Convert units
  mutate(
    DissNitrateNitrite = DissNitrateNitrite * (14.007 / (10^3)), # molar mass of N
    DissAmmonia = DissAmmonia * (14.007 / (10^3)), # molar mass of N
    DissOrthophos = DissOrthophos * (30.974 / (10^3)), # molar mass of P
    DissSilica = DissSilica * (60.084 / (10^3)) # molar mass of SiO2
  )

# Join WQ and nutrient data back together and finish cleaning data
USGS_SFBS <-
  full_join(df_USGS_SFBS_c2_wq, df_USGS_SFBS_c2_nutr) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  add_source_col(survey) %>%
  standardize_col_order() %>%
  arrange(Date, Station)

usethis::use_data(USGS_SFBS, overwrite = TRUE)

document_helper_other(USGS_SFBS)
