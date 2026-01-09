# Code to prepare `USGS_SFBS` dataset
library(dplyr)
library(stringr)
library(tidyr)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source helper functions
source("data-raw/01_Global/data_raw_helpers.R")

# Define settings for dataset
survey <- "USGS_SFBS"
tzone <- "America/Los_Angeles"

# Download data to temporary directory
# 1969-2015 published data: https://www.sciencebase.gov/catalog/item/64248ee5d34e370832fe343d
get_scibase_data(
  item_id = "64248ee5d34e370832fe343d",
  entity_regex = c("station_locations1969", "WaterQualityData1969")
)

# 2016-2021 published data: https://www.sciencebase.gov/catalog/item/5966abe6e4b0d1f9f05cf551
get_scibase_data(
  item_id = "5966abe6e4b0d1f9f05cf551",
  entity_regex = c("TableofStationLocations", "2016.+WaterQualityData")
)

# Run standardized workflow to import data and process it
# Notes:
  # 1) Using discrete DO from 1969-2015 and DO from CTD sensor from 2016-onward
  # 2) 2022-2025 data downloaded from website: https://sfbay.wr.usgs.gov/water-quality-database/
    # Nutrient data available through 9/10/2024, all other data available through 3/20/2025
ls_USGS_SFBS <- import_proc_data(survey)

# Add coordinates to each dataset separately
df_data_1969_2015 <- ls_USGS_SFBS$df_data_1969_2015 %>%
  left_join(ls_USGS_SFBS$df_stations_1969_2015, by = join_by(Station))

# Station 34 had a change in coordinates after the 1/21/2016 sampling event
df_stations_Jan2016 <- ls_USGS_SFBS$df_stations_2016_2021 %>%
  filter(is.na(Comments) | Comments == "location 1/21/2016") %>%
  select(-Comments)

df_stations_after_Jan2016 <- ls_USGS_SFBS$df_stations_2016_2021 %>%
  filter(is.na(Comments) | Comments == "location 2/2/2016 and after") %>%
  select(-Comments)

df_data_2016_2021 <- bind_rows(
  ls_USGS_SFBS$df_data_2016_2021 %>%
    filter(Date <= "2016-01-21") %>%
    left_join(df_stations_Jan2016, by = join_by(Station)),
  ls_USGS_SFBS$df_data_2016_2021 %>%
    filter(Date > "2016-01-21") %>%
    left_join(df_stations_after_Jan2016, by = join_by(Station))
)

# Use coordinates from latest publication (df_stations_after_Jan2016) for 2022-2025 data downloaded
  # from website
df_data_website <- ls_USGS_SFBS$df_data_website %>%
  mutate(Station = str_remove(Station, "\\.0$")) %>%
  left_join(df_stations_after_Jan2016, by = join_by(Station))

# Combine data and create Datetime column from Date and Time columns
df_data_c <- bind_rows(df_data_1969_2015, df_data_2016_2021, df_data_website) %>%
  combine_datetime(timezone = tzone)

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
df_data_c2 <- df_data_c %>%
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

# Average sample depths among each Depth_bin and Datetimes for each sample so they match correctly
  # when pivoted wider
df_data_c3 <- df_data_c2 %>%
  group_by(Date, Station, Depth_bin) %>%
  mutate(Sample_depth = mean(Sample_depth)) %>%
  ungroup(Depth_bin) %>%
  mutate(Datetime = mean(Datetime)) %>%
  ungroup()

# Pull out WQ parameters and restructure data frame to wide format
df_data_c3_wq <- df_data_c3 %>%
  filter(Depth_bin != "nutrient") %>%
  pivot_wider(names_from = Parameter, values_from = Value) %>%
  pivot_wider(names_from = Depth_bin, values_from = all_of(c(wq_param, "Sample_depth"))) %>%
  rename_with(\(x) str_remove(x, "_surface$"), ends_with("_surface") & !starts_with("Sample")) %>%
  select(-Chlorophyll_bottom)

# Pull out nutrient parameters and restructure data frame to wide format
df_data_c3_nutr <- df_data_c3 %>%
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
  full_join(df_data_c3_wq, df_data_c3_nutr) %>%
  # Remove rows where all measurements are NA, if they exist
  rm_rows_all_miss_data() %>%
  # Add Source column
  add_source_col(survey) %>%
  # Standardize column order
  standardize_col_order() %>%
  arrange(Date, Station) %>%
  add_update_info()

usethis::use_data(USGS_SFBS, overwrite = TRUE)
document_helper_other(USGS_SFBS)
