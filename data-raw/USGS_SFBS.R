## code to prepare `USGS_SFBS` dataset goes here

require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(purrr)
require(readxl)
require(stringr)

#Latest USGS_SFBS data from: https://sfbay.wr.usgs.gov/water-quality-database/

USGS_SFBS_stations <- read_excel(file.path("data-raw", "USGS_SFBS", "USGSSFBayStations.xlsx"))%>%
  select(Station, Latitude="Latitude degree", Longitude="Longitude degree")%>%
  mutate(Station=as.character(Station))

USGS_SFBSfiles_pub <- list.files(path = file.path("data-raw", "USGS_SFBS"), full.names = T, pattern="SanFranciscoBayWaterQualityData.csv")
USGS_SFBSfiles_web<-list.files(path = file.path("data-raw", "USGS_SFBS"), full.names = T, pattern="wqdata")

USGS_SFBS <- map_dfr(USGS_SFBSfiles_pub, ~read_csv(., col_types = cols_only(Date="c", Time="c", Station_Number="d", Depth="d",
                                                              Calculated_Chlorophyll="d", Salinity="d", Temperature="d",
                                                             `Nitrate_+_Nitrite` = 'd', Ammonium = 'd', Phosphate = 'd', Silicate = 'd')))%>%
  rename(Station=Station_Number, Chlorophyll=Calculated_Chlorophyll, DissNitrateNitrite=`Nitrate_+_Nitrite`, DissAmmonia = Ammonium, DissOrthophos = Phosphate,
         DissSilica = Silicate) %>%
  mutate(Time=paste0(str_sub(Time, end=-3), ":", str_sub(Time, start=-2)))%>%

  bind_rows(read_csv(file.path("data-raw", "USGS_SFBS", "1969_2015USGS_SFBAY_22APR20.csv"),
                     col_types = cols_only(Date="c", Time="c", Station="d", `Depth`="d", `Calculated Chl-a`="d",
                                           Salinity="d", `Temperature`="d", NO32='d', NH4='d', PO4='d', Si='d'))%>%
              rename(Chlorophyll=starts_with("Calculated"), DissNitrateNitrite = NO32, DissAmmonia = NH4, DissOrthophos = PO4, DissSilica = Si))%>%

  bind_rows(map_dfr(USGS_SFBSfiles_web, ~read_csv(., col_types = cols_only(Date="c", Time="c", Station="d", `Depth (m)`="d",
                                                                      `Calculated Chlorophyll-a (micrograms/L)`="d",
                                                                      Salinity="d", `Temperature (Degrees Celsius)`="d", `NO32 (Micromolar)` = 'd',
                                                                      `NH4 (Micromolar)` = 'd', `PO4 (Micromolar)` = 'd', `Si (Micromolar)` = 'd')))%>%
  rename(Depth=`Depth (m)`, Chlorophyll=`Calculated Chlorophyll-a (micrograms/L)`, Temperature=`Temperature (Degrees Celsius)`, DissNitrateNitrite=`NO32 (Micromolar)`,
         DissAmmonia = `NH4 (Micromolar)`, DissOrthophos = `PO4 (Micromolar)`, DissSilica = `Si (Micromolar)`))%>%
  filter(!is.na(Date))%>%
  mutate(Date=parse_date_time(Date, orders=c("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d"), tz="America/Los_Angeles"),
         Time=hm(Time),
         Station=as.character(Station))%>%
  mutate(Datetime=parse_date_time(paste0(Date, " ", hour(Time), ":", minute(Time)), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Source="USGS_SFBS")%>%
  select(-Time)%>%
  rename(Sample_depth=Depth)

# reset index
row.names(USGS_SFBS) <- NULL

# bin based on min depth and presense of nutrient vals (if no nutrients, then just min depth)
temp_surf_depth <- 2
nutr_surf_depth <- 5

USGS_SFBS <- USGS_SFBS %>%
  group_by(Station, Date) %>%
  mutate(
    Depth_bin = case_when(
      # Surface
      Sample_depth == min(Sample_depth) & Sample_depth < temp_surf_depth & all(is.na(DissNitrateNitrite)) & all(is.na(DissAmmonia)) & all(is.na(DissOrthophos)) & all(is.na(DissSilica)) ~ 'surface_no_nutr', # only temp etc. data
      Sample_depth == min(Sample_depth) & Sample_depth < temp_surf_depth & c(!is.na(DissNitrateNitrite) | !is.na(DissAmmonia) | !is.na(DissOrthophos) | !is.na(DissSilica)) ~ 'surface_temp_nutr', # temp/nutr data at same (min) depth
      Sample_depth != min(Sample_depth) & Sample_depth < nutr_surf_depth & c(!is.na(DissNitrateNitrite) | !is.na(DissAmmonia) | !is.na(DissOrthophos) | !is.na(DissSilica)) ~ 'surface_only_nutr', # only nutr data if at lower depth than temp
      Sample_depth == min(Sample_depth) & Sample_depth < temp_surf_depth & c(any(is.na(DissNitrateNitrite)) | any(is.na(DissAmmonia)) | any(is.na(DissOrthophos)) | any(is.na(DissSilica))) ~ 'surface_only_temp', # only temp data if at higher depth than nutr
      # Bottom
      Sample_depth == max(Sample_depth) & Sample_depth > temp_surf_depth & all(is.na(DissNitrateNitrite)) & all(is.na(DissAmmonia)) & all(is.na(DissOrthophos)) & all(is.na(DissSilica)) ~ 'bottom_no_nutr', # only temp etc. data
      Sample_depth == max(Sample_depth) & Sample_depth > temp_surf_depth & c(!is.na(DissNitrateNitrite) | !is.na(DissAmmonia) | !is.na(DissOrthophos) | !is.na(DissSilica)) ~ 'bottom_temp_nutr', # temp/nutr data at same (min) depth
      Sample_depth != max(Sample_depth) & Sample_depth > nutr_surf_depth & c(!is.na(DissNitrateNitrite) | !is.na(DissAmmonia) | !is.na(DissOrthophos) | !is.na(DissSilica)) ~ 'bottom_only_nutr', # only nutr data if at lower depth than temp
      Sample_depth == max(Sample_depth) & Sample_depth > temp_surf_depth & c(any(is.na(DissNitrateNitrite)) | any(is.na(DissAmmonia)) | any(is.na(DissOrthophos)) | any(is.na(DissSilica))) ~ 'bottom_only_temp', # only temp data if at higher depth than nutr
      TRUE ~ "other"
    ),
    Datetime = min(Datetime) + (max(Datetime) - min(Datetime)) / 2
  ) %>%
  ungroup()

# create nutrient cols for data that differs b/w nutrients and baseline
USGS_SFBS$Salinity_nutr <- ifelse(USGS_SFBS$Depth_bin %in% c('surface_only_nutr', 'surface_temp_nutr', 'bottom_only_nutr', 'bottom_temp_nutr'), USGS_SFBS$Salinity, NA)
USGS_SFBS$Temperature_nutr <- ifelse(USGS_SFBS$Depth_bin %in% c('surface_only_nutr', 'surface_temp_nutr', 'bottom_only_nutr', 'bottom_temp_nutr'), USGS_SFBS$Temperature, NA)
USGS_SFBS$Sample_depth_nutr <- ifelse(USGS_SFBS$Depth_bin %in% c('surface_only_nutr', 'surface_temp_nutr', 'bottom_only_nutr', 'bottom_temp_nutr'), USGS_SFBS$Sample_depth, NA)
USGS_SFBS$Chlorophyll_nutr <- ifelse(USGS_SFBS$Depth_bin %in% c('surface_only_nutr', 'surface_temp_nutr', 'bottom_only_nutr', 'bottom_temp_nutr'), USGS_SFBS$Chlorophyll, NA)

# remove vals for nutrient only rows to prepare for merge
USGS_SFBS[USGS_SFBS$Depth_bin %in% c('surface_only_nutr', 'bottom_only_nutr'),]$Chlorophyll <- NA
USGS_SFBS[USGS_SFBS$Depth_bin %in% c('surface_only_nutr', 'bottom_only_nutr'),]$Salinity <- NA
USGS_SFBS[USGS_SFBS$Depth_bin %in% c('surface_only_nutr', 'bottom_only_nutr'),]$Temperature <- NA
USGS_SFBS[USGS_SFBS$Depth_bin %in% c('surface_only_nutr', 'bottom_only_nutr'),]$Sample_depth <- NA

# rename bins
USGS_SFBS$Depth_bin <- ifelse(grepl('surface', USGS_SFBS$Depth_bin), 'surface', ifelse(grepl('bottom', USGS_SFBS$Depth_bin), 'bottom', 'other'))

# merge nutr_only and temp_only rows
coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}

USGS_SFBS <- USGS_SFBS %>%
  group_by(Station, Date, Datetime, Source, Depth_bin) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup()

# filter data/cols
USGS_SFBS <- USGS_SFBS %>%
  filter(Depth_bin %in% c('surface', 'bottom')) %>%
pivot_wider(names_from=Depth_bin, values_from=c(Sample_depth, Salinity, Chlorophyll, Temperature, Sample_depth_nutr, Salinity_nutr, Chlorophyll_nutr, Temperature_nutr, DissNitrateNitrite, DissAmmonia, DissOrthophos, DissSilica),
            values_fn=list(Sample_depth=mean, Salinity=mean, Chlorophyll=mean, Temperature=mean, Sample_depth_nutr=mean, Salinity_nutr=mean, Chlorophyll_nutr=mean, Temperature_nutr=mean, DissNitrateNitrite=mean, DissAmmonia=mean, DissOrthophos=mean, DissSilica=mean))%>% # This will just average out multiple measurements at same depth
left_join(USGS_SFBS_stations, by="Station") %>%
select(Source, Station, Latitude, Longitude, Date, Datetime, Sample_depth_surface, Sample_depth_bottom,
       Temperature=Temperature_surface, Temperature_bottom, Salinity=Salinity_surface, Chlorophyll=Chlorophyll_surface,
       Sample_depth_nutr_surface, DissNitrateNitrite=DissNitrateNitrite_surface, DissAmmonia=DissAmmonia_surface,
       DissOrthophos=DissOrthophos_surface, DissSilica=DissSilica_surface)


# convert units
USGS_SFBS$DissNitrateNitrite <- sapply(USGS_SFBS$DissNitrateNitrite, function(x) x*(14.007/(10^3))) # molar mass of N
USGS_SFBS$DissAmmonia <- sapply(USGS_SFBS$DissAmmonia, function(x) x*(14.007/(10^3))) # molar mass of N
USGS_SFBS$DissOrthophos <- sapply(USGS_SFBS$DissOrthophos, function(x) x*(30.974/(10^3))) # molar mass of P
USGS_SFBS$DissSilica <- sapply(USGS_SFBS$DissSilica, function(x) x*(60.084/(10^3))) # molar mass of SiO2

usethis::use_data(USGS_SFBS, overwrite = TRUE)
