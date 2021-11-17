## code to prepare `USGS` dataset goes here

require(readr)
require(dplyr)
require(tidyr)
require(lubridate)
require(purrr)
require(readxl)
require(stringr)

#Latest USGS data from: https://sfbay.wr.usgs.gov/water-quality-database/

USGS_stations <- read_excel(file.path("data-raw", "USGS", "USGSSFBayStations.xlsx"))%>%
  select(Station, Latitude="Latitude degree", Longitude="Longitude degree")%>%
  mutate(Station=as.character(Station))

USGSfiles_pub <- list.files(path = file.path("data-raw", "USGS"), full.names = T, pattern="SanFranciscoBayWaterQualityData.csv")
USGSfiles_web<-list.files(path = file.path("data-raw", "USGS"), full.names = T, pattern="wqdata")

USGS <- map_dfr(USGSfiles_pub, ~read_csv(., col_types = cols_only(Date="c", Time="c", Station_Number="d", Depth="d",
                                                              Calculated_Chlorophyll="d", Salinity="d", Temperature="d",
                                                             `Nitrate_+_Nitrite` = 'd', Ammonium = 'd', Phosphate = 'd', Silicate = 'd')))%>%
  rename(Station=Station_Number, Chlorophyll=Calculated_Chlorophyll, DissNitrateNitrite=`Nitrate_+_Nitrite`, DissAmmonia = Ammonium, DissOrthophos = Phosphate,
         DissSilica = Silicate) %>%
  mutate(Time=paste0(str_sub(Time, end=-3), ":", str_sub(Time, start=-2)))%>%

  bind_rows(read_csv(file.path("data-raw", "USGS", "1969_2015USGS_SFBAY_22APR20.csv"),
                     col_types = cols_only(Date="c", Time="c", Station="d", `Depth`="d", `Calculated Chl-a`="d",
                                           Salinity="d", `Temperature`="d", NO32='d', NH4='d', PO4='d', Si='d'))%>%
              rename(Chlorophyll=starts_with("Calculated"), DissNitrateNitrite = NO32, DissAmmonia = NH4, DissOrthophos = PO4, DissSilica = Si))%>%

  bind_rows(map_dfr(USGSfiles_web, ~read_csv(., col_types = cols_only(Date="c", Time="c", Station="d", `Depth (m)`="d",
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
         Source="USGS")%>%
  select(-Time)%>%
  rename(Sample_depth=Depth)
  # reset index
  row.names(USGS) <- NULL

  # bin based on min depth and presense of nutrient vals (if no nutrients, then just min depth)
  temp_surf_depth <- 2
  nutr_surf_depth <- 5

  USGS <- USGS %>%
    group_by(Station, Date)%>%
    mutate(
      Depth_bin =
        # surface
        if_else(
          Sample_depth == min(Sample_depth) & Sample_depth < temp_surf_depth & all(is.na(DissNitrateNitrite)) & all(is.na(DissAmmonia)) & all(is.na(DissOrthophos)) & all(is.na(DissSilica)), 'surface_no_nutr', # only temp etc. data
          if_else(
            Sample_depth == min(Sample_depth) & Sample_depth < temp_surf_depth & c(!is.na(DissNitrateNitrite) | !is.na(DissAmmonia) | !is.na(DissOrthophos) | !is.na(DissSilica)), 'surface_temp_nutr', # temp/nutr data at same (min) depth
            if_else(
              Sample_depth != min(Sample_depth) & Sample_depth < nutr_surf_depth & c(!is.na(DissNitrateNitrite) | !is.na(DissAmmonia) | !is.na(DissOrthophos) | !is.na(DissSilica)), 'surface_only_nutr', # only nutr data if at lower depth than temp
              if_else(
                Sample_depth == min(Sample_depth) & Sample_depth < temp_surf_depth & c(any(is.na(DissNitrateNitrite)) | any(is.na(DissAmmonia)) | any(is.na(DissOrthophos)) | any(is.na(DissSilica))), 'surface_only_temp', # only temp data if at higher depth than nutr
                # bottom
                if_else(
                  Sample_depth == max(Sample_depth) & Sample_depth > temp_surf_depth & all(is.na(DissNitrateNitrite)) & all(is.na(DissAmmonia)) & all(is.na(DissOrthophos)) & all(is.na(DissSilica)), 'bottom_no_nutr', # only temp etc. data
                  if_else(
                    Sample_depth == max(Sample_depth) & Sample_depth > temp_surf_depth & c(!is.na(DissNitrateNitrite) | !is.na(DissAmmonia) | !is.na(DissOrthophos) | !is.na(DissSilica)), 'bottom_temp_nutr', # temp/nutr data at same (min) depth
                    if_else(
                      Sample_depth != max(Sample_depth) & Sample_depth > nutr_surf_depth & c(!is.na(DissNitrateNitrite) | !is.na(DissAmmonia) | !is.na(DissOrthophos) | !is.na(DissSilica)), 'bottom_only_nutr', # only nutr data if at lower depth than temp
                      if_else(
                        Sample_depth == max(Sample_depth) & Sample_depth > temp_surf_depth & c(any(is.na(DissNitrateNitrite)) | any(is.na(DissAmmonia)) | any(is.na(DissOrthophos)) | any(is.na(DissSilica))), 'bottom_only_temp', # only temp data if at higher depth than nutr
                        'other')))))))),
      Datetime = min(Datetime)+(max(Datetime)-min(Datetime))/2) %>%
    ungroup()

  # create nutrient cols for data that differs b/w nutrients and baseline
  USGS$Salinity_nutr <- ifelse(USGS$Depth_bin %in% c('surface_only_nutr', 'surface_temp_nutr', 'bottom_only_nutr', 'bottom_temp_nutr'), USGS$Salinity, NA)
  USGS$Temperature_nutr <- ifelse(USGS$Depth_bin %in% c('surface_only_nutr', 'surface_temp_nutr', 'bottom_only_nutr', 'bottom_temp_nutr'), USGS$Temperature, NA)
  USGS$Sample_depth_nutr <- ifelse(USGS$Depth_bin %in% c('surface_only_nutr', 'surface_temp_nutr', 'bottom_only_nutr', 'bottom_temp_nutr'), USGS$Sample_depth, NA)
  USGS$Chlorophyll_nutr <- ifelse(USGS$Depth_bin %in% c('surface_only_nutr', 'surface_temp_nutr', 'bottom_only_nutr', 'bottom_temp_nutr'), USGS$Sample_depth, NA)

  # remove vals for nutrient only rows to prepare for merge
  USGS[USGS$Depth_bin %in% c('surface_only_nutr', 'bottom_only_nutr'),]$Chlorophyll <- NA
  USGS[USGS$Depth_bin %in% c('surface_only_nutr', 'bottom_only_nutr'),]$Salinity <- NA
  USGS[USGS$Depth_bin %in% c('surface_only_nutr', 'bottom_only_nutr'),]$Temperature <- NA
  USGS[USGS$Depth_bin %in% c('surface_only_nutr', 'bottom_only_nutr'),]$Sample_depth <- NA

  # rename bins
  USGS$Depth_bin <- ifelse(grepl('surface', USGS$Depth_bin), 'surface', ifelse(grepl('bottom', USGS$Depth_bin), 'bottom', 'other'))

  # merge nutr_only and temp_only rows
  coalesce_by_column <- function(df) {
    return(dplyr::coalesce(!!! as.list(df)))
  }

  USGS <- USGS %>%
    group_by(Station, Date, Datetime, Source, Depth_bin) %>%
    summarise_all(coalesce_by_column) %>%
    ungroup()

  # filter data/cols
  USGS <- USGS %>% filter(Depth_bin %in% c('surface', 'bottom'))%>%
  pivot_wider(names_from=Depth_bin, values_from=c(Sample_depth, Salinity, Chlorophyll, Temperature, Sample_depth_nutr, Salinity_nutr, Chlorophyll_nutr, Temperature_nutr, DissNitrateNitrite, DissAmmonia, DissOrthophos, DissSilica),
              values_fn=list(Sample_depth=mean, Salinity=mean, Chlorophyll=mean, Temperature=mean, Sample_depth_nutr=mean, Salinity_nutr=mean, Chlorophyll_nutr=mean, Temperature_nutr=mean, DissNitrateNitrite=mean, DissAmmonia=mean, DissOrthophos=mean, DissSilica=mean))%>% # This will just average out multiple measurements at same depth
  left_join(USGS_stations, by="Station") %>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Sample_depth=Sample_depth_surface, Sample_depth_bottom,
         Temperature=Temperature_surface, Temperature_bottom, Salinity=Salinity_surface, Chlorophyll=Chlorophyll_surface,
         Sample_depth_nutr=Sample_depth_nutr_surface, Chlorophyll_nutr=Chlorophyll_nutr_surface, Temperature_nutr=Temperature_nutr_surface,
         Salinity_nutr=Salinity_nutr_surface, DissNitrateNitrite=DissNitrateNitrite_surface, DissAmmonia=DissAmmonia_surface,
         DissOrthophos=DissOrthophos_surface, DissSilica=DissSilica_surface)


# convert units
USGS$DissNitrateNitrite <- sapply(USGS$DissNitrateNitrite, function(x) x*(14.007/(10^3))) # molar mass of N
USGS$DissAmmonia <- sapply(USGS$DissAmmonia, function(x) x*(14.007/(10^3))) # molar mass of N
USGS$DissOrthophos <- sapply(USGS$DissOrthophos, function(x) x*(30.974/(10^3))) # molar mass of P
USGS$DissSilica <- sapply(USGS$DissSilica, function(x) x*(60.084/(10^3))) # molar mass of SiO2

usethis::use_data(USGS, overwrite = TRUE)
