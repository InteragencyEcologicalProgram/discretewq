## code to prepare `YBFMP` dataset goes here

require(readr)
require(dplyr)
require(lubridate)

# Function to calculate mode
# Modified from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode to remove NAs
Mode <- function(x) {
  x<-na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.1&entityid=9190cd46d697e59aca2de678f4ca1c95",
              file.path(tempdir(), "Zooplankton_Data.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.494.1&entityid=f2332a9a2aad594f61fea525584694da",
              file.path(tempdir(), "YB_StationCoordinates.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=015e494911cf35c90089ced5a3127334",
              file.path(tempdir(), "YBFMP_Fish_Catch_and_Water_Quality.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.2&entityid=6a82451e84be1fe82c9821f30ffc2d7d",
              file.path(tempdir(), "YBFMP_Site_locations_latitude_and_longitude.csv"), mode="wb",method="libcurl")

YBFMP_stations<-read_csv(file.path(tempdir(), "YB_StationCoordinates.csv"),
                         col_types=cols_only(StationCode="c", Latitude="d", Longitude="d"))%>%
  rename(Station=StationCode)%>%
  bind_rows(
    read_csv(file.path(tempdir(), "YBFMP_Site_locations_latitude_and_longitude.csv"),
             col_types=cols_only(StationCode="c", LatitudeLocation="d", LongitudeLocation="d"))%>%
      rename(Station=StationCode, Latitude=LatitudeLocation, Longitude=LongitudeLocation))%>%
  distinct()

YBFMP<-read_csv(file.path(tempdir(), "Zooplankton_Data.csv"),
                col_types=cols_only(Date="c", Time="c", StationCode="c",
                                    Tide="c", WaterTemperature="d", Secchi="d",
                                    Conductivity="d", SpCnd="d", MicrocystisVisualRank="d",
                                    FieldComments="c"))%>%
  rename(Station=StationCode, Temperature=WaterTemperature, Microcystis=MicrocystisVisualRank, Notes=FieldComments)%>%
  distinct(Station, Date, Time, .keep_all = TRUE)%>%
  bind_rows(
    read_csv(file.path(tempdir(), "YBFMP_Fish_Catch_and_Water_Quality.csv"),
         col_types=cols_only(SampleDate="c", SampleTime="c", StationCode="c",
                             WaterTemperature="d", Secchi="d", Conductivity="d",
                             SpCnd="d", Tide="c"))%>%
           rename(Date=SampleDate, Time=SampleTime, Station=StationCode, Temperature=WaterTemperature)%>%
           distinct(Station, Date, Time, .keep_all = TRUE))%>%
  mutate(Source="YBFMP",
         Date=parse_date_time(Date, orders=c("%Y-%m-%d", "%m/%d/%Y"), tz="America/Los_Angeles"),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), orders=c("%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S"), tz="America/Los_Angeles"),
         Tide=recode(Tide, High="High Slack", Low="Low Slack"),
         Secchi=Secchi*100, #convert to cm
         Conductivity=if_else(is.na(SpCnd), Conductivity / (1 + 0.019 * (Temperature - 25)), SpCnd))%>% #See suisun.R for info
  mutate(ID=paste(Date, Station, Temperature, Secchi, Microcystis, Conductivity))%>% # Following steps to remove duplicated WQ data from multiple fish samples that were nearby in space and time
  group_by(Source, Date, Station, ID)%>%
  summarise(across(c(Temperature, Secchi, Conductivity, Datetime), ~mean(.x, na.rm=T)),
            Tide=Mode(Tide),
            Microcystis=Mode(Microcystis),
            Notes=paste(na.omit(Notes), collapse="; "),
            .groups="drop")%>%
  mutate(across(where(is.numeric), ~if_else(is.nan(.x), NA_real_, .x)), # Replace NaN values with NAs
         Notes=na_if(Notes, ""))%>% # Replace empty string notes with NAs
  left_join(YBFMP_stations, by="Station")%>%
  distinct(Station, Date, Datetime, .keep_all = TRUE)%>%
  select(Source, Station, Latitude, Longitude, Date, Datetime, Tide, Microcystis, Secchi, Temperature, Conductivity, Notes)

usethis::use_data(YBFMP, overwrite = TRUE)
