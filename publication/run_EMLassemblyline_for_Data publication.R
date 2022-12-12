# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------

# Update EMLassemblyline and load

#remotes::install_github("EDIorg/EMLassemblyline")
devtools::install_github("sbashevkin/discretewq")
library(EMLassemblyline)
library(EML)
library(dplyr)
library(readr)
library(stringr)

# Define paths for your metadata templates, data, and EML

root<-"Data publication"

path_templates <- file.path(root, "metadata_templates")
path_data <- file.path(root, "data_objects")
path_eml <- file.path(root, "eml")


# Add data ----------------------------------------------------------------

data<-discretewq::wq(Sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "SDO", "SKT", "SLS", "20mm", "Suisun", "Baystudy", "USBR", "USGS_SFBS", "YBFMP", "USGS_CAWSC"))%>%
  mutate(Date=as.character(Date, format="%Y-%m-%d"),
         Datetime=as.character(Datetime),
         Notes=str_replace_all(Notes, '"', "'"),# Replace full quotes with single quotes to avoid data read errors
         across(c(Notes, StationID), ~str_replace_all(.x, fixed('\n'), " ")), # Replace line breaks with space to avoid data read errors
         across(c(Notes, StationID), ~str_replace_all(.x, fixed('\r'), " ")))%>% # Replace line breaks with space to avoid data read errors
  select(Source, Station=StationID, Latitude, Longitude, Field_coords, Date, Datetime, Depth, 
         Sample_depth_surface, Sample_depth_nutr_surface, Sample_depth_bottom, Tide, Temperature, Temperature_bottom, 
         Conductivity, Conductivity_bottom, Salinity, Salinity_bottom, Secchi, Secchi_estimated, Microcystis, Chlorophyll_Sign, Chlorophyll, 
         DissolvedOxygen, DissolvedOxygen_bottom, DissolvedOxygenPercent, DissolvedOxygenPercent_bottom,
         pH, pH_bottom, TotAlkalinity, TotAmmonia, DissAmmonia_Sign, DissAmmonia, 
         DissBromide, DissCalcium, TotChloride, DissChloride, DissNitrateNitrite_Sign, 
         DissNitrateNitrite, DOC, TOC, DON, TON, DissOrthophos_Sign, DissOrthophos, TotPhos, 
         DissSilica, TDS, TSS, VSS, TKN, Notes)

write_csv(data, file.path(path_data, "Delta_Integrated_WQ.csv"))
# Create metadata templates ---------------------------------------------------

# Below is a list of boiler plate function calls for creating metadata templates.
# They are meant to be a reminder and save you a little time. Remove the 
# functions and arguments you don't need AND ... don't forget to read the docs! 
# E.g. ?template_core_metadata

# Create core templates (required for all data packages)

EMLassemblyline::template_core_metadata(
  path = path_templates,
  license = "CCBY",
  file.type = ".docx")

# Create provenance template

EMLassemblyline::template_provenance(
  path=path_templates,
)

# Create table attributes template (required when data tables are present)

EMLassemblyline::template_table_attributes(
  path = path_templates,
  data.path = path_data,
  data.table = c("Delta_Integrated_WQ.csv", "Delta_Integrated_WQ_metadata.csv"))

# Create categorical variables template (required when attributes templates
# contains variables with a "categorical" class)

EMLassemblyline::template_categorical_variables(
  path = path_templates, 
  data.path = path_data)

# Create geographic coverage (required when more than one geographic location
# is to be reported in the metadata).

EMLassemblyline::template_geographic_coverage(
  path = path_templates, 
  data.path = path_data, 
  data.table = "Delta_Integrated_WQ.csv", 
  lat.col = "Latitude",
  lon.col = "Longitude",
  site.col = "Station")

# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create 
# the EML.

# Sandbox
#ID<-"edi.750.1"

# EDI
ID<-"edi.731.5"

wq_eml<-EMLassemblyline::make_eml(
  path = path_templates,
  data.path = path_data,
  eml.path = path_eml, 
  dataset.title = "Six decades (1959-2021) of water quality in the upper San Francisco Estuary: an integrated database of 15 discrete monitoring surveys in the Sacramento San Joaquin Delta, Suisun Bay, Suisun Marsh, and San Francisco Bay", 
  temporal.coverage = c("1959-06-13", "2022-08-25"), 
  maintenance.description = "ongoing", 
  data.table = c("Delta_Integrated_WQ.csv", "Delta_Integrated_WQ_metadata.csv"), 
  data.table.description = c("Integrated water quality database", "Information on each survey included in the integrated database"),
  data.table.quote.character=c('"','"'),
  data.table.url=c("https://deltacouncil.box.com/shared/static/si4d9w68g3p1yd4toir1jok9fljiwe78.csv", "https://deltacouncil.box.com/shared/static/2nlzvr1rid97k37rtjbco6y1qwu51pfi.csv"),
  user.id = "sbashevkin",
  user.domain = "EDI", 
  package.id = ID,
  return.obj=TRUE)

changelog<-list(list(changeScope="Metadata and data",
                     oldValue="See previous version (1)",
                     changeDate="2022-01-10",
                     comment="Fixed EMP timezone issue. In the previous (first) version of this dataset, we had imported EMP data assuming that times were recorded in local time (PST/PDT). 
                              However, the Environmental Monitoring Program (EMP) dataset records times in PST year-round. This version (2) has corrected that issue and all datetimes
                              are now correctly in local Pacific time."),
                list(changeScope="Metadata and data",
                     oldValue="See previous version (2)",
                     changeDate="2022-03-10",
                     comment="1) Updated to newest versions of the source datasets.
                              2) Added USGS_SFBS and EMP nutrient data.
                              3) Added new surveys: Yolo Bypass Fish Monitoring Program, Stockton Dissolved Oxygen survey, Smelt Larva Survey, and USGS California Water Science Center monitoring data.
                              4) Renamed the USGS survey to USGS_SFBS because of the addition of the other USGS survey: USGS_CAWSC."),
                list(changeScope="Metadata and data",
                     oldValue="See previous version (3)",
                     changeDate="2022-03-28",
                     comment="1) Fixed error with YBFMP Secchi Depth Data (it was previously in m instead of cm). 
                              2) Added 'Secchi_estimated' column from FMWT data"),
                list(changeScope="Metadata and data",
                     oldValue="See previous version (3)",
                     changeDate=Sys.Date(),
                     comment="1) Fixed error with previous version where only the metadata file was uploaded and the full dataset was not included."))
class(changelog)<-c("emld", "list")

wq_eml$dataset$maintenance$changeHistory<-changelog
write_eml(wq_eml, file.path(path_eml, paste0(ID, ".xml")))
eml_validate(file.path(path_eml, paste0(ID, ".xml")))
