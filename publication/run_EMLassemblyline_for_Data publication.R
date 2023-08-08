# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------

# Update EMLassemblyline and load

#remotes::install_github("EDIorg/EMLassemblyline")
devtools::install_github("InteragencyEcologicalProgram/discretewq")
library(EMLassemblyline)
library(EML)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(EDIutils)

# Define paths for your metadata templates, data, and EML
root <- "publication"

path_templates <- file.path(root, "metadata_templates")
path_data <- file.path(root, "data_objects")
path_eml <- file.path(root, "eml")


# Add data ----------------------------------------------------------------

data_raw <- discretewq::wq(
  Sources = c(
    "20mm",
    "Baystudy",
    "DJFMP",
    "EDSM",
    "EMP",
    "FMWT",
    "NCRO",
    "SDO",
    "SKT",
    "SLS",
    "STN",
    "Suisun",
    "USBR",
    "USGS_CAWSC",
    "USGS_SFBS",
    "YBFMP"
  )
)

data <- data_raw %>%
  arrange(Source, Station, Datetime) %>%
  mutate(
    Date = as.character(Date, format = "%Y-%m-%d"),
    Datetime = as.character(Datetime),
    Notes = str_replace_all(Notes, '"', "'"), # Replace full quotes with single quotes to avoid data read errors
    across(c(Notes, StationID), ~ str_replace_all(.x, fixed("\n"), " ")), # Replace line breaks with space to avoid data read errors
    across(c(Notes, StationID), ~ str_replace_all(.x, fixed("\r"), " ")) # Replace line breaks with space to avoid data read errors
  ) %>%
  select(
    Source,
    Station = StationID,
    Latitude,
    Longitude,
    Field_coords,
    Date,
    Datetime,
    Depth,
    Sample_depth_surface,
    Sample_depth_nutr_surface,
    Sample_depth_bottom,
    Tide,
    Temperature,
    Temperature_bottom,
    Conductivity,
    Conductivity_bottom,
    Salinity,
    Salinity_bottom,
    Secchi,
    Secchi_estimated,
    TurbidityNTU,
    TurbidityNTU_bottom,
    TurbidityFNU,
    TurbidityFNU_bottom,
    DissolvedOxygen,
    DissolvedOxygen_bottom,
    DissolvedOxygenPercent,
    DissolvedOxygenPercent_bottom,
    pH,
    pH_bottom,
    Microcystis,
    Chlorophyll_Sign,
    Chlorophyll,
    Pheophytin_Sign,
    Pheophytin,
    TotAmmonia_Sign,
    TotAmmonia,
    DissAmmonia_Sign,
    DissAmmonia,
    DissNitrateNitrite_Sign,
    DissNitrateNitrite,
    TotPhos_Sign,
    TotPhos,
    DissOrthophos_Sign,
    DissOrthophos,
    TON_Sign,
    TON,
    DON_Sign,
    DON,
    TKN_Sign,
    TKN,
    TotAlkalinity_Sign,
    TotAlkalinity,
    DissBromide_Sign,
    DissBromide,
    DissCalcium_Sign,
    DissCalcium,
    TotChloride_Sign,
    TotChloride,
    DissChloride_Sign,
    DissChloride,
    DissSilica_Sign,
    DissSilica,
    TOC_Sign,
    TOC,
    DOC_Sign,
    DOC,
    TDS_Sign,
    TDS,
    TSS_Sign,
    TSS,
    VSS_Sign,
    VSS,
    Notes
  )

write_csv(data, file.path(path_data, "Delta_Integrated_WQ.csv"))


# Create metadata templates ---------------------------------------------------

# Below is a list of boiler plate function calls for creating metadata templates.
# They are meant to be a reminder and save you a little time. Remove the
# functions and arguments you don't need AND ... don't forget to read the docs!
# E.g. ?template_core_metadata

# Create core templates (required for all data packages)
# EMLassemblyline::template_core_metadata(
#   path = path_templates,
#   license = "CCBY",
#   file.type = ".docx"
# )

# Create provenance template
# EMLassemblyline::template_provenance(
#   path = path_templates,
# )

# Create table attributes template (required when data tables are present)
# EMLassemblyline::template_table_attributes(
#   path = path_templates,
#   data.path = path_data,
#   data.table = c("Delta_Integrated_WQ.csv", "Delta_Integrated_WQ_metadata.csv")
# )

# Create categorical variables template (required when attributes templates
# contains variables with a "categorical" class)
# EMLassemblyline::template_categorical_variables(
#   path = path_templates,
#   data.path = path_data
# )

# Create geographic coverage (required when more than one geographic location
# is to be reported in the metadata).
# >>> When stations are modified, added, or deleted, delete the
  # geographic_coverage.txt file, and run this function to re-generate it
EMLassemblyline::template_geographic_coverage(
  path = path_templates,
  data.path = path_data,
  data.table = "Delta_Integrated_WQ.csv",
  lat.col = "Latitude",
  lon.col = "Longitude",
  site.col = "Station"
)


# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create
# the EML.

# Custom function to create EML file for discretewq:
discretewq_eml <- function(edi_id, df_raw, chng_log) {
  # Create initial EML file
  eml_init <- EMLassemblyline::make_eml(
    path = path_templates,
    data.path = path_data,
    eml.path = path_eml,
    dataset.title = str_glue(
      "Six decades (1959-{end_yr}) of water quality in the upper San Francisco Estuary: an integrated database of 16 discrete monitoring surveys in the Sacramento San Joaquin Delta, Suisun Bay, Suisun Marsh, and San Francisco Bay",
      end_yr = as.character(max(year(df_raw$Date)))
    ),
    temporal.coverage = range(format(df_raw$Date, "%Y-%m-%d")),
    maintenance.description = "ongoing",
    data.table = c("Delta_Integrated_WQ.csv", "Delta_Integrated_WQ_metadata.csv"),
    data.table.description = c("Integrated water quality database", "Information on each survey included in the integrated database"),
    data.table.quote.character = c('"', '"'),
    # Fill this in with the URLs if you upload to box.com or similar - need a static link
    # Possibly add these URL's as function inputs if able to figure out the static link
    # data.table.url = c(
    #   "",
    #   ""
    # ),
    user.id = c("sbashevkin", "dbosworth"),
    user.domain = c("EDI", "EDI"),
    package.id = edi_id,
    return.obj = TRUE
  )

  # Convert changelog to class "emld" if it isn't already
  if (!all(class(chng_log) == c("emld", "list"))) class(chng_log) <- c("emld", "list")

  # Append changelog to EML file and rewrite
  eml_init$dataset$maintenance$changeHistory <- chng_log
  write_eml(eml_init, file.path(path_eml, paste0(edi_id, ".xml")))
}

# Create changelog file
changelog <- list(
  list(
    changeScope = "Metadata and data",
    oldValue = "See previous version (1)",
    changeDate = "2022-01-10",
    comment = "Fixed EMP timezone issue. In the previous (first) version of this dataset, we had imported EMP data assuming that times were recorded in local time (PST/PDT).
                However, the Environmental Monitoring Program (EMP) dataset records times in PST year-round. This version (2) has corrected that issue and all datetimes
                are now correctly in local Pacific time."
  ),
  list(
    changeScope = "Metadata and data",
    oldValue = "See previous version (2)",
    changeDate = "2022-03-10",
    comment = "1) Updated to newest versions of the source datasets.
                2) Added USGS_SFBS and EMP nutrient data.
                3) Added new surveys: Yolo Bypass Fish Monitoring Program, Stockton Dissolved Oxygen survey, Smelt Larva Survey, and USGS California Water Science Center monitoring data.
                4) Renamed the USGS survey to USGS_SFBS because of the addition of the other USGS survey: USGS_CAWSC."
  ),
  list(
    changeScope = "Metadata and data",
    oldValue = "See previous version (3)",
    changeDate = "2022-03-28",
    comment = "1) Fixed error with YBFMP Secchi Depth Data (it was previously in m instead of cm).
                2) Added 'Secchi_estimated' column from FMWT data"
  ),
  list(
    changeScope = "Metadata and data",
    oldValue = "See previous version (4)",
    changeDate = "2022-04-08",
    comment = "1) Fixed error with previous version where only the metadata file was uploaded and the full dataset was not included."
  ),
  list(
    changeScope = "Metadata and data",
    oldValue = "See previous version (5)",
    changeDate = Sys.Date(),
    comment = "1) Added temperature and conductivity to USGS_CAWSC.
                2) Added DO and pH data to all surveys that collect this data. USGS_SFBS collects both calculated (from a sensor) and discrete DO, so we used discrete DO up to 2016 and calculated DO afterwards to mirror the methodological change that occurred in the EMP survey in 2016.
                3) Fixed some historical data issues in EMP dataset.
                4) Added NCRO laboratory and water quality data.
                5) Added bottom conductivity to to 20mm, Baystudy, SDO, FMWT, and STN surveys.
                6) Added turbidity to EMP, FMWT, and STN surveys.
                7) Fixed timezones for SDO data. SDO times are reported in PST but had incorrectly been imported as local time (PST/PDT). Now, they are imported as Etc/GMT+8 and then converted to America/Los_Angeles to correspond to the other surveys.
                8) Updated STN, FMWT, EDSM, DJFMP, SLS, Suisun, EMP, USGS_SFBS, USGS_CAWSC, YBFMP, SKT, 20mm, baystudy, and SDO datasets.
                9) Removed rows from FMWT and STN datasets that did not contain any water quality information."
  )
)

# EDI Staging environment
# Request a new id from the EDI staging environment here:
# https://portal-s.edirepository.org/nis/reservations.jsp, OR
# use EDIutils functions below to create one programatically
ID_staging <- "" # enter EDI staging ID here manually if not using EDIutils to create one

EDIutils::login()
ID_staging <- create_reservation(scope = "edi", env = "staging")
EDIutils::logout()

# Create EML file for staging environment
discretewq_eml(ID_staging, data_raw, changelog)

# Validate EML file
eml_validate(file.path(path_eml, paste0(ID_staging, ".xml")))


# EDI Production environment
ID_prod <- "edi.731.7"

# Create EML file for production environment
discretewq_eml(ID_prod, data_raw, changelog)

# Validate EML file
eml_validate(file.path(path_eml, paste0(ID_prod, ".xml")))

