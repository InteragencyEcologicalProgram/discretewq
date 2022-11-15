#' 20mm water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife 20mm survey.
#'
#' @encoding UTF-8
#' @format a tibble with 10,469 rows and 14 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Field_coords}{Were lat/long coordinates collected in the field (TRUE), or do they represent the location of a fixed station (FALSE)?}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Conductivity_bottom}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at bottom.}
#'   \item{Notes}{Comments.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/20mm-Survey}{here}.
#' @seealso \code{\link{wq}}
"twentymm"

#' Bay Study water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Bay Study.
#'
#' @encoding UTF-8
#' @format a tibble with 21,264 rows and 12 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   }
#' @details More metadata and information on methods are available \href{http://www.dfg.ca.gov/delta/projects.asp?ProjectID=BAYSTUDY}{here}.
#' @seealso \code{\link{wq}}
"baystudy"

#' EDSM water quality data
#'
#' Water quality data from the United States Fish and Wildlife Service Enhanced Delta Smelt Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 30,957 rows and 14 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected. This represents an identifier for the unique EDSM target location. Multiple tows (and water quality samples) were often collected at each target location on a day.}
#'   \item{Latitude}{Latitude in decimal degrees. This is the actual latitude of the sample collection, not the latitude of the target location.}
#'   \item{Longitude}{Longitude in decimal degrees. This is the actual longitude of the sample collection, not the longitude of the target location.}
#'   \item{Field_coords}{Were lat/long coordinates collected in the field (TRUE), or do they represent the location of a fixed station (FALSE)?}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature in °C.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{DissolvedOxygen_bottom}{Dissolved oxygen (mg/L) at bottom.}
#'   \item{Notes}{Comments.}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.415.1}{here}.
#' @seealso \code{\link{wq}}
"EDSM"

#' EMP water quality data
#'
#' Water quality data from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 17,037 rows and 56 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Field_coords}{Were lat/long coordinates collected in the field (TRUE), or do they represent the location of a fixed station (FALSE)?}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Notes}{Notes or comments.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage (always High Slack).}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Chlorophyll_Sign}{Whether the Chlorophyll value is below the reporting limit or equal to it.}
#'   \item{Chlorophyll}{Chlorophyll concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}).}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Conductivity_bottom}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at bottom.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{DissolvedOxygen_bottom}{Dissolved oxygen (mg/L) at bottom.}
#'   \item{DissolvedOxygenPercent}{Dissolved oxygen percent (dimensionless) at surface.}
#'   \item{DissolvedOxygenPercent_bottom}{Dissolved oxygen percent (dimensionless) at bottom.}
#'   \item{pH}{pH (dimensionless) at surface.}
#'   \item{pH_bottom}{pH (dimensionless) at bottom.}
#'   \item{TotAlkalinity}{Total Alkalinity (mg/L).}
#'   \item{TotAmmonia}{Total ammonia (mg/L).}
#'   \item{DissAmmonia_Sign}{Whether the Dissolved Ammonia value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissAmmonia}{Dissolved Ammonia (mg/L). If DissAmmonia_Sign is <, this is equal to the reporting limit, NA = RL unknown.}
#'   \item{DissBromide_Sign}{Whether the Dissolved Bromide value is below the reporting limit or equal to it.}
#'   \item{DissBromide}{Dissolved bromide (mg/L).}
#'   \item{DissCalcium_Sign}{Whether the Dissolved Calcium value is below the reporting limit or equal to it.}
#'   \item{DissCalcium}{Dissolved calcium (mg/L).}
#'   \item{TotChloride}{Total chloride (mg/L).}
#'   \item{DissChloride}{Dissolved chloride (mg/L).}
#'   \item{DissNitrateNitrite_Sign}{Whether the Dissolved Nitrate Nitrite value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate and Nitrite (mg/L). If DissNitrateNitrite_Sign is <, this is equal to the reporting limit, with NA = RL unknown.}
#'   \item{DOC_Sign}{Whether the Dissolved Organic Carbon value is below the reporting limit or equal to it.}
#'   \item{DOC}{Dissolved organic carbon (mg/L).}
#'   \item{TOC_Sign}{Whether the Total Organic Carbon value is below the reporting limit or equal to it.}
#'   \item{TOC}{Total Organic Carbon (mg/L).}
#'   \item{DON_Sign}{Whether the Dissolved Organic Nitrate value is below the reporting limit or equal to it.}
#'   \item{DON}{Dissolved Organic Nitrogen (mg/L).}
#'   \item{TON}{Total Organic Nitrogen (mg/L).}
#'   \item{DissOrthophos_Sign}{Whether the Dissolved Ortho-phosphate value is below the reporting limit or equal to it.}
#'   \item{TOC}{Total organic carbon (mg/L).}
#'   \item{DON}{Dissolved organic nitrogen (mg/L).}
#'   \item{TON}{Total organic nitrogen (mg/L).}
#'   \item{DissOrthophos_Sign}{Whether the Dissolved Orthophos value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate (mg/L). If DissOrthophos_Sign is <, this is equal to the reporting limit, with NA = RL unknown.}
#'   \item{TotPhos_Sign}{Whether the Total Phosphate value is below the reporting limit or equal to it.}
#'   \item{TotPhos}{Total phosphorous (mg/L).}
#'   \item{DissSilica_Sign}{Whether the Dissolved Silica value is below the reporting limit or equal to it.}
#'   \item{DissSilica}{Dissolved silica (mg/L).}
#'   \item{TDS}{Total dissolved solids (mg/L).}
#'   \item{TSS_Sign}{Whether the Total Suspended Solids value is below the reporting limit or equal to it.}
#'   \item{TSS}{Total suspended solids (mg/L).}
#'   \item{VSS_Sign}{Whether the Volatile Suspended Solids value is below the reporting limit or equal to it.}
#'   \item{VSS}{Volatile suspended solids (mg/L).}
#'   \item{TKN_Sign}{Whether the Total Kjeldahl Nitrogen value is below the reporting limit or equal to it.}
#'   \item{TKN}{Total Kjeldahl Nitrogen (mg/L).}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.458.6}{here}.
#' @seealso \code{\link{wq}}
"EMP"

#' DJFMP water quality data
#'
#' Water quality data from the United States Fish and Wildlife Service Delta Juvenile Fish Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 150,488 rows and 9 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature in °C.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.244.3}{here}.
#' @seealso \code{\link{wq}}
"DJFMP"

#' FMWT water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Fall Midwater Trawl.
#'
#' @encoding UTF-8
#' @format a tibble with 29,579 rows and 13 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Secchi_estimated}{Was Secchi depth estimated?}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   }
#' @details More metadata and information on methods are available \href{https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT}{here}.
#' @seealso \code{\link{wq}}
"FMWT"

#' SDO water quality data
#'
#' Water quality data from the California Department of Water Resources Stockton Dissolved Oxygen monitoring.
#'
#' @encoding UTF-8
#' @format a tibble with 3,108 rows and 11 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{DissolvedOxygen_bottom}{Dissolved oxygen (mg/L) at bottom.}
#'   \item{pH}{pH (dimensionless) at surface.}
#'   \item{pH_bottom}{pH (dimensionless) at bottom.}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.276.2}{here}.
#' @seealso \code{\link{wq}}
"SDO"

#' SKT water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Spring Kodiak Trawl.
#'
#' @encoding UTF-8
#' @format a tibble with 4,505 rows and 13 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Field_coords}{Were lat/long coordinates collected in the field (TRUE), or do they represent the location of a fixed station (FALSE)?}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Notes}{Comments.}
#'   }
#' @details More metadata and information on methods are available \href{http://www.dfg.ca.gov/delta/projects.asp?ProjectID=SKT}{here}.
#' @seealso \code{\link{wq}}
"SKT"

#' SLS water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Smelt Larva Survey.
#'
#' @encoding UTF-8
#' @format a tibble with 2,889 rows and 12 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Notes}{Comments.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Smelt-Larva-Survey}{here}.
#' @seealso \code{\link{wq}}
"SLS"

#' STN water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Summer Townet Survey.
#'
#' @encoding UTF-8
#' @format a tibble with 8,978 rows and 14 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Notes}{Comments.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Townet-Survey}{here}.
#' @seealso \code{\link{wq}}
"STN"

#' Suisun water quality data
#'
#' Water quality data from the UC Davis Suisun Marsh Fish Study.
#'
#' @encoding UTF-8
#' @format a tibble with 14,206 rows and 11 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{DissolvedOxygenPercent}{Dissolved oxygen percent (dimensionless) at surface.}
#'   }
#' @details More metadata and information on methods are available \href{https://watershed.ucdavis.edu/project/suisun-marsh-fish-study}{here}.
#' @seealso \code{\link{wq}}
"suisun"

#' USBR water quality data
#'
#' Water quality data from the United States Bureau of Reclamation Sacramento Deepwater Ship Channel cruises.
#'
#' @encoding UTF-8
#' @format a tibble with 904 rows and 13 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m). Only 1 value per station, probably an average?}
#'   \item{Sample_depth_surface}{Depth (m) of surface sample.}
#'   \item{Sample_depth_bottom}{Depth (m) of bottom sample.}
#'   \item{Chlorophyll}{Chlorophyll concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   }
#' @seealso \code{\link{wq}}
"USBR"

#' USGS SFBS water quality data
#'
#' Water quality data from the United States Geological Survey San Francisco Bay Water Quality Survey.
#'
#' @encoding UTF-8
#' @format a tibble with 23,923 rows and 22 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection. Reported as an average
#'     when the collection times varied among the the surface and bottom WQ and
#'     nutrient parameters.}
#'   \item{Sample_depth_surface}{Depth (m) of surface sample. Reported as an
#'     average when surface depths varied among the WQ parameters.}
#'   \item{Sample_depth_bottom}{Depth (m) of bottom sample. Reported as an
#'     average when bottom depths varied among the WQ parameters.}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Salinity}{Salinity at surface.}
#'   \item{Salinity_bottom}{Salinity at bottom.}
#'   \item{Chlorophyll}{Chlorophyll concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}) at surface.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{DissolvedOxygen_bottom}{Dissolved oxygen (mg/L) at bottom.}
#'   \item{DissolvedOxygenPercent}{Dissolved oxygen percent (dimensionless) at surface.}
#'   \item{DissolvedOxygenPercent_bottom}{Dissolved oxygen percent (dimensionless) at bottom.}
#'   \item{Sample_depth_nutr_surface}{Depth (m) paired w/ nutrient sampling
#'     (range: 0-5 m). Reported as an average when surface depths varied among the
#'     nutrient parameters.}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate and Nitrite (mg/L).}
#'   \item{DissAmmonia}{Dissolved Ammonia (mg/L).}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate (mg/L).}
#'   \item{DissSilica}{Dissolved Silica (mg/L).}
#'   }
#' @details More metadata and information on methods are available \href{https://www.sciencebase.gov/catalog/item/5841f97ee4b04fc80e518d9f}{here for data from 1969-2015} and \href{https://www.sciencebase.gov/catalog/item/5966abe6e4b0d1f9f05cf551}{here for data from 2016-2019}.
#' @seealso \code{\link{wq}}
"USGS_SFBS"

#' USGS CAWSC water quality data
#'
#' Discrete water quality data from the USGS California Water Science Center
#'
#' @encoding UTF-8
#' @format a tibble with 16,751 rows and 19 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Chlorophyll_Sign}{Whether the Chlorophyll value is estimated (extrapolated at low end) or reported as measured.}
#'   \item{Chlorophyll}{Chlorophyll concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}).}
#'   \item{DissAmmonia_Sign}{Whether the Dissolved Ammonia value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), estimated "~", or reported as the measured value "=".}
#'   \item{DissAmmonia}{Dissolved Ammonia (mg/L). If DissAmmonia_Sign is <, this is equal to the reporting limit, NA = RL unknown.}
#'   \item{DissNitrateNitrite_Sign}{Whether the Dissolved Nitrate Nitrite value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), estimated "~", or reported as the measured value "=".}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate and Nitrite (mg/L)}
#'   \item{DOC}{Dissolved Organic Carbon (mg/L)}
#'   \item{DissOrthophos_Sign}{Whether the Dissolved Orthophosphate value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), estimated "~", or reported as the measured value "=".}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate (mg/L)}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{pH}{pH (dimensionless) at surface.}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   }
#' @details More metadata and information on methods are available \href{https://nwis.waterdata.usgs.gov/usa/nwis/qwdata}{here for data} and \href{https://help.waterdata.usgs.gov/codes-and-parameters}{here for metadata}.
#' @seealso \code{\link{wq}}
"USGS_CAWSC"

#' YBFMP water quality data
#'
#' Water quality data from the California Department of Water Resources Yolo Bypass Fish Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 8,883 rows and 14 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Tide}{Tidal stage ('overtopping' refers to periods of floodplain inundation that drown out tidal effects).}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{pH}{pH (dimensionless) at surface.}
#'   \item{Notes}{Notes or comments.}
#'   }
#'
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.494.2}{here} and \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.233.3}{here}.
#' @seealso \code{\link{wq}}
"YBFMP"

#' NCRO Chlorophyll data
#'
#' Chlorophyll from the South Delta collected by DWR's North Central Region Office
#'
#' @encoding UTF-8
#' @format a tibble with 4,820 rows and 9 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Secchi}{Secchi depth (cm).}d
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{DissolvedOxygen_bottom}{Dissolved oxygen (mg/L) at bottom.}
#'   \item{pH}{pH (dimensionless) at surface.}
#'   \item{TotAlkalinity}{Total Alkalinity (mg/L).}
#'   \item{DissAmmonia_Sign}{Whether the Dissolved Ammonia value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissAmmonia}{Dissolved Ammonia (mg/L). If DissAmmonia_Sign is <, this is equal to the reporting limit, NA = RL unknown.}
#'   \item{DissBromide_Sign}{Whether the Dissolved Bromide value is below the reporting limit or equal to it.}
#'   \item{DissBromide}{Dissolved bromide (mg/L).}
#'   \item{DissCalcium_Sign}{Whether the Dissolved Calcium value is below the reporting limit or equal to it.}
#'   \item{DissCalcium}{Dissolved calcium (mg/L).}
#'   \item{DissChloride}{Dissolved chloride (mg/L).}
#'   \item{DissNitrateNitrite_Sign}{Whether the Dissolved Nitrate Nitrite value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate and Nitrite (mg/L). If DissNitrateNitrite_Sign is <, this is equal to the reporting limit, with NA = RL unknown.}
#'   \item{DOC_Sign}{Whether the Dissolved Organic Carbon value is below the reporting limit or equal to it.}
#'   \item{DOC}{Dissolved organic carbon (mg/L).}
#'   \item{TOC_Sign}{Whether the Total Organic Carbon value is below the reporting limit or equal to it.}
#'   \item{TOC}{Total Organic Carbon (mg/L).}
#'   \item{DON_Sign}{Whether the Dissolved Organic Nitrate value is below the reporting limit or equal to it.}
#'   \item{DON}{Dissolved Organic Nitrogen (mg/L).}
#'   \item{TON}{Total Organic Nitrogen (mg/L).}
#'   \item{DissOrthophos_Sign}{Whether the Dissolved Ortho-phosphate value is below the reporting limit or equal to it.}
#'   \item{TOC}{Total organic carbon (mg/L).}
#'   \item{DON}{Dissolved organic nitrogen (mg/L).}
#'   \item{TON}{Total organic nitrogen (mg/L).}
#'   \item{DissOrthophos_Sign}{Whether the Dissolved Orthophos value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate (mg/L). If DissOrthophos_Sign is <, this is equal to the reporting limit, with NA = RL unknown.}
#'   \item{TotPhos_Sign}{Whether the Total Phosphate value is below the reporting limit or equal to it.}
#'   \item{TotPhos}{Total phosphorous (mg/L).}
#'   \item{TSS_Sign}{Whether the Total Suspended Solids value is below the reporting limit or equal to it.}
#'   \item{TSS}{Total suspended solids (mg/L).}
#'   \item{VSS_Sign}{Whether the Volatile Suspended Solids value is below the reporting limit or equal to it.}
#'   \item{VSS}{Volatile suspended solids (mg/L).}
#'   \item{TKN_Sign}{Whether the Total Kjeldahl Nitrogen value is below the reporting limit or equal to it.}
#'   \item{TKN}{Total Kjeldahl Nitrogen (mg/L).}
#'   }
#'
#' @details Contact Jared Frantzich Jared.Frantzich@water.ca.gov for more information.
#' @seealso \code{\link{wq}}
"NCRO"

