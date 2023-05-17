
# 20mm --------------------------------------------------------------------

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
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/metadataviewer?packageid=edi.535.4}{here}.
#' @seealso \code{\link{wq}}
"twentymm"


# Bay Study ---------------------------------------------------------------

#' Bay Study water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Bay Study.
#'
#' @encoding UTF-8
#' @format a tibble with 21,836 rows and 14 variables
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
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Conductivity_bottom}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at bottom.}
#'   }
#' @details More metadata and information on methods are available \href{http://www.dfg.ca.gov/delta/projects.asp?ProjectID=BAYSTUDY}{here}.
#' @seealso \code{\link{wq}}
"baystudy"


# DJFMP -------------------------------------------------------------------

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
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/metadataviewer?packageid=edi.244.9}{here}.
#' @seealso \code{\link{wq}}
"DJFMP"


# EDSM --------------------------------------------------------------------

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
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/metadataviewer?packageid=edi.415.8}{here}.
#' @seealso \code{\link{wq}}
"EDSM"


# EMP ---------------------------------------------------------------------

#' EMP water quality data
#'
#' Water quality data from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 17,366 rows and 68 variables
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
#'   \item{Chlorophyll_Sign}{Whether the Chlorophyll value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
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
#'   \item{TurbidityNTU}{Turbidity (NTU) at surface.}
#'   \item{TurbidityNTU_bottom}{Turbidity (NTU) at bottom.}
#'   \item{TurbidityFNU}{Turbidity (FNU) at surface.}
#'   \item{TurbidityFNU_bottom}{Turbidity (FNU) at bottom.}
#'   \item{Pheophytin_Sign}{Whether the Pheophytin value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{Pheophytin}{Pheophytin concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}).}
#'   \item{TotAlkalinity_Sign}{Whether the Total Alkalinity value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TotAlkalinity}{Total Alkalinity (mg/L).}
#'   \item{TotAmmonia_Sign}{Whether the Total Ammonia value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TotAmmonia}{Total Ammonia (mg/L).}
#'   \item{DissAmmonia_Sign}{Whether the Dissolved Ammonia value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissAmmonia}{Dissolved Ammonia (mg/L). If DissAmmonia_Sign is <, this is equal to the reporting limit, NA = RL unknown.}
#'   \item{DissBromide_Sign}{Whether the Dissolved Bromide value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissBromide}{Dissolved bromide (mg/L).}
#'   \item{DissCalcium_Sign}{Whether the Dissolved Calcium value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissCalcium}{Dissolved calcium (mg/L).}
#'   \item{TotChloride_Sign}{Whether the Total Chloride value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TotChloride}{Total chloride (mg/L).}
#'   \item{DissChloride_Sign}{Whether the Dissolved Chloride value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissChloride}{Dissolved chloride (mg/L).}
#'   \item{DissNitrateNitrite_Sign}{Whether the Dissolved Nitrate Nitrite value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate and Nitrite (mg/L). If DissNitrateNitrite_Sign is <, this is equal to the reporting limit, with NA = RL unknown.}
#'   \item{DOC_Sign}{Whether the DOC value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DOC}{Dissolved organic carbon (mg/L).}
#'   \item{TOC_Sign}{Whether the TOC value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TOC}{Total Organic Carbon (mg/L).}
#'   \item{DON_Sign}{Whether the DON value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DON}{Dissolved Organic Nitrogen (mg/L).}
#'   \item{TON_Sign}{Whether the Total Organic Nitrogen value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TON}{Total Organic Nitrogen (mg/L).}
#'   \item{DissOrthophos_Sign}{Whether the Dissolved Ortho-phosphate value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate (mg/L). If DissOrthophos_Sign is <, this is equal to the reporting limit, with NA = RL unknown.}
#'   \item{TotPhos_Sign}{Whether the Total Phosphate value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TotPhos}{Total phosphorous (mg/L).}
#'   \item{DissSilica_Sign}{Whether the Dissolved Silica value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissSilica}{Dissolved silica (mg/L).}
#'   \item{TDS_Sign}{Whether the Total Dissolved Solids value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TDS}{Total Dissolved Solids (mg/L).}
#'   \item{TSS_Sign}{Whether the TSS value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TSS}{Total Suspended Solids (mg/L).}
#'   \item{VSS_Sign}{Whether the VSS value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{VSS}{Volatile Suspended Solids (mg/L).}
#'   \item{TKN_Sign}{Whether the TKN value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TKN}{Total Kjeldahl Nitrogen (mg/L).}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/metadataviewer?packageid=edi.458.8}{here}.
#' @seealso \code{\link{wq}}; for more information on _Sign variables: [sign_variables]
"EMP"


# FMWT --------------------------------------------------------------------

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


# NCRO --------------------------------------------------------------------

#' NCRO water quality data
#'
#' Water quality data from the California Department of Water Resources North Central Region Office.
#'
#' @encoding UTF-8
#' @format a tibble with 10,350 rows and 49 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{DissolvedOxygen_bottom}{Dissolved oxygen (mg/L) at bottom.}
#'   \item{pH}{pH (dimensionless) at surface.}
#'   \item{TurbidityNTU}{Turbidity (NTU) at surface.}
#'   \item{TurbidityFNU}{Turbidity (FNU) at surface.}
#'   \item{TotAlkalinity_Sign}{Whether the Total Alkalinity value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TotAlkalinity}{Total Alkalinity (mg/L).}
#'   \item{DissAmmonia_Sign}{Whether the Dissolved Ammonia value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissAmmonia}{Dissolved Ammonia (mg/L). If DissAmmonia_Sign is <, this is equal to the reporting limit}
#'   \item{DissBromide_Sign}{Whether the Dissolved Bromide value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissBromide}{Dissolved bromide (mg/L). If DissBromide_Sign is <, this is equal to the reporting limit}
#'   \item{DissCalcium_Sign}{Whether the Dissolved Calcium value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissCalcium}{Dissolved calcium (mg/L). If DissCalcium_Sign is <, this is equal to the reporting limit}
#'   \item{DissChloride_Sign}{Whether the Dissolved chloride value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=". }
#'   \item{DissChloride}{Dissolved chloride (mg/L).}
#'   \item{Chlorophyll_Sign}{Whether the Chlorophyll value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "="}
#'   \item{Chlorophyll}{Chlorophyll concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}).}
#'   \item{Pheophytin_Sign}{Whether the Pheophytin is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{Pheophytin}{Pheophytin concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}).}
#'   \item{DissNitrateNitrite_Sign}{Whether the Dissolved Nitrate Nitrite value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissNitrateNitrite}{Dissolved Nitrate and Nitrite (mg/L). If DissNitrateNitrite_Sign is <, this is equal to the reporting limit}
#'   \item{DOC_Sign}{Whether the Dissolved Organic Carbon value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "="}
#'   \item{DOC}{Dissolved organic carbon (mg/L). If DOC_Sign is <, this is equal to the reporting limit}
#'   \item{TOC_Sign}{Whether the Total Organic Carbon value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "="}
#'   \item{TOC}{Total Organic Carbon (mg/L).If TOC_Sign is <, this is equal to the reporting limit}
#'   \item{DON_Sign}{Whether the Dissolved Organic Nitrate value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "="}
#'   \item{DON}{Dissolved Organic Nitrogen (mg/L).If DON_Sign is <, this is equal to the reporting limit}
#'   \item{DissOrthophos_Sign}{Whether the Dissolved Orthophos value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{DissOrthophos}{Dissolved Ortho-phosphate (mg/L). If DissOrthophos_Sign is <, this is equal to the reporting limit}
#'   \item{TotPhos_Sign}{Whether the Total Phosphate value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "="}
#'   \item{TotPhos}{Total phosphorous (mg/L). If TotPhos_Sign is <, this is equal to the reporting limit.}
#'   \item{TSS_Sign}{Whether the Total Suspended Solids value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "="}
#'   \item{TSS}{Total suspended solids (mg/L). If TSS_Sign is <, this is equal to the reporting limit}
#'   \item{VSS_Sign}{Whether the Volatile Suspended Solids value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "="}
#'   \item{VSS}{Volatile suspended solids (mg/L). If VSS_Sign is <, this is equal to the reporting limit}
#'   \item{TKN_Sign}{Whether the Total Kjeldahl Nitrogen value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=". "NA" indicates reporting limit unknown.}
#'   \item{TKN}{Total Kjeldahl Nitrogen (mg/L). IF TKN_Sign is <, this is equal to the reporting limit.}
#'   \item{TDS_Sign}{Whether the Total Dissolved Solids value is lower than reported ("<" because it is below the reporting limit and the reporting limit is used as the value), or reported as the measured value "=".}
#'   \item{TDS}{Total Dissolved Solids (mg/L).}
#'   }
#'
#' @details Contact Jared Frantzich Jared.Frantzich@water.ca.gov for more information.
#' @seealso \code{\link{wq}}; for more information on _Sign variables: [sign_variables]
"NCRO"


# SDO ---------------------------------------------------------------------

#' SDO water quality data
#'
#' Water quality data from the California Department of Water Resources Stockton Dissolved Oxygen monitoring.
#'
#' @encoding UTF-8
#' @format a tibble with 3,112 rows and 16 variables
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
#'   \item{Conductivity_bottom}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at bottom.}
#'   \item{DissolvedOxygen}{Dissolved oxygen (mg/L) at surface.}
#'   \item{DissolvedOxygen_bottom}{Dissolved oxygen (mg/L) at bottom.}
#'   \item{pH}{pH (dimensionless) at surface.}
#'   \item{pH_bottom}{pH (dimensionless) at bottom.}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/metadataviewer?packageid=edi.276.2}{here}.
#' @seealso \code{\link{wq}}
"SDO"


# SKT ---------------------------------------------------------------------

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
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/metadataviewer?packageid=edi.527.4}{here}.
#' @seealso \code{\link{wq}}
"SKT"


# SLS ---------------------------------------------------------------------

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
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/metadataviewer?packageid=edi.534.4}{here}.
#' @seealso \code{\link{wq}}
"SLS"


# STN ---------------------------------------------------------------------

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


# Suisun ------------------------------------------------------------------

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


# USBR --------------------------------------------------------------------

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


# USGS CAWSC --------------------------------------------------------------

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
#' @details More metadata and information on methods are available \href{https://doi.org/10.5066/F7P55KJN}{here for data} and \href{https://help.waterdata.usgs.gov/codes-and-parameters}{here for metadata}.
#' @seealso \code{\link{wq}}; for more information on _Sign variables: [sign_variables]
"USGS_CAWSC"


# USGS SFBS ---------------------------------------------------------------

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
#' @details More metadata and information on methods are available \href{https://doi.org/10.5066/F7TQ5ZPR}{here for data from 1969-2015} and \href{https://doi.org/10.5066/F7D21WGF}{here for data from 2016-2019}.
#' @seealso \code{\link{wq}}
"USGS_SFBS"


# YBFMP -------------------------------------------------------------------

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
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/metadataviewer?packageid=edi.494.2}{here} and \href{https://portal.edirepository.org/nis/metadataviewer?packageid=edi.233.3}{here}.
#' @seealso \code{\link{wq}}
"YBFMP"


# Internal Documentation --------------------------------------------------

#' @title Sign Variables
#' @description For the variables that have the _Sign suffix, the symbols in
#'   these variables represent three conditions of the value contained in its
#'   corresponding result variable:
#' * "<" - The value is below the Reporting Limit (RL) with the value in the
#' corresponding result variable equal to the RL. An `NA` value in the
#' corresponding result variable indicates that the RL value is unknown.
#' * "=" - The value is above the RL with the value in the corresponding result
#' variable equal to the actual value measured by the laboratory. An `NA` value
#' in the corresponding result variable indicates that the value is missing or
#' wasn't collected.
#' * "~" - The value in the corresponding result variable was estimated by the laboratory.
#' @name sign_variables
#' @keywords internal
NULL
