#' Process and combine water quality data
#'
#' Imports, filters, and processes water quality datasets and outputs an integrated dataset
#' @param Sources Character vector of data sources for the water quality variables. No default, this must be specified.
#'   Choices include "20mm" (20mm Survey, \code{\link{twentymm}}),
#'   "Baystudy" (Bay Study, \code{\link{baystudy}}),
#'   "DJFMP" (Delta Juvenile Fish Monitoring Program, \code{\link{DJFMP}}),
#'   "EDSM" (Enhanced Delta Smelt Monitoring, \code{\link{EDSM}}),
#'   "EMP" (Environmental Monitoring Program, \code{\link{EMP}}),
#'   "FMWT" (Fall Midwater Trawl, \code{\link{FMWT}}),
#'   "NCRO" (NCRO, \code{\link{NCRO}}),
#'   "SDO" (Stockton Dissolved Oxygen Survey, \code{\link{SDO}}),
#'   "SKT" (Spring Kodiak Trawl, \code{\link{SKT}}),
#'   "SLS" (Smelt Larva Survey, \code{\link{SLS}}),
#'   "STN" (Summer Townet Survey, \code{\link{STN}}),
#'   "Suisun" (Suisun Marsh Fish Study, \code{\link{suisun}}),
#'   "USBR" (United States Bureau of Reclamation Sacramento Deepwater Ship Channel data, \code{\link{USBR}}),
#'   "USGS_CAWSC" (USGS California Water Science Center monitoring data, \code{\link{USGS_CAWSC}}),
#'   "USGS_SFBS" (USGS San Francisco Bay Surveys, \code{\link{USGS_SFBS}}), and
#'   "YBFMP" (Yolo Bypass Fish Monitoring Program, \code{\link{YBFMP}}).
#' @param Start_year Earliest year you would like included in the dataset. Must be an integer. Defaults to year \code{0}.
#' @param End_year Latest year you would like included in the dataset. Must be an integer. Defaults to the current year.
#' @importFrom magrittr %>%
#' @importFrom dplyr .data
#' @return An integrated dataset
#' @examples
#' Data <- wq(
#'   Sources = c(
#'     "20mm",
#'     "Baystudy",
#'     "DJFMP",
#'     "EDSM",
#'     "EMP",
#'     "FMWT",
#'     "NCRO",
#'     "SDO",
#'     "SKT",
#'     "SLS",
#'     "STN",
#'     "Suisun",
#'     "USBR",
#'     "USGS_CAWSC",
#'     "USGS_SFBS",
#'     "YBFMP"
#'   )
#' )
#' @export

wq <- function(Sources = NULL,
               Start_year = NULL,
               End_year = NULL) {

  # Check arguments ---------------------------------------------------------

  if("USGS" %in% Sources) {
    stop('The "USGS" data source has been renamed to "USGS_SFBS" because of the inclusion of an additional USGS dataset, "USGS_CAWSC".')
  }

  sources_expect <- c(
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

  if(is.null(Sources) | !all(Sources %in% sources_expect)) {
    stop(paste0("You must specify the data sources you wish to include. Choices include: ", stringr::str_c(sources_expect, collapse = ", ")))
  }

  # Set end year to current year if blank
  if(is.null(End_year)) {
    End_year <- as.numeric(format(Sys.Date(), "%Y"))
  }

  # Set start year to 0 if blank
  if(is.null(Start_year)) {
    Start_year <- 0
  }

  # Load and combine data ---------------------------------------------------

  WQ_list <- list()

  if ("20mm" %in% Sources) {
    WQ_list[["twentymm"]] <- discretewq::twentymm
  }

  if ("Baystudy" %in% Sources) {
    WQ_list[["Baystudy"]] <- discretewq::baystudy
  }

  if ("DJFMP" %in% Sources) {
    WQ_list[["DJFMP"]] <- discretewq::DJFMP
  }

  if ("EDSM" %in% Sources) {
    WQ_list[["EDSM"]] <- discretewq::EDSM
  }

  if ("EMP" %in% Sources) {
    WQ_list[["EMP"]] <- discretewq::EMP
  }

  if ("FMWT" %in% Sources) {
    WQ_list[["FMWT"]] <- discretewq::FMWT
  }

  if ("NCRO" %in% Sources) {
    WQ_list[["NCRO"]] <- discretewq::NCRO
  }

  if ("SDO" %in% Sources) {
    WQ_list[["SDO"]] <- discretewq::SDO
  }

  if ("SKT" %in% Sources) {
    WQ_list[["SKT"]] <- discretewq::SKT
  }

  if ("SLS" %in% Sources) {
    WQ_list[["SLS"]] <- discretewq::SLS
  }

  if ("STN" %in% Sources) {
    WQ_list[["STN"]] <- discretewq::STN
  }

  if ("Suisun" %in% Sources) {
    WQ_list[["Suisun"]] <- discretewq::suisun
  }

  if ("USBR" %in% Sources) {
    WQ_list[["USBR"]] <- discretewq::USBR
  }

  if ("USGS_CAWSC" %in% Sources) {
    WQ_list[["USGS_CAWSC"]] <- discretewq::USGS_CAWSC
  }

  if ("USGS_SFBS" %in% Sources) {
    WQ_list[["USGS_SFBS"]] <- discretewq::USGS_SFBS
  }

  if ("YBFMP" %in% Sources) {
    WQ_list[["YBFMP"]] <- discretewq::YBFMP
  }


  out <- dplyr::bind_rows(WQ_list) %>%
    dplyr::mutate(
      MonthYear = lubridate::floor_date(.data$Date, unit = "month"),
      Year = lubridate::year(.data$Date),
      StationID = paste(.data$Source, .data$Station),
      Month = lubridate::month(.data$MonthYear)
    ) %>%
    dplyr::filter(.data$Year >= Start_year & .data$Year <= End_year) %>%
    dplyr::mutate(
      Season = dplyr::case_when(
        .data$Month %in% c(12, 1, 2) ~ "Winter",
        .data$Month %in% c(3, 4, 5) ~ "Spring",
        .data$Month %in% c(6, 7, 8) ~ "Summer",
        .data$Month %in% c(9, 10, 11) ~ "Fall"
      )
    ) %>%
    {
      if ("Field_coords" %in% names(.)) {
        dplyr::mutate(., Field_coords = tidyr::replace_na(.data$Field_coords, FALSE))
      } else {
        .
      }
    } %>%
    {
      if ("Conductivity" %in% names(.)) {
        if ("Salinity" %in% names(.)) {
          dplyr::mutate(., Salinity = dplyr::if_else(is.na(.data$Salinity), wql::ec2pss(.data$Conductivity / 1000, t = 25), .data$Salinity))
        } else {
          dplyr::mutate(., Salinity = wql::ec2pss(.data$Conductivity / 1000, t = 25))
        }
      } else {
        if ("Salinity" %in% names(.)) {
          .
        } else {
          dplyr::mutate(., Salinity = NA_real_)
        }
      }
    } %>%
    {
      if ("Conductivity_bottom" %in% names(.)) {
        if ("Salinity_bottom" %in% names(.)) {
          dplyr::mutate(., Salinity_bottom = dplyr::if_else(is.na(.data$Salinity_bottom), wql::ec2pss(.data$Conductivity_bottom / 1000, t = 25), .data$Salinity_bottom))
        } else {
          dplyr::mutate(., Salinity_bottom = wql::ec2pss(.data$Conductivity_bottom / 1000, t = 25))
        }
      } else {
        if ("Salinity_bottom" %in% names(.)) {
          .
        } else {
          dplyr::mutate(., Salinity_bottom = NA_real_)
        }
      }
    }

  # Return ------------------------------------------------------------------

  return(out)
}
