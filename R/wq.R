#' Process and combine water quality data
#'
#' Imports, filters, and processes water quality datasets and outputs an integrated dataset
#' @param Start_year Earliest year you would like included in the dataset. Must be an integer. Defaults to \code{1950}.
#' @param End_year Latest year you would like included in the dataset. Must be an integer. Defaults to \code{2020}.
#' @param Sources Character vector of data sources for the water quality variables.
#'   Choices include "EMP" (Environmental Monitoring Program, \code{\link{EMP}}),
#'   "STN" (Summer Townet Survey, \code{\link{STN}}),
#'   "FMWT" (Fall Midwater Trawl, \code{\link{FMWT}}),
#'   "EDSM" (Enhanced Delta Smelt Monitoring, \code{\link{EDSM}}),
#'   "DJFMP" (Delta Juvenile Fish Monitoring Program, \code{\link{DJFMP}}),
#'   "20mm" (20mm Survey, \code{\link{twentymm}}),
#'   "SKT" (Spring Kodiak Trawl, \code{\link{SKT}}),
#'   "Baystudy" (Bay Study, \code{\link{baystudy}}),
#'   "USGS" (USGS San Francisco Bay Surveys, \code{\link{USGS}}),
#'   "USBR" (United States Bureau of Reclamation Sacramento Deepwater Ship Channel data, \code{\link{USBR}}), and
#'   "Suisun" (Suisun Marsh Fish Study, \code{\link{suisun}}).
#' @param Shapefile Shapefile you would like used to define regions in the dataset. Must be in \code{\link[sf]{sf}} format, e.g., imported with \code{\link[sf]{st_read}}. Defaults to \code{\link[deltamapr]{R_EDSM_Strata_1819P1}}.
#' @param Region_column Quoted name of the column in the Shapefile with the region designations.
#' @param Regions Character vector of regions to be included in the dataset. Must correspond with levels of the \code{Region_column}. To include all data points regardless of whether they correspond to a region in the \code{Shapefile} set \code{Regions = NULL}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return An integrated dataset
#' @examples
#' Data <- wq(Sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "SKT",
#' "20mm", "Suisun", "Baystudy", "USBR", "USGS"),
#' Regions = NULL)
#' @export

wq <- function(Start_year=1950,
               End_year=2020,
                       Sources = c("EMP", "STN", "FMWT", "EDSM"),
                       Shapefile = deltamapr::R_EDSM_Strata_1819P1,
                       Region_column = "Stratum",
                       Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta")){

  Region_column <- rlang::sym(Region_column)
  Region_column <- rlang::enquo(Region_column)

  # Stations ----------------------------------------------------------------

  Stations<-discretewq::stations%>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs=4326,
                 remove=FALSE)%>%
    sf::st_transform(crs=sf::st_crs(Shapefile))%>%
    sf::st_join(Shapefile%>%
                  dplyr::select(!!Region_column))%>%
    tibble::as_tibble()%>%
    dplyr::select(-.data$geometry)%>%
    dplyr::rename(Region=!!Region_column)


    # Load and combine data ---------------------------------------------------
    WQ_list<-list()

    if("FMWT"%in%Sources){
      WQ_list[["FMWT"]]<-discretewq::FMWT
    }

    if("Baystudy"%in%Sources){
      WQ_list[["Baystudy"]]<-discretewq::baystudy
    }

    if("STN"%in%Sources){
      WQ_list[["STN"]]<-discretewq::STN
    }

    if("Suisun"%in%Sources){
      WQ_list[["Suisun"]]<-discretewq::suisun
    }

    if("SKT"%in%Sources){
      WQ_list[["SKT"]]<-discretewq::SKT
    }

    if("20mm"%in%Sources){
      WQ_list[["twentymm"]]<-discretewq::twentymm
    }

    if("EDSM"%in%Sources){
      WQ_list[["EDSM"]]<-discretewq::EDSM
    }

    if("DJFMP"%in%Sources){
      WQ_list[["DJFMP"]]<-discretewq::DJFMP
    }

    if("EMP"%in%Sources){
      WQ_list[["EMP"]]<-discretewq::EMP
    }

    if("USBR"%in%Sources){
      WQ_list[["USBR"]]<-discretewq::USBR
    }

    if("USGS"%in%Sources){
      WQ_list[["USGS"]]<-discretewq::USGS
    }

    out<-dplyr::bind_rows(WQ_list)%>%
      dplyr::mutate(MonthYear=lubridate::floor_date(.data$Date, unit = "month"),
                    Year=lubridate::year(.data$Date),
                    StationID=paste(.data$Source, .data$Station))%>%
      {if("Conductivity"%in%names(.)){
        if("Salinity"%in%names(.)){
          dplyr::mutate(., Salinity=dplyr::if_else(is.na(.data$Salinity), wql::ec2pss(.data$Conductivity/1000, t=25), .data$Salinity))
        } else{
          dplyr::mutate(., Salinity=wql::ec2pss(.data$Conductivity/1000, t=25))
        }

      } else{
        if("Salinity"%in%names(.)){
          .
        } else{
          dplyr::mutate(., Salinity=NA_real_)
        }
      }}%>%
      {if("Latitude"%in%names(.)){
        .
      } else{
        dplyr::mutate(., Latitude=NA_real_, Longitude=NA_real_)
      }}




    # Add regions and summarize -------------------------------------------------------------

    #Add regions and lat/long to dataset
    out<-dplyr::filter(out, .data$StationID%in%Stations$StationID)%>%
      dplyr::select(-.data$Latitude, -.data$Longitude)%>%
      dplyr::left_join(Stations, by=c("Source", "Station", "StationID"))%>%
      {if(any(!is.na(out$Latitude))){
        dplyr::bind_rows(., dplyr::filter(out, !(.data$StationID%in%Stations$StationID) & !is.na(.data$Latitude) & !is.na(.data$Longitude))%>%
                           sf::st_as_sf(coords = c("Longitude", "Latitude"), #Add regions to EDSM data
                                        crs=4326,
                                        remove = FALSE)%>%
                           sf::st_transform(crs=sf::st_crs(Shapefile))%>%
                           sf::st_join(Shapefile%>%
                                         dplyr::select(!!Region_column))%>%
                           tibble::as_tibble()%>%
                           dplyr::select(-.data$geometry)%>%
                           dplyr::rename(Region=!!Region_column)%>%
                           dplyr::mutate(Field_coords=TRUE))

      } else{
        .
      }}%>%
      dplyr::filter(lubridate::year(.data$MonthYear)>=Start_year & lubridate::year(.data$MonthYear)<=End_year)%>%
      {if (is.null(Regions)){
        dplyr::bind_rows(., dplyr::filter(out, !(.data$StationID%in%Stations$StationID) & (is.na(.data$Latitude) | is.na(.data$Longitude))))
      } else{
        dplyr::filter(., .data$Region%in%Regions)
      }}%>%
      dplyr::mutate(Month=lubridate::month(.data$MonthYear))%>%
      {if ("Field_coords"%in%names(.)){
        dplyr::mutate(., Field_coords = tidyr::replace_na(.data$Field_coords, FALSE))
      } else{
        .
      }}%>%
      dplyr::mutate(Season=dplyr::case_when(
        .data$Month%in%c(12,1,2) ~ "Winter",
        .data$Month%in%c(3,4,5) ~ "Spring",
        .data$Month%in%c(6,7,8) ~ "Summer",
        .data$Month%in%c(9,10,11) ~ "Fall"),
        Year=dplyr::if_else(.data$Month==12, .data$Year-1, .data$Year)
      )


  # Return ------------------------------------------------------------------

  return(out)
}
