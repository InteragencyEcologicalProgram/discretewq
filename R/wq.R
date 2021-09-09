#' Process and combine water quality data
#'
#' Imports, filters, and processes water quality datasets and outputs an integrated dataset
#' @param Sources Character vector of data sources for the water quality variables. No default, this must be specified.
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
#' @param Start_year Earliest year you would like included in the dataset. Must be an integer. Defaults to year \code{0}.
#' @param End_year Latest year you would like included in the dataset. Must be an integer. Defaults to the current year.
#' @importFrom magrittr %>%
#' @importFrom dplyr .data
#' @return An integrated dataset
#' @examples
#' Data <- wq(Sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "SKT",
#' "20mm", "Suisun", "Baystudy", "USBR", "USGS"))
#' @export

wq <- function(Sources=NULL,
               Start_year=NULL,
               End_year=NULL){


# Check arguments ---------------------------------------------------------

if(is.null(Sources) | !all(Sources%in%c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "SKT",
                                        "20mm", "Suisun", "Baystudy", "USBR", "USGS"))){
  stop('You must specify the data sources you wish to include. Choices include
  c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "SKT", "20mm", "Suisun", "Baystudy", "USBR", "USGS")')
}

  # Set end year to current year if blank
  if(is.null(End_year)){
    End_year<-as.numeric(format(Sys.Date(), "%Y"))
  }

  # Set start year to 0 if blank
  if(is.null(Start_year)){
    Start_year<-0
  }

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
                  StationID=paste(.data$Source, .data$Station),
                  Month=lubridate::month(.data$MonthYear))%>%
    dplyr::filter(.data$Year>=Start_year & .data$Year<=End_year)%>%
    dplyr::mutate(Season=dplyr::case_when(
      .data$Month%in%c(12,1,2) ~ "Winter",
      .data$Month%in%c(3,4,5) ~ "Spring",
      .data$Month%in%c(6,7,8) ~ "Summer",
      .data$Month%in%c(9,10,11) ~ "Fall")
    )%>%
    {if("Field_coords"%in%names(.)){
      dplyr::mutate(., Field_coords=tidyr::replace_na(.data$Field_coords, FALSE))
    } else{
      .
    }}%>%
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
    }}

  # Return ------------------------------------------------------------------

  return(out)
}
