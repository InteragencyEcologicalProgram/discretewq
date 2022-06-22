
<!-- README.md is generated from README.Rmd. Please edit that file -->

# discretewq

<!-- badges: start -->

[![R build
status](https://github.com/sbashevkin/discretewq/workflows/R-CMD-check/badge.svg)](https://github.com/sbashevkin/discretewq/actions)
[![Codecov test
coverage](https://codecov.io/gh/sbashevkin/discretewq/branch/main/graph/badge.svg)](https://codecov.io/gh/sbashevkin/discretewq?branch=main)
[![DOI](https://zenodo.org/badge/309747392.svg)](https://zenodo.org/badge/latestdoi/309747392)
[![Data
DOI](https://img.shields.io/badge/Data%20publication%20DOI-10.6073/pasta/567ca1dce56cc819b1819117538bd718-blue.svg)](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=731)
<!-- badges: end -->

The goal of discretewq is to integrate discrete water quality data from
the San Francisco Estuary.

## Installation

You can install the latest version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("sbashevkin/discretewq")
```

## Usage

To obtain the full integrated water quality dataset

``` r
require(discretewq)
#> Loading required package: discretewq
Data <- wq(Sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP",
                       "SDO", "SKT", "SLS", "20mm", "Suisun", 
                       "Baystudy", "USBR", "USGS_SFBS", "YBFMP", "USGS_CAWSC"))

str(Data)
#> tibble [318,374 × 57] (S3: tbl_df/tbl/data.frame)
#>  $ Source                       : chr [1:318374] "FMWT" "FMWT" "FMWT" "FMWT" ...
#>  $ Station                      : chr [1:318374] "070" "070" "070" "070" ...
#>  $ Latitude                     : num [1:318374] 38.2 38.2 38.2 38.2 38.2 ...
#>  $ Longitude                    : num [1:318374] -122 -122 -122 -122 -122 ...
#>  $ Date                         : POSIXct[1:318374], format: "1992-01-10" "1992-02-07" ...
#>  $ Datetime                     : POSIXct[1:318374], format: "1992-01-10 08:18:00" "1992-02-07 08:23:00" ...
#>  $ Depth                        : num [1:318374] 6.1 3.66 6.1 4.57 6.1 ...
#>  $ Tide                         : chr [1:318374] "Flood" "Flood" "Ebb" "Flood" ...
#>  $ Microcystis                  : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ Secchi                       : num [1:318374] 69 120 32 71 104 71 15 16 9 70 ...
#>  $ Secchi_estimated             : logi [1:318374] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ Temperature                  : num [1:318374] 8.3 10 13.9 21.1 19.4 14.7 8.9 7.2 10.6 14.7 ...
#>  $ Temperature_bottom           : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ Conductivity                 : num [1:318374] 216 204 246 NA 174 225 157 192 211 192 ...
#>  $ Notes                        : chr [1:318374] NA NA NA NA ...
#>  $ Field_coords                 : logi [1:318374] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ Chlorophyll                  : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ Conductivity_bottom          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissolvedOxygen              : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissolvedOxygen_bottom       : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissolvedOxygenPercent       : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissolvedOxygenPercent_bottom: num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ pH                           : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ pH_bottom                    : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ TotAlkalinity                : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ TotAmmonia                   : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissAmmonia_Sign             : chr [1:318374] NA NA NA NA ...
#>  $ DissAmmonia                  : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissBromide                  : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissCalcium                  : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ TotChloride                  : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissChloride                 : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissNitrateNitrite_Sign      : chr [1:318374] NA NA NA NA ...
#>  $ DissNitrateNitrite           : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DOC                          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ TOC                          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DON                          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ TON                          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissOrthophos_Sign           : chr [1:318374] NA NA NA NA ...
#>  $ DissOrthophos                : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ TotPhos                      : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ DissSilica                   : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ TDS                          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ TSS                          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ VSS                          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ TKN                          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ Sample_depth_surface         : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ Sample_depth_bottom          : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ Salinity                     : num [1:318374] 0.1018 0.0961 0.1163 NA 0.0817 ...
#>  $ Sample_depth_nutr_surface    : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
#>  $ Chlorophyll_Sign             : chr [1:318374] NA NA NA NA ...
#>  $ MonthYear                    : POSIXct[1:318374], format: "1992-01-01" "1992-02-01" ...
#>  $ Year                         : num [1:318374] 1992 1992 1992 1992 1992 ...
#>  $ StationID                    : chr [1:318374] "FMWT 070" "FMWT 070" "FMWT 070" "FMWT 070" ...
#>  $ Month                        : num [1:318374] 1 2 3 9 10 11 12 1 2 3 ...
#>  $ Season                       : chr [1:318374] "Winter" "Winter" "Spring" "Fall" ...
#>  $ Salinity_bottom              : num [1:318374] NA NA NA NA NA NA NA NA NA NA ...
```

## Data publication

The dataset is also [published on the Environmental Data
Initiative](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=731),
where you can find detailed metadata. This static version of the dataset
corresponds to version 2.3.2 of the R package ([archived on
zenodo](https://zenodo.org/record/6390964)).

## Data sources

CDFW. 2020a. Bay Study data. <ftp://ftp.wildlife.ca.gov/BayStudy/>.

CDFW. 2020b. Fall Midwater Trawl data.
<ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/FMWT%20Data/>.

CDFW. 2020c. Summer Townet data.
[ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/TNS MS Access
Data/TNS
data/](ftp://ftp.wildlife.ca.gov/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/).

Cloern, J. E., and T. S. Schraga. 2016. USGS Measurements of Water
Quality in San Francisco Bay (CA), 1969-2015 (ver. 3.0 June 2017). U. S.
Geological Survey data release.
[doi:https://doi.org/10.5066/F7TQ5ZPR](https://doi.org/10.5066/F7TQ5ZPR)

Interagency Ecological Program (IEP), L. Damon, T. Tempel, and A.
Chorazyczewski. 2020a. Interagency Ecological Program San Francisco
Estuary 20mm Survey 1995 - 2020. Environmental Data Initiative.
[doi:10.6073/PASTA/DA7269F6B68975232A2665B211E57229](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=535&revision=2)

Interagency Ecological Program (IEP), L. Damon, T. Tempel, and A.
Chorazyczewski. 2020b. Interagency Ecological Program San Francisco
Estuary Spring Kodiak Trawl Survey 2002 - 2020. Environmental Data
Initiative.
[doi:10.6073/PASTA/2EDAAA415ABE672008E0AF7542AA5D31](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=527&revision=2)

Interagency Ecological Program (IEP), L. Damon, T. Tempel, and A.
Chorazyczewski. 2021c. Interagency Ecological Program San Francisco
Estuary Smelt Larva Survey 2009 – 2021. Environmental Data Initiative.
[doi:10.6073/PASTA/696749029898FEF9AD268435BEE54D3D](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=534&revision=3)

Interagency Ecological Program (IEP), S. Lesmeister, and J. Rinde. 2020.
Interagency Ecological Program: Discrete dissolved oxygen monitoring in
the Stockton Deep Water Ship Channel, collected by the Environmental
Monitoring Program, 1997-2018. Environmental Data Initiative.
[doi:10.6073/PASTA/3268530C683726CD430C81894FFAD768](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.276.2)

Interagency Ecological Program (IEP), M. Martinez, and S. Perry. 2021.
Interagency Ecological Program: Discrete water quality monitoring in the
Sacramento-San Joaquin Bay-Delta, collected by the Environmental
Monitoring Program, 1975-2020. Environmental Data Initiative.
[doi:10.6073/PASTA/31F724011CAE3D51B2C31C6D144B60B0](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=458&revision=4)

Interagency Ecological Program (IEP), R. McKenzie, J. Speegle, A.
Nanninga, J. R. Cook, J. Hagen, and B. Mahardja. 2020c. Interagency
Ecological Program: Over four decades of juvenile fish monitoring data
from the San Francisco Estuary, collected by the Delta Juvenile Fish
Monitoring Program, 1976-2019. Environmental Data Initiative.
[doi:10.6073/PASTA/41B9EEBED270C0463B41C5795537CA7C](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.244.4)

Interagency Ecological Program (IEP), B. Schreier, B. Davis, and N.
Ikemiyagi. 2019. Interagency Ecological Program: Fish catch and water
quality data from the Sacramento River floodplain and tidal slough,
collected by the Yolo Bypass Fish Monitoring Program, 1998-2018.
Environmental Data Initiative.
[doi:10.6073/PASTA/B0B15AEF7F3B52D2C5ADC10004C05A6F](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.233.2)

Interagency Ecological Program (IEP), B. M. Schreier, C. L. Pien, and J.
B. Adams. 2020. Interagency Ecological Program: Zooplankton catch and
water quality data from the Sacramento River floodplain and tidal
slough, collected by the Yolo Bypass Fish Monitoring Program, 1998-2018.
Environmental Data Initiative.
[doi:10.6073/PASTA/EA437DB178D6F7B93213CC0E4A915885](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.494.1)

O’Rear, T., J. Durand, and P. Moyle. 2020. Suisun Marsh Fish Study.
<https://watershed.ucdavis.edu/project/suisun-marsh-fish-study>.

Schraga, T. S., E. S. Nejad, C. A. Martin, and J. E. Cloern. 2018. USGS
measurements of water quality in San Francisco Bay (CA), beginning in
2016 (ver. 3.0, March 2020). U. S. Geological Survey data release.
[doi:https://doi.org/10.5066/F7D21WGF](https://doi.org/10.5066/F7D21WGF)

United States Fish And Wildlife Service, C. Johnston, S. Durkacz, and
others. 2020. Interagency Ecological Program and US Fish and Wildlife
Service: San Francisco Estuary Enhanced Delta Smelt Monitoring Program
data, 2016-2020. Environmental Data Initiative.
[doi:10.6073/PASTA/764F27FF6B0A7B11A487A71C90397084](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.415.3)

U.S. Geological Survey. 2022. USGS water data for the Nation: U.S.
Geological Survey National Water Information System database, accessed
February 7, 2022, at
[doi:10.5066/F7P55KJN](https://doi.org/10.5066/F7P55KJN)

USBR, R. Dahlgren, L. Loken, and E. Van Nieuwenhuyse. 2020. Monthly
vertical profiles of water quality in the Sacramento Deep Water Ship
Channel 2012-2019.
