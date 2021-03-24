
<!-- README.md is generated from README.Rmd. Please edit that file -->

# discretewq

<!-- badges: start -->

[![R build
status](https://github.com/sbashevkin/discretewq/workflows/R-CMD-check/badge.svg)](https://github.com/sbashevkin/discretewq/actions)
[![Codecov test
coverage](https://codecov.io/gh/sbashevkin/discretewq/branch/main/graph/badge.svg)](https://codecov.io/gh/sbashevkin/discretewq?branch=main)
[![DOI](https://zenodo.org/badge/309747392.svg)](https://zenodo.org/badge/latestdoi/309747392)
[![Data
DOI](https://img.shields.io/badge/Data%20publication%20DOI-10.6073/pasta/1694ea7f9ef9cc8619b01c3588029683-blue.svg)](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=731)
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
Data <- wq(Sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "SKT",
                       "20mm", "Suisun", "Baystudy", "USBR", "USGS"),
           Regions = NULL)
```

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

USBR, R. Dahlgren, L. Loken, and E. Van Nieuwenhuyse. 2020. Monthly
vertical profiles of water quality in the Sacramento Deep Water Ship
Channel 2012-2019.
