
<!-- README.md is generated from README.Rmd. Please edit that file -->

# discretewq

<!-- badges: start -->

[![R build
status](https://github.com/sbashevkin/discretewq/workflows/R-CMD-check/badge.svg)](https://github.com/sbashevkin/discretewq/actions)
[![Codecov test
coverage](https://codecov.io/gh/sbashevkin/discretewq/branch/master/graph/badge.svg)](https://codecov.io/gh/sbashevkin/discretewq?branch=master)
<!-- badges: end -->

The goal of discretewq is to integrate discrete water quality data from
the San Francisco Estuary.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

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
