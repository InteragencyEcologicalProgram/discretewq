#' discretewq: A package to integrate discrete water quality data from the San Francisco Estuary
#'
#' This package contains the source datasets and a function to combine any combination into an integrated dataset
#' @name discretewq
"_PACKAGE"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
