library(discretewq)
require(purrr)
require(dplyr)
require(stringr)
require(lubridate)

All_surveys_ls <- list(
  baystudy,
  DJFMP,
  DOP,
  EDSM,
  EMP,
  FMWT,
  NCRO,
  SDO,
  SKT,
  SLS,
  STN,
  suisun,
  twentymm,
  USBR,
  USGS_CAWSC,
  USGS_SFBS,
  YBFMP
)

All_surveys_chr <- c(
  "20mm",
  "Baystudy",
  "DJFMP",
  "DOP",
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

All_rows <- sum(map_dbl(All_surveys_ls, nrow))

tzs <- map_chr(All_surveys_ls, ~ tz(.x$Datetime))

Data <- wq(Sources = All_surveys_chr) %>%
  mutate(ID = paste(Source, Station, Date, Datetime, Latitude, Longitude))

test_that("All rows of data make it to the final dataset", {
  expect_equal(All_rows, nrow(Data))
})

test_that("No samples are duplicated", {
  expect_equal(length(unique(Data$ID)), nrow(Data))
})

test_that("All Lats are between 37 and 39 and all Longs are between -123 and -121", {
  expect_true(all((Data$Latitude < 39 & Data$Latitude > 37) | is.na(Data$Latitude)))
  expect_true(all((Data$Longitude < (-121) & Data$Longitude > (-123)) | is.na(Data$Longitude)))
})

test_that("All timezones are in local California time", {
  expect_true(all(tzs %in% "America/Los_Angeles"))
})

test_that("No zeros in environmental variables that shouldn't have them", {
  expect_true(!any(na.omit(Data$Temperature) == 0))
  expect_true(!any(na.omit(Data$Conductivity) == 0))
})

test_that("Errors work correctly", {
  expect_error(
    wq(Sources = "USGS"),
    'The "USGS" data source has been renamed to "USGS_SFBS" because of the inclusion of an additional USGS dataset, "USGS_CAWSC".',
    fixed = TRUE
  )
  expect_error(
    wq(Sources = "SFBS"),
    paste0("You must specify the data sources you wish to include. Choices include:\n ", str_c(All_surveys_chr, collapse = ", ")),
    fixed = TRUE
  )
})

