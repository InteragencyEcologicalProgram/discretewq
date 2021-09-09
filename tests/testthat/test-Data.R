library(discretewq)
require(purrr)
require(dplyr)
require(lubridate)

All_rows<-sum(map_dbl(list(baystudy, DJFMP, EDSM, EMP, FMWT, SKT, STN, suisun, twentymm, USBR, USGS), nrow))
tzs<-map_chr(list(baystudy, DJFMP, EDSM, EMP, FMWT, SKT, STN, suisun, twentymm, USBR, USGS), ~tz(.x$Datetime))
Data<-wq(Sources=c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "SKT",
                   "20mm", "Suisun", "Baystudy", "USBR", "USGS"))%>%
  mutate(ID=paste(Source, Station, Date, Datetime, Latitude, Longitude))


test_that("All rows of data make it to the final dataset", {
  expect_equal(All_rows, nrow(Data))
})

test_that("No samples are duplicated", {
  expect_equal(length(unique(Data$ID)), nrow(Data))
})

test_that("All Lats are between 37 ad 39 and all Longs are between -123 and -121", {
  expect_true(all((Data$Latitude<39 & Data$Latitude>37) | is.na(Data$Latitude)))
  expect_true(all((Data$Longitude<(-121) & Data$Longitude>(-123)) | is.na(Data$Longitude)))
})

test_that("All timezones are in local California time", {
  expect_true(all(tzs%in%"America/Los_Angeles"))
})

test_that("No zeros in environmental variables that shouldn't have them", {
  expect_true(!any(na.omit(Data$Temperature)==0))
  expect_true(!any(na.omit(Data$Conductivity)==0))
})
