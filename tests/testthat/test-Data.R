library(discretewq)
require(purrr)
require(dplyr)

All_rows<-sum(map_dbl(list(baystudy, DJFMP, EDSM, EMP, FMWT, SKT, STN, suisun, twentymm, USBR, USGS), nrow))
Data<-wq()%>%
  mutate(ID=paste(Source, Station, Date, Datetime, Latitude, Longitude))


test_that("All rows of data make it to the final dataset", {
  expect_equal(All_rows, nrow(Data))
})

test_that("No sampes are duplicated", {
  expect_equal(length(unique(Data$ID)), nrow(Data))
})

test_that("All Lats are between 37 ad 39 and all Longs are between -123 and -121", {
  expect_true(all((Data$Latitude<39 & Data$Latitude>37) | is.na(Data$Latitude)))
  expect_true(all((Data$Longitude<(-121) & Data$Longitude>(-123)) | is.na(Data$Longitude)))
})
