library(discretewq)
require(purrr)
require(dplyr)

All_rows<-sum(map_dbl(list(baystudy, DJFMP, EDSM, EMP, FMWT, SKT, STN, suisun, twentymm, USBR, USGS), nrow))
Data<-wq(Sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "SKT",
                     "20mm", "Suisun", "Baystudy", "USBR", "USGS"),
         Regions = NULL)%>%
  mutate(ID=paste(Source, Station, Date, Datetime))


test_that("All rows of data make it to the final dataset", {
  expect_equal(All_rows, nrow(Data))
})

test_that("No sampes are duplicated", {
  expect_equal(length(unique(Data$ID)), nrow(Data))
})
