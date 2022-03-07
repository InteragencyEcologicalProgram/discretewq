# Download USGS CAWSC discrete chl-a and nutrient data

library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# identify stations and parameters of interest

siteNumbers<-c('USGS-11303500',
               'USGS-11304810',
               'USGS-11311300',
               'USGS-11312672',
               'USGS-11312676',
               'USGS-11312685',
               'USGS-11312968',
               'USGS-11313240',
               'USGS-11313315',
               'USGS-11313405',
               'USGS-11313431',
               'USGS-11313433',
               'USGS-11313434',
               'USGS-11313440',
               'USGS-11313452',
               'USGS-11313460',
               'USGS-11336600',
               'USGS-11336680',
               'USGS-11336685',
               'USGS-11336790',
               'USGS-11336930',
               'USGS-11337080',
               'USGS-11337190',
               'USGS-11447650',
               'USGS-11447830',
               'USGS-11447850',
               'USGS-11447890',
               'USGS-11447903',
               'USGS-11447905',
               'USGS-11455095',
               'USGS-11455136',
               'USGS-11455139',
               'USGS-11455140',
               'USGS-11455142',
               'USGS-11455143',
               'USGS-11455146',
               'USGS-11455165',
               'USGS-11455166',
               'USGS-11455167',
               'USGS-11455276',
               'USGS-11455280',
               'USGS-11455315',
               'USGS-11455335',
               'USGS-11455338',
               'USGS-11455350',
               'USGS-11455385',
               'USGS-11455420',
               'USGS-11455478',
               'USGS-11455485',
               'USGS-11455508',
               'USGS-380631122032201',
               'USGS-380833122033401',
               'USGS-381142122015801',
               'USGS-381424121405601',
               'USGS-381614121415301',
               'USGS-382006121401601',
               'USGS-382010121402301',
               'USGS-383019121350701',
               'USGS-381944121405201')

parameterCd <- c('00608', '00613', '00631', '00671', '00681', '62854', '70953')

# retrieve data

cawsc <- dataRetrieval::readWQPqw(siteNumbers, parameterCd)

#retrieve station lat/long and attach to data

lat_long <- whatWQPsites(siteid=siteNumbers)%>%
  select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)

cawsc_long <- cawsc%>%
  left_join(lat_long,
            by ="MonitoringLocationIdentifier") %>%
  mutate(Source = "USGS_CAWSC") %>%
  select(Source,
         MonitoringLocationIdentifier,
         LatitudeMeasure,
         LongitudeMeasure,
         ActivityStartDate,
         ActivityStartTime.Time,
         ActivityStartDateTime,
         CharacteristicName,
         ResultMeasureValue,
         ActivityStartTime.TimeZoneCode,
         ResultStatusIdentifier,
         ResultDetectionConditionText,
         ResultLaboratoryCommentText,
         ResultValueTypeName,
         DetectionQuantitationLimitMeasure.MeasureValue)

#summary as of Feb 2022
summary(cawsc_long$ResultStatusIdentifier=="Historical")#2099
summary(cawsc_long$ResultStatusIdentifier=="Accepted")#7412
summary(cawsc_long$ResultStatusIdentifier=="Preliminary")#5885

#limit data to historical and accepted
cawsc_long_approved <- filter(cawsc_long, ResultStatusIdentifier!="Preliminary")

#add qualifier code for 'estimated' values and '<' (nutrients). See dataRetrieval vignette: https://cran.r-project.org/web/packages/dataRetrieval/vignettes/qwdata_changes.html

cawsc_sign<-mutate(cawsc_long_approved, sign=case_when(ResultDetectionConditionText=="Not Detected" ~ "<",
                                                      ResultValueTypeName%in%c("Estimated") ~ "~",
                                                      TRUE ~ "="))%>%
  select(-ResultDetectionConditionText, -ResultValueTypeName, -ResultStatusIdentifier, -ResultLaboratoryCommentText, -DetectionQuantitationLimitMeasure.MeasureValue)
#make df wide

cawsc_wide <- pivot_wider(cawsc_sign, names_from=CharacteristicName, values_from=c(ResultMeasureValue, sign))

#create new DateTime columns that pastes Data and Time

USGS_CAWSC <- cawsc_wide %>%
  rename(Station = MonitoringLocationIdentifier, Latitude = LatitudeMeasure, Longitude = LongitudeMeasure,
         Date = ActivityStartDate, Datetime = ActivityStartDateTime, TimeDatum = ActivityStartTime.TimeZoneCode,
         Chlorophyll = 'ResultMeasureValue_Chlorophyll a', Chlorophyll_Sign = "sign_Chlorophyll a", DissAmmonia_Sign = "sign_Ammonia and ammonium",
         DissAmmonia = 'ResultMeasureValue_Ammonia and ammonium', DissNitrateNitrite_Sign = "sign_Inorganic nitrogen (nitrate and nitrite)" ,
         DissNitrateNitrite = 'ResultMeasureValue_Inorganic nitrogen (nitrate and nitrite)',
         DissOrthophos = 'ResultMeasureValue_Orthophosphate', DissOrthophos_Sign = "sign_Orthophosphate", DOC = "ResultMeasureValue_Organic carbon", )%>%
  mutate(Datetime=with_tz(Datetime, tzone = "America/Los_Angeles"))%>% #convert times to Pacific time

  select(Source, Station, Latitude, Longitude, Date, Datetime, Chlorophyll_Sign, Chlorophyll, DissAmmonia_Sign, DissAmmonia, DissNitrateNitrite_Sign, DissNitrateNitrite, DOC, DissOrthophos_Sign, DissOrthophos) #reorder columns

usethis::use_data(USGS_CAWSC, overwrite = TRUE)
