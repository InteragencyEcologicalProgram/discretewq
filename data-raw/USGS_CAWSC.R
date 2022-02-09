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

lat_long <- whatWQPsites(siteid=siteNumbers)


lat_long <- lat_long %>%
  select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)

cawsc_long <- cawsc%>%
  left_join(lat_long,
            by ="MonitoringLocationIdentifier")%>%
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
         ResultStatusIdentifier)

summary(cawsc_long$ResultStatusIdentifier=="Historical")#2099
summary(cawsc_long$ResultStatusIdentifier=="Accepted")#7412
summary(cawsc_long$ResultStatusIdentifier=="Preliminary")#5885

#limit data to historical and accepted
cawsc_approved <- filter(cawsc_long, ResultStatusIdentifier!="Preliminary")

cawsc_wide <- pivot_wider(cawsc_approved, names_from = "CharacteristicName", values_from = "ResultMeasureValue")

#create new DateTime columns that pastes Data and Time

USGS_CAWSC <- cawsc_wide %>%
  rename(Station = MonitoringLocationIdentifier, Latitude = LatitudeMeasure, Longitude = LongitudeMeasure,
         Date = ActivityStartDate, Datetime = ActivityStartDateTime, TimeDatum = ActivityStartTime.TimeZoneCode,
         Chlorophyll = 'Chlorophyll a', DissAmmonia = 'Ammonia and ammonium', DissNitrateNitrite = 'Inorganic nitrogen (nitrate and nitrite)',
         DissOrthophos = Orthophosphate, DOC = "Organic carbon")%>%
  mutate(Datetime=with_tz(Datetime, tzone = "America/Los_Angeles"))%>% #convert times to Pacific time
  select(Source, Station, Latitude, Longitude, Date, Datetime, Chlorophyll, DissAmmonia, DissNitrateNitrite, DOC, DissOrthophos) #reorder columns

usethis::use_data(USGS_CAWSC, overwrite = TRUE)
