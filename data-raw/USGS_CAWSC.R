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

# 00608 Ammonium
# 00631 NO2 + NO3
# 00671 Ortho-phosphate
# 00681 DOC
# 70953 Chl-a
parameterCd <- c('00608', '00631', '00671', '00681', '70953')

# retrieve data-------------------

cawsc <- dataRetrieval::readWQPqw(siteNumbers, parameterCd)

#retrieve lat/long and attach to data--------------------

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
         USGSPCode,
         ResultMeasureValue,
         ActivityStartTime.TimeZoneCode,
         ResultStatusIdentifier,
         ResultDetectionConditionText,
         ResultLaboratoryCommentText,
         ResultValueTypeName,
         DetectionQuantitationLimitMeasure.MeasureValue,
         DetectionQuantitationLimitTypeName,
         ResultAnalyticalMethod.MethodName,
         ResultAnalyticalMethod.MethodIdentifier)

#summary as of Aug 19 2022
summary(cawsc_long$ResultStatusIdentifier=="Historical")#2099
summary(cawsc_long$ResultStatusIdentifier=="Accepted")#8032
summary(cawsc_long$ResultStatusIdentifier=="Preliminary")#6288

#create sign column----------------

#a qualifier code for 'estimated' values and 'less than' reporting limit. See dataRetrieval vignette: https://cran.r-project.org/web/packages/dataRetrieval/vignettes/qwdata_changes.html

cawsc_sign <- mutate(cawsc_long, sign=case_when(ResultDetectionConditionText=="Not Detected" ~ "<",
                                                ResultValueTypeName%in%c("Estimated") ~ "~",
                                                TRUE ~ "="))

#create new result column that assigns Limit Measure if ResultMeasureValue is missing


cawsc_sign$result <- ifelse((cawsc_sign$sign == "~")|((cawsc_sign$sign == "<")), cawsc_sign$DetectionQuantitationLimitMeasure.MeasureValue, cawsc_sign$ResultMeasureValue)

#explore NAs----------------

na <- cawsc_sign[is.na(cawsc_sign$result),]

unique(na$CharacteristicName)

table(na$CharacteristicName, useNA = 'always')

#931 total NAs
#NH4 = 430
#Chl-a = 5
#NO3 + NO2 = 283
#Nitrite = 207
#Total Nitrogen = 1
#Orthophosphate = 5

# replace reporting limits for parameters according to internal USGS website http://nwql.cr.usgs.gov/usgs/limits/limits.cfm
## citation for changes to historical reporting limits is:

#Foreman, W.T., Williams, T.L., Furlong, E.T., Hemmerle, D.M., Stetson, S.J., Jha, V.K.,
#Noriega, M.C., Decess, J.A., Reed-Parker, C., and Sandstrom, M.W., 2021, Comparison of
#detection limits estimated using single- and multi-concentration spike-based and blankbased procedures:
#Talanta, v. 228, article 122139, 15 p., accessed [enter date article accessed] at https://doi.org/10.1016/j.talanta.2021.122139

#subset Nitrite---------------------

NO2 <- subset(cawsc_sign, CharacteristicName == "Nitrite")

NO2_na <- NO2[is.na(NO2$result),] #207

#explore methods used for Nitrite parameter---------------------

unique(NO2_na$ResultAnalyticalMethod.MethodIdentifier)

table(NO2_na$ResultAnalyticalMethod.MethodIdentifier, useNA = 'always')

#Nitrite reporting limits-------------------------

#DZ001 2014-10-01 - present, DLDQC, RL = 0.002
#DZ001 2011-10-01 - 2014-09-30, mdl, RL = 0.002
#DZ001 2010-10-01 - 2011-09-30, mdl, RL = 0.002
#DZ001 2009-10-01 - 2010-09-30, lrl, RL = 0.002
#DZ001 2006-05-01 - 2009-09-30, lrl, RL = 0.002

#CL041 2001-10-01 - 2012-01-04, lrl, RL = 0.008
#CL041 2001-05-15 - 2001-09-30, lrl, RL = 0.006
#CL041 2000-10-04 - 2001-05-14, lrl, RL = 0.006

NO2$result <- ifelse(is.na(NO2$result) & NO2$ResultAnalyticalMethod.MethodIdentifier == "DZ001" & NO2$ActivityStartDate > '2006-04-30',
                     yes = '0.002', no = NO2$result)

NO2$result <- ifelse(is.na(NO2$result) & NO2$ResultAnalyticalMethod.MethodIdentifier == "CL041" & NO2$ActivityStartDate < '2001-09-30',
                     yes = '0.006', no = NO2$result)

NO2_na <- NO2[is.na(NO2$result),] #0

#subset Ammonia + ammonium--------------

NH4 <- subset(cawsc_sign, CharacteristicName == "Ammonia and ammonium")

NH4_na <- NH4[is.na(NH4$result),] #430

#explore methods used for Ammonia + ammonium parameter-------------------

unique(NH4_na$ResultAnalyticalMethod.MethodIdentifier)

table(NH4_na$ResultAnalyticalMethod.MethodIdentifier, useNA = 'always')

#Ammonia + ammonium reporting limits-------------------------

#SHC02 2020-10-20 - present, DLDQC, RL = 0.04
#SHC02 2014-10-01 - 2020-10-19, DLDQC, RL = 0.02
#SHC02 2011-10-01 - 2014-09-30, ltmdl, RL = 0.02
#SHC02 2010-10-01 - 2011-09-30, ltmdl, RL = 0.02
#SHC02 2009-10-01 - 2010-09-30, lrl, RL = 0.02
#SHC02 2006-10-01 - 2009-09-30, lrl, RL = 0.02
#SHC02 2006-05-01 - 2006-09-30, lrl, RL = 0.01

#CL037 2001-05-15 - 2003-09-30, lrl, RL = 0.41
#CL037 2000-10-04 - 2001-05-14, lrl, RL = 0.41

#assign reporting limit if Ammonia + ammonium result is blank------------------

NH4$result <- ifelse(is.na(NH4$result) & NH4$ResultAnalyticalMethod.MethodIdentifier == "SHC02" & NH4$ActivityStartDate > '2020-10-19', "0.04", NH4$result)

NH4$result <- ifelse(is.na(NH4$result) & NH4$ResultAnalyticalMethod.MethodIdentifier == "SHC02" & NH4$ActivityStartDate > '2006-09-30' & NH4$ActivityStartDate < '2020-10-20' , '0.02', NH4$result)

#CL037 ResultMeasure Value didn't meet conditions in ifelse statement on line 131

NH4$result <- ifelse(is.na(NH4$result) & NH4$ResultAnalyticalMethod.MethodIdentifier == "CL037", NH4$ResultMeasureValue, NH4$result)

NH4_na <- NH4[is.na(NH4$result),] #0

#subset Nitrate + nitrite--------------

NO3_NO2 <- subset(cawsc_sign, CharacteristicName == "Inorganic nitrogen (nitrate and nitrite)")

NO3_NO2_na <- NO3_NO2[is.na(NO3_NO2$result),] #283

#explore methods used for Nitrate + nitrite parameter-----------------------------

unique(NO3_NO2_na$ResultAnalyticalMethod.MethodIdentifier)

table(NO3_NO2_na$ResultAnalyticalMethod.MethodIdentifier, useNA = 'always')

#Nitrate + nitrite reporting limits-------------------------

#RED02 2014-10-01 - present, DLDQC, RL = 0.02
#RED02 2011-10-03 - 2014-09-30, mdl, RL = 0.02
#RED02 2008-01-01 - 2011-10-02, lrl, RL = 0.01

#RED01 2014-10-01 - present, DLDQC, RL = 0.08
#RED01 2011-10-03 - 2014-09-30, mdl, RL = 0.08
#RED01 2008-01-01 - 2011-10-02, lrl, RL = 0.04

#assign Nitrate + nitrite if reporting limit is blank-----------------------------

NO3_NO2$result <- ifelse(is.na(NO3_NO2$result) & NO3_NO2$ResultAnalyticalMethod.MethodIdentifier == "RED02" & NO3_NO2$ActivityStartDate > '2011-10-02', "0.02", NO3_NO2$result)

NO3_NO2$result <- ifelse(is.na(NO3_NO2$result) & NO3_NO2$ResultAnalyticalMethod.MethodIdentifier == "RED01" & NO3_NO2$ActivityStartDate > '2011-10-02', "0.08", NO3_NO2$result)

#NO3_NO2$result <- ifelse(is.na(NO3_NO2$result) & NO3_NO2$ResultAnalyticalMethod.MethodIdentifier == "RED02" & NO3_NO2$ActivityStartDate < '2011-10-02', "0.01", NO3_NO2$result)

#NO3_NO2$result <- ifelse(is.na(NO3_NO2$result) & NO3_NO2$ResultAnalyticalMethod.MethodIdentifier == "RED01" & NO3_NO2$ActivityStartDate < '2011-10-03', "0.04", NO3_NO2$result)

NO3_NO2_na <- NO3_NO2[is.na(NO3_NO2$result),]

#subset Chlorophyll----------------------

chla <- subset(cawsc_sign, CharacteristicName == "Chlorophyll a")

chla_na <- chla[is.na(chla$result),]

#no missing reporting limits,  but 5 samples didn't meet the conditions in ifelse statement on line 131

chla$result <- ifelse(is.na(chla$result) & chla$sign == "~", chla$ResultMeasureValue, chla$result)

chla_na <- chla[is.na(chla$result),]

#subset orthophosphate----------------------

orthop <- subset(cawsc_sign, CharacteristicName == "Orthophosphate")

orthop_na <- orthop[is.na(orthop$result),] #5 NAs

#3 NAs were not filled in with ifelse conditional statement using sign column

orthop$result <- ifelse(is.na(orthop$result) & orthop$ResultAnalyticalMethod.MethodIdentifier == "PHM01", '0.008', orthop$result)

orthop$result <- ifelse(is.na(orthop$result), orthop$ResultMeasureValue, orthop$result)

orthop_na <- orthop[is.na(orthop$result),]

#subset Nitrogen

TN <- subset(cawsc_sign, CharacteristicName == "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)")

TN_na <- TN[is.na(TN$result),] #1

#subset DOC

DOC <- subset(cawsc_sign, CharacteristicName == "Organic carbon")

DOC_na <- DOC[is.na(DOC$result),]

#column bind all parameters--------------------------------

cawsc_long_v2 <- rbind(NH4, NO2, NO3_NO2, orthop, chla, TN, DOC)

cawsc_long_v2$result <- as.numeric(cawsc_long_v2$result)

cawsc_long_v3 <- cawsc_long_v2 %>%
  select(-USGSPCode, -ResultMeasureValue, -ResultDetectionConditionText, -ResultValueTypeName, -ResultStatusIdentifier, -ResultLaboratoryCommentText, -DetectionQuantitationLimitMeasure.MeasureValue, -DetectionQuantitationLimitTypeName, -ResultAnalyticalMethod.MethodName, -ResultAnalyticalMethod.MethodIdentifier)

#make df wide-------------------------------

cawsc_wide <- pivot_wider(cawsc_long_v3, names_from=CharacteristicName, values_from=c(result, sign))

#create new column names and DateTime columns that pastes Date and Time

USGS_CAWSC <- cawsc_wide %>%
  rename(Station = MonitoringLocationIdentifier, Latitude = LatitudeMeasure, Longitude = LongitudeMeasure,
         Date = ActivityStartDate, Datetime = ActivityStartDateTime, TimeDatum = ActivityStartTime.TimeZoneCode,
         Chlorophyll = 'result_Chlorophyll a', Chlorophyll_Sign = "sign_Chlorophyll a", DissAmmonia_Sign = "sign_Ammonia and ammonium",
         DissAmmonia = 'result_Ammonia and ammonium', DissNitrateNitrite_Sign = "sign_Inorganic nitrogen (nitrate and nitrite)" ,
         DissNitrateNitrite = 'result_Inorganic nitrogen (nitrate and nitrite)',
         DissOrthophos = 'result_Orthophosphate', DissOrthophos_Sign = "sign_Orthophosphate", DOC = "result_Organic carbon", )%>%
  mutate(Datetime=with_tz(Datetime, tzone = "America/Los_Angeles"))%>% #convert times to Pacific time

  select(Source, Station, Latitude, Longitude, Date, Datetime, Chlorophyll_Sign, Chlorophyll, DissAmmonia_Sign, DissAmmonia, DissNitrateNitrite_Sign, DissNitrateNitrite, DOC, DissOrthophos_Sign, DissOrthophos) #reorder columns

usethis::use_data(USGS_CAWSC, overwrite = TRUE)
