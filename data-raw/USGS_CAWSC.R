# Download USGS CAWSC discrete chl-a and nutrient data

library(dataRetrieval)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Identify stations and parameters of interest
siteNumbers<-c(
  'USGS-11303500',
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
  'USGS-381944121405201'
)

# 00608 Ammonium
# 00631 NO2 + NO3
# 00671 Ortho-phosphate
# 00681 DOC
# 70953 Chl-a
parameterCd <- c('00608', '00631', '00671', '00681', '70953')

# Retrieve data
cawsc <- dataRetrieval::readWQPqw(siteNumbers, parameterCd)

# Retrieve lat/long coordinates and attach to data
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
         ResultValueTypeName,
         DetectionQuantitationLimitMeasure.MeasureValue,
         ResultAnalyticalMethod.MethodIdentifier)

# Summary as of Aug 29 2022
cawsc_long %>% count(ResultStatusIdentifier)
# We will keep all Result status categories at this time, including Preliminary

# Create sign column
# A qualifier code for 'estimated' values and 'less than' reporting limit. See
  # dataRetrieval vignette:
  # https://cran.r-project.org/web/packages/dataRetrieval/vignettes/qwdata_changes.html

cawsc_sign <- cawsc_long %>%
  mutate(
    Sign = case_when(
      ResultDetectionConditionText == "Not Detected" ~ "<",
      ResultValueTypeName == "Estimated" ~ "~",
      TRUE ~ "="
    )
  )

# All NA's in ResultMeasureValue are <RL, we'll substitute these with their RL
  # values. Create new "Value" column that assigns RL if ResultMeasureValue is NA.
cawsc_sign <- cawsc_sign %>%
  mutate(
    Value = if_else(
      Sign == "<",
      DetectionQuantitationLimitMeasure.MeasureValue,
      ResultMeasureValue
    )
  )

# Explore remaining NA values in the Value column
missing_rl <- cawsc_sign %>% filter(is.na(Value))

missing_rl %>% count(CharacteristicName)
# 710 total NA values
# NH4 = 425
# NO3 + NO2 = 283
# Ortho-phosphate = 2

# These remaining NA values are <RL records that don't have RL values provided.
# For these we'll assign RL values based on this internal USGS website:
# http://nwql.cr.usgs.gov/usgs/limits/limits.cfm

# The citation for changes to historical reporting limits is:
# Foreman, W.T., Williams, T.L., Furlong, E.T., Hemmerle, D.M., Stetson, S.J.,
# Jha, V.K., Noriega, M.C., Decess, J.A., Reed-Parker, C., and Sandstrom, M.W.,
# 2021, Comparison of detection limits estimated using single- and
# multi-concentration spike-based and blankbased procedures: Talanta, v. 228,
# article 122139, 15 p., accessed [enter date article accessed] at
# https://doi.org/10.1016/j.talanta.2021.122139

# Ammonia + Ammonium:
missing_rl %>%
  filter(CharacteristicName == "Ammonia and ammonium") %>%
  distinct(ResultAnalyticalMethod.MethodIdentifier)

# Only 1 method with missing RL's - SHC02
# Historical RL values for this method:
  # SHC02 2020-10-20 - present, DLDQC, RL = 0.04
  # SHC02 2014-10-01 - 2020-10-19, DLDQC, RL = 0.02
  # SHC02 2011-10-01 - 2014-09-30, ltmdl, RL = 0.02
  # SHC02 2010-10-01 - 2011-09-30, ltmdl, RL = 0.02
  # SHC02 2009-10-01 - 2010-09-30, lrl, RL = 0.02
  # SHC02 2006-10-01 - 2009-09-30, lrl, RL = 0.02
  # SHC02 2006-05-01 - 2006-09-30, lrl, RL = 0.01
missing_rl %>%
  filter(CharacteristicName == "Ammonia and ammonium") %>%
  summarize(
    min_date = min(ActivityStartDate),
    max_date = max(ActivityStartDate)
  )
# We need to fill in two RL values for Ammonia + Ammonium based on the date
  # 2020-10-20 - present: RL = 0.04
  # 2014-10-01 - 2020-10-19: RL = 0.02

# Nitrate + Nitrite:
missing_rl %>%
  filter(CharacteristicName == "Inorganic nitrogen (nitrate and nitrite)") %>%
  distinct(ResultAnalyticalMethod.MethodIdentifier)

# 2 methods with missing RL's - RED01 and RED02
# Historical RL values for these methods:
  # RED01 2014-10-01 - present, DLDQC, RL = 0.08
  # RED01 2011-10-03 - 2014-09-30, mdl, RL = 0.08
  # RED01 2008-01-01 - 2011-10-02, lrl, RL = 0.04

  # RED02 2014-10-01 - present, DLDQC, RL = 0.02
  # RED02 2011-10-03 - 2014-09-30, mdl, RL = 0.02
  # RED02 2008-01-01 - 2011-10-02, lrl, RL = 0.01
missing_rl %>%
  filter(CharacteristicName == "Inorganic nitrogen (nitrate and nitrite)") %>%
  group_by(ResultAnalyticalMethod.MethodIdentifier) %>%
  summarize(
    min_date = min(ActivityStartDate),
    max_date = max(ActivityStartDate)
  )
# We need to fill in two RL values for Nitrate + Nitrite based on the method
  # RED01 2014-10-01 - present: RL = 0.08
  # RED02 2014-10-01 - present: RL = 0.02

# Ortho-phosphate:
missing_rl %>%
  filter(CharacteristicName == "Orthophosphate") %>%
  distinct(ResultAnalyticalMethod.MethodIdentifier)

# Only 1 method with missing RL's - PHM01
# Historical RL value for this method: 0.008

# Fill in values for <RL records that are missing RL's - based on methods and
  # time periods shown above
cawsc_sign_c <- cawsc_sign %>%
  mutate(
    Value = case_when(
        is.na(Value) & CharacteristicName == "Ammonia and ammonium" & ActivityStartDate < "2020-10-20" ~ 0.02,
        is.na(Value) & CharacteristicName == "Ammonia and ammonium" & ActivityStartDate >= "2020-10-20" ~ 0.04,
        is.na(Value) & CharacteristicName == "Inorganic nitrogen (nitrate and nitrite)" & ResultAnalyticalMethod.MethodIdentifier == "RED01" ~ 0.08,
        is.na(Value) & CharacteristicName == "Inorganic nitrogen (nitrate and nitrite)" & ResultAnalyticalMethod.MethodIdentifier == "RED02" ~ 0.02,
        is.na(Value) & CharacteristicName == "Orthophosphate" ~ 0.008,
        TRUE ~ Value
      )
  )

# Finish preparing the data
USGS_CAWSC <- cawsc_sign_c %>%
  select(!starts_with(c("Result", "Detection"))) %>%
  mutate(
    CharacteristicName = case_when(
      CharacteristicName == "Ammonia and ammonium" ~ "DissAmmonia",
      CharacteristicName == "Chlorophyll a" ~ "Chlorophyll",
      CharacteristicName == "Inorganic nitrogen (nitrate and nitrite)" ~ "DissNitrateNitrite",
      CharacteristicName == "Organic carbon" ~ "DOC",
      CharacteristicName == "Orthophosphate" ~ "DissOrthophos"
    )
  ) %>%
  pivot_wider(
    names_from = CharacteristicName,
    values_from = c(Value, Sign),
    names_glue = "{CharacteristicName}_{.value}"
  ) %>%
  rename_with(~ str_remove(.x, "_Value$")) %>%
  transmute(
    Source,
    Station = MonitoringLocationIdentifier,
    Latitude = as.numeric(LatitudeMeasure),
    Longitude = as.numeric(LongitudeMeasure),
    Date = ActivityStartDate,
    # Convert times to Pacific time
    Datetime = with_tz(ActivityStartDateTime, tzone = "America/Los_Angeles"),
    Chlorophyll_Sign,
    Chlorophyll,
    DissAmmonia_Sign,
    DissAmmonia,
    DissNitrateNitrite_Sign,
    DissNitrateNitrite,
    DOC,
    DissOrthophos_Sign,
    DissOrthophos
  ) %>%
  mutate(
    # Fill in "=" for the NA values in the _Sign variables
    across(ends_with("_Sign"), ~ if_else(is.na(.x), "=", .x)),
    # Fix 4 records where Date and Datetime don't match
    Date = date(Datetime)
  )

usethis::use_data(USGS_CAWSC, overwrite = TRUE)

