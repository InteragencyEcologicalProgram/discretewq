# discretewq (development version)

* Fixing error where `YBFMP` Secchi data was in meters. Converted it to cm for consistency with other surveys. 
* Adding `Secchi_estimated` variable to `FMWT` data

# discretewq 2.3.1

# discretewq 2.3.0

* Changed the name of the `USGS` survey to `USGS_SFBS` to avoid confusion with the new `USGS_CAWSC` survey.
* Added USGS_CAWSC data.

# discretewq 2.2.0

* Add reporting limits (RLs) to EMP data.
* Updated STN data to newest version with 2021 data

# discretewq 2.1.0

* Added nutrient data to EMP and USGS datasets.

# discretewq 2.0.0

* Added the Yolo Bypass Fish Monitoring Program data
* Added Stockton Dissolved Oxygen survey as an additional data source
* Removed duplicated water quality data from suisun data
* Added Smelt Larva Survey as an additional data source
* Tweaked the wq function interface to force users to specify data sources, in preparation for adding more sources.
* Updated the DJFMP, EDSM, EMP, FMWT, SKT, STN, USGS, 20mm, Suisun datasets to their latest version

# discretewq 1.1.0

* Fixed timezones for EMP data. EMP times are reported in PST but had incorrectly been imported as local time (PST/PDT). Now, they are imported as Etc/GMT+8 and then converted to America/Los_Angeles to correspond to the other surveys.

# discretewq 1.0.1

# discretewq 1.0.0

* Publishing to link with data publication.

# discretewq 0.2.0

* Fixing one tide value of 0 from SKT by converting it to NA. 

# discretewq 0.1.0

* Undoing previous DJFMP change and now retaining all DJFMP data (except duplicates at the same date and time)
* Changing EDSM station variable to reflect each unique random 'site' so it is possible to identify replicate tows at the same location
* Tweaked USBR data to retain the average sample time across all depths (previously it used the earliest sample time).
* Tweaked DJFMP data to keep the datapoint closest to noon of each day (previously it was averaging across all time points)
* Added conductivity data from EDSM and DJFMP after 06/01/2019 when it was standardized to specific conductivity
* Added a `NEWS.md` file to track changes to the package.
