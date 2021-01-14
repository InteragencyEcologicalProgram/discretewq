# discretewq 0.1.0

* Undoing previous DJFMP change and now retaining all DJFMP data (except duplicates at the same date and time)
* Changing EDSM station variable to reflect each unique randon 'site' so it is possible to identify replicate tows at the same location
* Tweaked USBR data to retain the average sample time across all depths (previously it used the earliest sample time).
* Tweaked DJFMP data to keep the datapoint closest to noon of each day (previously it was averaging across all time points)
* Added conductivity data from EDSM and DJFMP after 06/01/2019 when it was standardized to specific conductivity
* Added a `NEWS.md` file to track changes to the package.
