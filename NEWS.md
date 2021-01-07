# discretewq 0.1.0

* Tweaked USBR data to retain the average sample time across all depths (previously it used the earliest sample time).
* Tweaked DJFMP data to keep the datapoint closest to noon of each day (previously it was averaging across all time points)
* Added conductivity data from EDSM and DJFMP after 06/01/2019 when it was standardized to specific conductivity
* Added a `NEWS.md` file to track changes to the package.
