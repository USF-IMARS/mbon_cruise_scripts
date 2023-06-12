# MBON Cruise Scripts
on return from cruise:

* upload cruise logs to location in box.com
* get the files from box.com onto your local in the right location
* run `merge_meta_chl_hplc_cdom.Rmd`
  * this will do some auto-qc
  * this will append rows to the inventory file
* manually check the added rows, then ensure the updated inventory file is in box.com


# NASA SeaBass Links for Data Submission:
1. [Submission Instructions](https://seabass.gsfc.nasa.gov/wiki/Data_Submission#Setting%20up%20SFTP%20Access)
2. [Metadata Headers](https://seabass.gsfc.nasa.gov/wiki/metadataheaders)
3. [Documentation Guidelines](https://seabass.gsfc.nasa.gov/wiki/User_Resources)
4. [Data Submission Special Requirements](https://seabass.gsfc.nasa.gov/wiki/data_submission_special_requirements) - useful for HPLC
5. [Standarized Fields and Units](https://seabass.gsfc.nasa.gov/wiki/stdfields) 
6. [FCHECK](https://seabass.gsfc.nasa.gov/wiki/FCHECK#Download%20Source%20Code) - check data before submission locally, needs `Perl`
