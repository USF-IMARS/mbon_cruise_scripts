# mbon_cruise_scripts
on return from cruise:

* upload cruise logs to location in box.com
* get the files from box.com onto your local in the right location
* run `merge_meta_chl_hplc_cdom.Rmd`
  * this will do some auto-qc
  * this will append rows to the inventory file
* manually check the added rows, then ensure the updated inventory file is in box.com
