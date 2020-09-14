## This script is used to screen the monthly and annual data sets for appropriate
## time windows of analyses (ie, begin/end months/years).


##-------------------
## required packages
##-------------------

library(dplyr)
library(tidyr)
library(readr)

##-------
## setup
##-------

## path to data directory
data_dir <- here::here("data")

## categories for sites
site_type <- c("managed", "unmanaged")


##-------------
## annual data
##-------------

## read data files
dat_ann <- read_csv(file.path(data_dir, "annual_data.csv"))

## get first and last year by site
tbl_ann_yr_rng <- dat_ann %>% 
  group_by(site, catchment) %>%
  summarise(yr_first = min(WaterYear),
            yr_last = max(WaterYear),
            n_years = n(),
            na_str_Ca = sum(is.na(StrCamgL)),
            na_str_DOC = sum(is.na(StrDOCmgL)),
            na_str_NH4 = sum(is.na(StrNH4NmgL)),
            na_str_NO3 = sum(is.na(StrNO3NmgL)),
            na_str_TDP = sum(is.na(StrTDPmgL)),
            na_str_SO4 = sum(is.na(StrSO4SmgL))) %>%
  print(n = Inf)

## write to csv
write_csv(tbl_ann_yr_rng,
          file.path(here::here("analysis"), "tbl_ann_yr_rng.csv"))


## NA screening across sites
solutes_unmanaged_ann <- read_csv(file.path(here::here("data"),
                                            "tbl_solutes_unmanaged_ann.csv"))

## years with NA for all stream solutes
yrs_all_na <- solutes_unmanaged_ann %>%
  select(-c(solute, WaterYear)) %>%
  is.na() %>%
  apply(1, all)

## thin data set to exclude years with NA at all sites/catchments
solutes_unmanaged_ann_thin <- solutes_unmanaged_ann %>%
  filter(!yrs_all_na)


##--------------
## monthly data
##--------------

## read data files
dat_mon <- read_csv(file.path(data_dir, "monthly_data.csv"))

## get first and last year by site
tbl_mon_yr_rng <- dat_mon %>% 
  group_by(site, catchment) %>%
  summarise(yr_first = min(WaterYear),
            yr_last = max(WaterYear),
            n_months = n(),
            na_str_Ca = sum(is.na(StrCamgL)),
            na_str_DOC = sum(is.na(StrDOCmgL)),
            na_str_NH4 = sum(is.na(StrNH4NmgL)),
            na_str_NO3 = sum(is.na(StrNO3NmgL)),
            na_str_TDP = sum(is.na(StrTDPmgL)),
            na_str_SO4 = sum(is.na(StrSO4SmgL))) %>%
  print(n = Inf)

## write to csv
write_csv(tbl_mon_yr_rng,
          file.path(here::here("analysis"), "tbl_mon_yr_rng.csv"))

