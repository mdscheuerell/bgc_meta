## This script is used to screen the monthly and annual data sets for appropriate
## time windows of analyses (ie, begin/end months/years).

##-----------------------
## necessary user inputs
##-----------------------

yr_first <- 1964
yr_last <- 2018

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
            na_FWA_Ca = sum(is.na(FWACamgL)),
            na_FWA_DOC = sum(is.na(FWADOCmgL)),
            na_FWA_NH4 = sum(is.na(FWANH4NmgL)),
            na_FWA_NO3 = sum(is.na(FWANO3NmgL)),
            na_FWA_TDP = sum(is.na(FWATDPmgL)),
            na_FWA_SO4 = sum(is.na(FWASO4SmgL))) %>%
  print(n = Inf)

## write to csv
write_csv(tbl_ann_yr_rng,
          file.path(here::here("analysis"), "tbl_ann_yr_rng.csv"))


## ANNUAL DATA

# ## empty lists for tmp data
# solutes_unmanaged_ann <- list()
# solutes_managed_ann <- list()
# 
# ## create solute-specific tables of year-by-site/catchment
# for(i in solutes) {
#   ## unmanaged
#   solutes_unmanaged_ann[[i]] <- dat_ann %>%
#     filter(type == "unmanaged") %>%
#     select(region:WaterYear, all_of(i)) %>%
#     pivot_wider(names_from = c(region, site, catchment), values_from = i) %>%
#     arrange(WaterYear) %>%
#     mutate(solute = sub("(FWA)(.*)(mgL)", "\\2", i)) %>%
#     select(solute, everything(), -type)
#   ## managed
#   solutes_managed_ann[[i]] <- dat_ann %>%
#     filter(type == "managed") %>%
#     select(region:WaterYear, all_of(i)) %>%
#     pivot_wider(names_from = c(region, site, catchment), values_from = i) %>%
#     arrange(WaterYear) %>%
#     mutate(solute = sub("(FWA)(.*)(mgL)", "\\2", i)) %>%
#     select(solute, everything(), -type)
# }
# 
# ## combine lists into df
# solutes_unmanaged_ann <- do.call(rbind, solutes_unmanaged_ann)
# solutes_managed_ann <- do.call(rbind, solutes_managed_ann)
# 
# ## write to csv
# readr::write_csv(solutes_unmanaged_ann,
#                  file.path(here::here("data"), "tbl_solutes_unmanaged_ann.csv"))
# readr::write_csv(solutes_managed_ann,
#                  file.path(here::here("data"), "tbl_solutes_managed_ann.csv"))

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
            na_FWA_Ca = sum(is.na(FWACamgL)),
            na_FWA_DOC = sum(is.na(FWADOCmgL)),
            na_FWA_NH4 = sum(is.na(FWANH4NmgL)),
            na_FWA_NO3 = sum(is.na(FWANO3NmgL)),
            na_FWA_TDP = sum(is.na(FWATDPmgL)),
            na_FWA_SO4 = sum(is.na(FWASO4SmgL))) %>%
  print(n = Inf)

## write to csv
write_csv(tbl_mon_yr_rng,
          file.path(here::here("analysis"), "tbl_mon_yr_rng.csv"))


##---------------------------------------------------
## monthly solute-only data for unmanaged catchments
##---------------------------------------------------

## get FWA solutes for unmanaged catchments
solutes <- dat_mon %>%
  ## pull out unmanaged catchments
  filter(type == "unmanaged") %>%
  ## create decimal water year
  mutate(dec_water_yr = case_when(
       Month >= 10 ~ (Year + 1) + (Month - 10) / 12,
       Month < 10 ~ Year + (Month + 2) / 12)) %>%
  ## pull out years of interest (last year + 1 to get full water year)
  filter(dec_water_yr >= yr_first & dec_water_yr < (yr_last + 1)) %>%
  ## only keep solutes for now
  select(region:catchment, dec_water_yr, starts_with("FWA")) 
  

solutes %>%
  group_by(site, catchment) %>%
  summarise_at(-c(1:3), list(min = min, max = max), na.rm = TRUE) %>%
  print(n = Inf)


## correct units based on TKH's analysis (/data/unit_corrections.csv)
## converting g to mg
solutes <- solutes %>%
  mutate(FWADOCmgL = case_when(
    site == "BBWM" | site == "LEF" ~ FWADOCmgL * 1000,
    site != "BBWM" | site != "LEF" ~ FWADOCmgL)) %>%
  mutate(FWASO4SmgL = case_when(
    site == "DOR" ~ FWASO4SmgL * 1000,
    site != "DOR" ~ FWASO4SmgL)) %>%
  mutate(FWANH4NmgL = case_when(
    site == "LEF" ~ FWANH4NmgL * 1000,
    site != "LEF" ~ FWANH4NmgL)) %>%
  mutate(FWANO3NmgL = case_when(
    site == "LEF" ~ FWANO3NmgL * 1000,
    site != "LEF" ~ FWANO3NmgL))

## write solutes to file
readr::write_csv(solutes,
                 file.path(here::here("data"), "tbl_solutes_unmanaged_mon.csv"))

## write solutes to file
# readr::write_csv(solutes,
#                  file.path(here::here("data"), "tbl_solutes_mon.csv"))



# ## empty lists for tmp data
# solutes_unmanaged_mon <- list()
# solutes_managed_mon <- list()
# 
# ## create solute-specific tables of year-by-site/catchment
# for(i in solutes) {
#   ## unmanaged
#   solutes_unmanaged_mon[[i]] <- dat_mon %>%
#     filter(type == "unmanaged") %>%
#     select(region:WaterYearMonth, all_of(i)) %>%
#     pivot_wider(names_from = c(region, site, catchment),
#                 values_from = i) %>%
#     arrange(WaterYear) %>%
#     mutate(solute = sub("(FWA)(.*)(mgL)", "\\2", i)) %>%
#     select(solute, everything(), -type)
#   ## managed
#   solutes_managed_mon[[i]] <- dat_mon %>%
#     filter(type == "managed") %>%
#     select(region:WaterYearMonth, all_of(i)) %>%
#     pivot_wider(names_from = c(region, site, catchment),
#                 values_from = i) %>%
#     arrange(WaterYear) %>%
#     mutate(solute = sub("(FWA)(.*)(mgL)", "\\2", i)) %>%
#     select(solute, everything(), -type)
# }
# 
# ## combine lists into df
# solutes_unmanaged_mon <- do.call(rbind, solutes_unmanaged_mon)
# solutes_managed_mon <- do.call(rbind, solutes_managed_mon)
# 
# ## write to csv
# readr::write_csv(solutes_unmanaged_mon,
#                  file.path(here::here("data"), "tbl_solutes_unmanaged_mon.csv"))
# readr::write_csv(solutes_managed_mon,
#                  file.path(here::here("data"), "tbl_solutes_managed_mon.csv"))






