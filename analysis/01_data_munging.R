## This script extracts information from each of the site-specific files
## located in the /data/managed/ and /data/unmanaged/ directories, and
## writes all of it to 2 different files based on the temporal resoulation:
## 1) monthly_data.csv
## 2) annual_data.csv

##-------------------
## required packages
##-------------------

## library(here)
## library(readr)

##-------
## setup
##-------

## directories within /data/
data_dirs <- c("managed", "unmanaged")

## col names for monthly data
colnames_mon <- c("Year", "Month", "WaterYear", "WaterYearMonth",
                  "CSMeanTempC", "CSPrecipitationmm", "Runoffmm",
                  "StrCamgL", "StrDOCmgL", "StrNH4NmgL", "StrNO3NmgL", 
                  "StrTDPmgL", "StrSO4SmgL",
                  "depNH4Nkgha", "depNO3Nkgha", "depTotNkgha", "depSO4Skgha",
                  "NAMGridTempC", "NAMGridPrecipitationmm", 
                  "FWACamgL", "FWADOCmgL", "FWANH4NmgL",
                  "FWANO3NmgL", "FWATDPmgL", "FWASO4SmgL")

## col names for annual data
colnames_ann <- c("WaterYear",
                  "CSMeanTempC", "CSPrecipitationmm", "Runoffmm",
                  "StrCamgL", "StrDOCmgL", "StrNH4NmgL", "StrNO3NmgL", 
                  "StrTDPmgL", "StrSO4SmgL",
                  "depNH4Nkgha", "depNO3Nkgha", "depTotNkgha", "depSO4Skgha",
                  "NAMGridTempC", "NAMGridPrecipitationmm", 
                  "FWACamgL", "FWADOCmgL", "FWANH4NmgL",
                  "FWANO3NmgL", "FWATDPmgL", "FWASO4SmgL",
                  "iNDVI")

## empty data matrices
dat_mon <- NULL
dat_ann <- NULL

##------------
## munge data
##------------

## loop over data directories
for(dd in data_dirs) {
  
  ## path to data directory
  data_dir <- here::here(paste0("data/",dd))
  
  ## monthly files
  files_mon <- grep("monthly", dir(data_dir), value = TRUE)
  
  ## loop over monthly files
  for(mm in files_mon) {
    
    ## read file
    tmp_m <- readr::read_csv(file.path(data_dir, mm),
                             na = c("", "NA", "N/A", "#DIV/0!", "#VALUE!"))
    ## remove spaces & commas from col names
    colnames(tmp_m) <- gsub(" ", "", colnames(tmp_m))
    colnames(tmp_m) <- gsub(",", "", colnames(tmp_m))
    ## replace different names for P
    colnames(tmp_m) <- gsub("TPmgL", "TDPmgL", colnames(tmp_m))
    colnames(tmp_m) <- gsub("PO4PmgL", "TDPmgL", colnames(tmp_m))
    ## replace different names for N
    colnames(tmp_m) <- gsub("NO3NO2NmgL", "NO3NmgL", colnames(tmp_m))
    ## replace different names for C
    colnames(tmp_m) <- gsub("TOCTCICmgL", "DOCmgL", colnames(tmp_m))

    ## check if file col names match set names
    col_chk <- colnames_mon %in% colnames(tmp_m)
    if(!all(col_chk)) {
      col_miss <- colnames_mon[!col_chk]
      mis <- matrix(NA, nrow(tmp_m), length(col_miss))
      colnames(mis) <- col_miss
      tmp_m <- cbind(tmp_m, mis)
    }
    
    ## add watershed type, site & catchment
    tmp_m <- cbind(type = dd,
                   site = sub("([A-Z]{3,4})(_.*)", "\\1", mm),
                   catchment = sub("([A-Z]{3,4}_)(.*)(_monthly.csv)", "\\2", mm),
                   tmp_m[,colnames_mon])
    
    ## concatenate with monthly data
    dat_mon <- rbind(dat_mon, tmp_m)
    
    class(dat_mon$CSMeanTempC)
    
  } ## end loop over monthly files
  
  ## annual files
  files_ann <- grep("annual", dir(data_dir), value = TRUE)
  
  ## loop over annual files
  for(aa in files_ann) {
    
    ## read file
    tmp_a <- readr::read_csv(file.path(data_dir, aa),
                             na = c("", "NA", "N/A", "#DIV/0!", "#VALUE!"))
    ## remove spaces & commas from col names
    colnames(tmp_a) <- gsub(" ", "", colnames(tmp_a))
    colnames(tmp_a) <- gsub(",", "", colnames(tmp_a))
    ## replace different names for P
    colnames(tmp_a) <- gsub("TPmgL", "TDPmgL", colnames(tmp_a))
    colnames(tmp_a) <- gsub("PO4PmgL", "TDPmgL", colnames(tmp_a))
    ## replace different names for N
    colnames(tmp_a) <- gsub("NO3NO2NmgL", "NO3NmgL", colnames(tmp_a))
    ## replace different names for C
    colnames(tmp_a) <- gsub("TOCTCICmgL", "DOCmgL", colnames(tmp_a))
    
    ## check if file col names match set names
    col_chk <- colnames_ann %in% colnames(tmp_a)
    if(!all(col_chk)) {
      col_miss <- colnames_ann[!col_chk]
      mis <- matrix(NA, nrow(tmp_a), length(col_miss))
      colnames(mis) <- col_miss
      tmp_a <- cbind(tmp_a, mis)
    }
    
    ## add watershed type, site & catchment
    tmp_a <- cbind(type = dd,
                   site = sub("([A-Z]{3,4})(_.*)", "\\1", aa),
                   catchment = sub("([A-Z]{3,4}_)(.*)(_wateryearannual.csv)", "\\2", aa),
                   tmp_a[,colnames_ann])
    
    ## concatenate with annual data
    dat_ann <- rbind(dat_ann, tmp_a)
    
  } ## end loop over annual files
  
} ## end loop over directories

## change WaterYearMonth to decimal years
dat_mon <- dat_mon %>%
  mutate(WaterYearMonth = case_when(
  Month >= 10 ~ (Year + 1) + (Month - 10) / 12,
  Month < 10 ~ Year + (Month + 2) / 12
))

## assign regions to sites
dat_mon <- dat_mon %>%
  mutate(region = case_when(
    site == "HJA" ~ "WC",
    site == "ELA" | site == "MEF" | site == "TLW" | site == "DOR" ~ "GL",
    site == "HBEF" | site == "BBWM" | site == "SLP" ~ "NE",
    site == "CWT" | site == "SEF" ~ "SE",
    site == "LEF" ~ "PR"
  )) %>%
  select(region, site, catchment, type, everything())

dat_ann <- dat_ann %>%
  mutate(region = case_when(
    site == "HJA" ~ "WC",
    site == "ELA" | site == "MEF" | site == "TLW" | site == "DOR" ~ "GL",
    site == "HBEF" | site == "BBWM" | site == "SLP" ~ "NE",
    site == "CWT" | site == "SEF" ~ "SE",
    site == "LEF" ~ "PR"
  )) %>%
  select(region, site, catchment, type, everything())

## write monthly data to file
readr::write_csv(dat_mon,
                 file.path(here::here("data"), "monthly_data.csv"))

## write annual data to file
readr::write_csv(dat_ann,
                 file.path(here::here("data"), "annual_data.csv"))


##-----------------------------------------------------
## solute-only data for managed & unmanaged catchments
##-----------------------------------------------------

## get names of solutes
solutes <- dat_ann %>%
  select(starts_with("FWA")) %>%
  colnames()

## ANNUAL DATA

## empty lists for tmp data
solutes_unmanaged_ann <- list()
solutes_managed_ann <- list()

## create solute-specific tables of year-by-site/catchment
for(i in solutes) {
  ## unmanaged
  solutes_unmanaged_ann[[i]] <- dat_ann %>%
    filter(type == "unmanaged") %>%
    select(region:WaterYear, all_of(i)) %>%
    pivot_wider(names_from = c(region, site, catchment), values_from = i) %>%
    arrange(WaterYear) %>%
    mutate(solute = sub("(FWA)(.*)(mgL)", "\\2", i)) %>%
    select(solute, everything(), -type)
  ## managed
  solutes_managed_ann[[i]] <- dat_ann %>%
    filter(type == "managed") %>%
    select(region:WaterYear, all_of(i)) %>%
    pivot_wider(names_from = c(region, site, catchment), values_from = i) %>%
    arrange(WaterYear) %>%
    mutate(solute = sub("(FWA)(.*)(mgL)", "\\2", i)) %>%
    select(solute, everything(), -type)
}

## combine lists into df
solutes_unmanaged_ann <- do.call(rbind, solutes_unmanaged_ann)
solutes_managed_ann <- do.call(rbind, solutes_managed_ann)

## write to csv
readr::write_csv(solutes_unmanaged_ann,
                 file.path(here::here("data"), "tbl_solutes_unmanaged_ann.csv"))
readr::write_csv(solutes_managed_ann,
                 file.path(here::here("data"), "tbl_solutes_managed_ann.csv"))


## MONTHLY DATA

## empty lists for tmp data
solutes_unmanaged_mon <- list()
solutes_managed_mon <- list()

## create solute-specific tables of year-by-site/catchment
for(i in solutes) {
  ## unmanaged
  solutes_unmanaged_mon[[i]] <- dat_mon %>%
    filter(type == "unmanaged") %>%
    select(region:WaterYearMonth, all_of(i)) %>%
    group_by(region, site, catchment) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = c(region, site, catchment),
                values_from = i) %>%
    select(-row) %>%
    arrange(WaterYear) %>%
    mutate(solute = sub("(FWA)(.*)(mgL)", "\\2", i)) %>%
    select(solute, everything(), -type)
  ## managed
  solutes_managed_mon[[i]] <- dat_mon %>%
    filter(type == "managed") %>%
    select(region:WaterYearMonth, all_of(i)) %>%
    group_by(region, site, catchment) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = c(region, site, catchment),
                values_from = i) %>%
    select(-row) %>%
    arrange(WaterYear) %>%
    mutate(solute = sub("(FWA)(.*)(mgL)", "\\2", i)) %>%
    select(solute, everything(), -type)
}

## combine lists into df
solutes_unmanaged_mon <- do.call(rbind, solutes_unmanaged_mon)
solutes_managed_mon <- do.call(rbind, solutes_managed_mon)

## write to csv
readr::write_csv(solutes_unmanaged_mon,
                 file.path(here::here("data"), "tbl_solutes_unmanaged_mon.csv"))
readr::write_csv(solutes_managed_mon,
                 file.path(here::here("data"), "tbl_solutes_managed_mon.csv"))





