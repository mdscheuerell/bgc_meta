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
    
    mm <- files_mon[5]
    
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
  select(starts_with("Str")) %>%
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
    select(site:WaterYear, all_of(i)) %>%
    pivot_wider(names_from = c(site, catchment), values_from = i) %>%
    arrange(WaterYear) %>%
    mutate(solute = sub("(Str)(.*)(mgL)", "\\2", i)) %>%
    select(solute, everything())
  ## managed
  solutes_managed_ann[[i]] <- dat_ann %>%
    filter(type == "managed") %>%
    select(site:WaterYear, all_of(i)) %>%
    pivot_wider(names_from = c(site, catchment), values_from = i) %>%
    arrange(WaterYear) %>%
    mutate(solute = sub("(Str)(.*)(mgL)", "\\2", i)) %>%
    select(solute, everything())
}

## combine lists into df
solutes_unmanaged_ann <- do.call(rbind, solutes_unmanaged_ann)
solutes_managed_ann <- do.call(rbind, solutes_managed_ann)

## write to csv
readr::write_csv(solutes_unmanaged_ann,
                 file.path(here::here("data"), "tbl_solutes_unmanaged_ann.csv"))
readr::write_csv(solutes_managed_ann,
                 file.path(here::here("data"), "tbl_solutes_managed_ann.csv"))





