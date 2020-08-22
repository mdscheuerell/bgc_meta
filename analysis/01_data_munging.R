## load libraries
library(here)
library(readr)

## setup
## directories within /data/
data_dirs <- c("managed", "unmanaged")

## col names for monthly data
colnames_mon <- c("Year", "Month", "WaterYear", "WaterYearMonth", "CSMeanTempC", 
                  "CSPrecipitationmm", "Runoffmm", "StrCamgL", "StrDOCmgL", "StrNH4NmgL", 
                  "StrNO3NmgL", "StrTDPmgL", "StrSO4SmgL", "depNH4Nkgha", "depNO3Nkgha", 
                  "depTotNkgha", "depSO4Skgha", "NAMGridTempC", "NAMGridPrecipitationmm", 
                  "FWACamgL", "FWADOCmgL", "FWANH4NmgL", "FWANO3NmgL", "FWATDPmgL", 
                  "FWASO4SmgL")

## col names for annual data
colnames_ann <- c("WaterYear", "CSMeanTempC", "CSPrecipitationmm", "Runoffmm", 
                  "StrCamgL", "StrDOCmgL", "StrNH4NmgL", "StrNO3NmgL", "StrTPmgL", 
                  "StrSO4SmgL", "depNH4Nkgha", "depNO3Nkgha", "depTotNkgha", "depSO4Skgha", 
                  "NAMGridTempC", "NAMGridPrecipitationmm", "FWACamgL", "FWADOCmgL", 
                  "FWANH4NmgL", "FWANO3NmgL", "FWATPmgL", "FWASO4SmgL", "iNDVI")

## empty data files
dat_mon <- NULL
dat_ann <- NULL


## read data
## loop over data directories
for(dd in data_dirs) {
  
  ## data directory
  data_dir <- here(paste0("data/",dd))
  
  ## monthly files
  files_mon <- grep("monthly", dir(data_dir), value = TRUE)
  
  ## loop over monthly files
  for(mm in files_mon) {
    
    ## read file
    tmp_m <- read_csv(file.path(data_dir, mm),
                    na = c("", "NA", "N/A"))
    ## remove spaces from col names
    colnames(tmp_m) <- sub(" ", "", colnames(tmp_m))
    
    ## check if file col names match set names
    col_chk <- colnames_mon %in% colnames(tmp_m)
    if(!all(col_chk)) {
      col_miss <- colnames_mon[!col_chk]
      mis <- matrix(NA, nrow(tmp_m), length(col_miss))
      colnames(mis) <- col_miss
      tmp_m <- cbind(tmp_m, mis)
    }
    
    ## add watershed type and site name
    tmp_m <- cbind(type = dd,
                 site = sub("_monthly.csv", "", mm),
                 tmp_m[,colnames_mon])
    ## add to monthly data
    dat_mon <- rbind(dat_mon, tmp_m)
    
  }
  
  ## annual files
  files_ann <- grep("annual", dir(data_dir), value = TRUE)
  
  ## loop over monthly files
  for(aa in files_ann) {
    
    ## read file
    tmp_a <- read_csv(file.path(data_dir, aa),
                    na = c("", "NA", "N/A"))
    ## remove spaces from col names
    colnames(tmp_a) <- sub(" ", "", colnames(tmp_a))
    
    ## check if file col names match set names
    col_chk <- colnames_ann %in% colnames(tmp_a)
    if(!all(col_chk)) {
      col_miss <- colnames_ann[!col_chk]
      mis <- matrix(NA, nrow(tmp_a), length(col_miss))
      colnames(mis) <- col_miss
      tmp_a <- cbind(tmp_a, mis)
    }
    
    ## add watershed type and site name
    tmp_a <- cbind(type = dd,
                 site = sub("_wateryearannual.csv", "", aa),
                 tmp_a[,colnames_ann])
    ## add to annual data
    dat_ann <- rbind(dat_ann, tmp_a)
    
  }
  
}

## write monthly data to file
write_csv(dat_mon, file.path(here("data"), "monthly_data.csv"))

## write annual data to file
write_csv(dat_ann, file.path(here("data"), "annual_data.csv"))


