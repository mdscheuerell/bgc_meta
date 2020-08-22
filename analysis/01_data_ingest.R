## load libraries
library(here)
library(readr)

## setup
data_dirs <- c("managed", "unmanaged")

col_names <- list(managed = list(mon = list(), ann = list()),
                  unmanaged = list(mon = list(), ann = list()))

colnames_mon <- c("Year", "Month", "WaterYear", "WaterYearMonth", "CSMeanTempC", 
                  "CSPrecipitationmm", "Runoffmm", "StrCamgL", "StrDOCmgL", "StrNH4NmgL", 
                  "StrNO3NmgL", "StrTDPmgL", "StrSO4SmgL", "depNH4Nkgha", "depNO3Nkgha", 
                  "depTotNkgha", "depSO4Skgha", "NAMGridTempC", "NAMGridPrecipitationmm", 
                  "FWACamgL", "FWADOCmgL", "FWANH4NmgL", "FWANO3NmgL", "FWATDPmgL", 
                  "FWASO4SmgL")

colnames_ann <- c("WaterYear", "CSMeanTempC", "CSPrecipitationmm", "Runoffmm", 
                  "StrCamgL", "StrDOCmgL", "StrNH4NmgL", "StrNO3NmgL", "StrTPmgL", 
                  "StrSO4SmgL", "depNH4Nkgha", "depNO3Nkgha", "depTotNkgha", "depSO4Skgha", 
                  "NAMGridTempC", "NAMGridPrecipitationmm", "FWACamgL", "FWADOCmgL", 
                  "FWANH4NmgL", "FWANO3NmgL", "FWATPmgL", "FWASO4SmgL", "iNDVI")

dat_mon <- NULL

dat_ann <- NULL

## read data

# for(dd in 1:length(data_dirs)) {
for(dd in data_dirs) {
  
  ## data directory
  data_dir <- here(paste0("data/",dd))
  
  ## monthly files
  files_mon <- grep("monthly", dir(data_dir), value = TRUE)
  
  for(mm in files_mon) {
    
    ## read file
    tmp <- read_csv(file.path(data_dir, mm),
                    na = c("", "NA", "N/A"))
    ## remove spaces from col names
    colnames(tmp) <- sub(" ", "", colnames(tmp))
    ## check if file col names match set names
    col_chk <- colnames_mon %in% colnames(tmp)
    if(!all(col_chk)) {
      col_miss <- colnames_mon[!col_chk]
      mis <- matrix(NA, nrow(tmp), length(col_miss))
      colnames(mis) <- col_miss
      tmp <- cbind(tmp, mis)
    }
    
    ## add watershed type and site name
    tmp <- cbind(type = dd,
                 site = sub("_monthly.csv", "", mm),
                 tmp[,colnames_mon])
    ## add to monthly data
    dat_mon <- rbind(dat_mon, tmp)
    
    # col_names[[dd]][["mon"]][[site]] <- colnames(tmp)
    
  }
  
  
  ## annual
  # files_ann <- grep("annual", dir(data_dirs[dd]), value = TRUE)  
  # 
  # for(aa in 1:length(files_ann)) {
  #   
  #   tmp <- read_csv(file.path(data_dirs[dd], files_ann[aa]))
  #   site <- sub("_wateryearannual.csv", "", files_ann[aa])
  #   col_names[[dd]][["ann"]][[site]] <- colnames(tmp)
  #   
  # }

}
