## load libraries
library(here)
library(readr)

## setup
data_dirs <- c(here("data/managed"),
               here("data/unmanaged"))

col_names <- list(managed = list(mon = list(), ann = list()),
                  unmanaged = list(mon = list(), ann = list()))

colnames_mon <- c("Year", "Month", "WaterYear", "WaterYearMonth", "CSMeanTempC", 
                  "CSPrecipitationmm", "Runoffmm", "StrCamgL", "StrDOCmgL", "StrNH4NmgL", 
                  "StrNO3NmgL", "StrSO4SmgL", "depNH4Nkgha", "depNO3Nkgha", "depTotNkgha", 
                  "depSO4Skgha", "NAMGridTempC", "NAMGridPrecipitationmm")

colnames_ann <- c("Water Year", "CSMeanTempC", "CSPrecipitationmm", "Runoffmm", 
                  "StrCamgL", "StrDOCmgL", "StrNH4NmgL", "StrNO3NmgL", "StrTPmgL", 
                  "StrSO4SmgL", "depNH4Nkgha", "depNO3Nkgha", "depTotNkgha", "depSO4Skgha", 
                  "NAMGridTempC", "NAMGridPrecipitationmm", "FWACamgL", "FWADOCmgL", 
                  "FWANH4NmgL", "FWANO3NmgL", "FWATPmgL", "FWASO4SmgL", "iNDVI")

## read data

for(dd in 1:length(data_dirs)) {
  
  ## monthly
  mon_files <- grep("monthly", dir(data_dirs[dd]), value = TRUE)
  
  for(mm in 1:length(mon_files)) {
    
    tmp <- read_csv(file.path(data_dirs[dd], mon_files[mm]))
    site <- sub("_monthly.csv", "", mon_files[mm])
    col_names[[dd]][["mon"]][[site]] <- colnames(tmp)
    
  }
  
  ## annual
  ann_files <- grep("annual", dir(data_dirs[dd]), value = TRUE)  
  
  for(aa in 1:length(ann_files)) {
    
    tmp <- read_csv(file.path(data_dirs[dd], ann_files[aa]))
    site <- sub("_wateryearannual.csv", "", ann_files[aa])
    col_names[[dd]][["ann"]][[site]] <- colnames(tmp)
    
  }

}
