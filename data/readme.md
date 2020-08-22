# Data files

There are 2 data files in this directory, each containing site specific time series of biogeochemistry data:

1) `monthly_data.csv`

2) `annual_data.csv`

Both of them were created by the script `/analysis/01_data_munging.R`, which takes information from the files located in these sub-directories:

* `/managed/`

* `/unmanaged/`


## Metadata

### Monthly data

| Column name | Description |
|:------------|:--------------|
| Year | calendar year |
| Month | month of year |
| WaterYear | calculated water year |
| WaterYearMonth | concatenated water year and month |
| CSmeanTempC | climate station mean monthly temperature (C) 
| CSPrecipitationmm | climate station sum of monthly precipitation (mm) |
| Runoffmm | monthly sum of runoff (mm) |
| StrCamgL | stream solute monthly mean concentration of Ca (mg/L) |
| StrDOCmgL | stream solute monthly mean concentration of DOC (mg/L) |
| StrNH4NmgL | stream solute monthly mean concentration of NH4-N (mg/L) |
| StrNO3NmgL | stream solute monthly mean concentration of NO3-N (mg/L) |
| StrTDPmgL | stream solute monthly mean concentration of TDP (mg/L) |
| StrSO4SmgL | stream solute monthly mean concentration of SO4-S (mg/L) |
| depNH4Nkgha | monthly sum of deposition of NH4-N (kg/ha) |
| depNO3Nkgha | monthly sum of deposition of NO3-N (kg/ha) |
| depTotNkgha | monthly sum of deposition of N_atm (sum of NO3-N and NH4-N) (kg/ha) |
| depSO4Skgha | monthly sum of deposition of SO4-S (kg/ha) |
| NAMGridTempC | mean monthly temperature from Canadian Forest Services Historical Climate Grids |
| NAMGridPrecipitationmm | sum of monthly Precipitation from Canadian Forest Services Historical Climate Grids |
| FWACamgL | flow weighted-mean concentration of Ca (mg/L) |
| FWADOCmgL | flow weighted-mean concentration of DOC (mg/L) |
| FWANH4NmgL | flow weighted-mean concentration of NH4-N (mg/L) |
| FWANO3NmgL | flow weighted-mean concentration of NO3-N (mg/L) |
| FWATDPmgL | flow weighted-mean concentration of TDP (mg/L) |
| FWASO4SmgL | flow weighted-mean concentration of SO4-S (mg/L) |

### Annual data

| Column name | Description |
|:------------|:--------------|
| WaterYear | calculated water year |
| CSmeanTempC | Climate Station mean annual temperature (C) |
| CSPrecipitationmm | Climate Station sum of annual Precipitation (mm) |
| Runoffmm | annual sum of runoff (mm) |
| StrCamgL | stream solute annual mean concentration of Ca (mg/L) |
| StrDOCmgL | stream solute annual mean concentration of DOC (mg/L) |
| StrNH4NmgL | stream solute annual mean concentration of NH4-N (mg/L) |
| StrNO3NmgL | stream solute annual mean concentration of NO3-N (mg/L) |
| StrTDPmgL | stream solute annual mean concentration of TDP (mg/L) |
| StrSO4SmgL | stream solute annual mean concentration of SO4-S (mg/L) |
| depNH4Nkgha | annual sum of deposition of NH4-N (kg/ha) |
| depNO3Nkgha | annual sum of deposition of NO3-N (kg/ha) |
| depTotNkgha | annual sum of deposition of N_atm (sum of NO3N and NH4N) (kg/ha) |
| depSO4Skgha | annual sum of deposition of SO4-S (kg/ha) |
| NAMGridTempC | mean monthly temperature from Canadian Forest Services Historical Climate Grids |
| NAMGridPrecipitationmm | sum of annual Precipitation from Canadian Forest Services Historical Climate Grids |
| FWACamgL | flow weighted-mean concentration of Ca (mg/L) |
| FWADOCmgL | flow weighted-mean concentration of DOC (mg/L) |
| FWANH4NmgL | flow weighted-mean concentration of NH4-N (mg/L) |
| FWANO3NmgL | flow weighted-mean concentration of NO3-N (mg/L) |
| FWATDPmgL | flow weighted-mean concentration of TDP (mg/L) |
| FWASO4SmgL | flow weighted-mean concentration of SO4-S (mg/L) |
| iNDVI | integrated normalized difference vegetation index (unitless) |

#### Notes

HJA: NH4-N is labeled as NH3-N  

MEF: TDP is TP, DOC is TOC TC-IC* 

SEF: TDP is TP 

LEF: PO4P instead of TDP** 

HBEF: PO4P instead of TDP 
