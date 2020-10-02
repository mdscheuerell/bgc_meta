### Manage mean daily discharge ###
## TKH
## Convert all to L/s

library(here)
library(tidyverse)
library(rio)

### Read data from tabs of excel spreadsheet
# Uses rio::import_list. Date formatting of LEF-MPR differs from all others despite efforts to coerce all dates to same format in excel
Qin <- import_list(here("data", "Daily Discharge - All Sites.xlsx"))
# Result = list of 37 sites

# Replace problematic "-" with "_" in list element names (any of the following should work)
Qin <- lapply(Qin, function(x) setNames(x, sub("-", "_", names(x)))) #this one worked once

#Qin <- map(Qin, ~sub("-", "_", names(.x)))

#Qin <- lapply(Qin, function(x) sub("-", "_", names(x)))

Qin <- lapply(Qin, function(x) {
  names(x)[grep("-", names(x))] <- "_"
  return(x)})

change_names <- function(x) {
  names(x) <- sub("-", "_", names(x))
  x
}
Qin <- map(Qin, ~change_names(.x))

########
## Going to brute force names of list items...ugh
names(Qin)[1] <- "BBWM_EB"
names(Qin)[2] <- "CWT_WS7"
names(Qin)[3] <- "CWT_WS18"
names(Qin)[4] <- "DOR_HP3"
names(Qin)[5] <- "DOR_HP3A"
names(Qin)[6] <- "DOR_HP5"
names(Qin)[7] <- "DOR_HP6"
names(Qin)[8] <- "DOR_HP6A"
names(Qin)[9] <- "ELA_EIF"
names(Qin)[10] <- "ELA_NEIF"
names(Qin)[11] <- "ELA_NWIF"
names(Qin)[12] <- "HBEF_WS1"
names(Qin)[13] <- "HBEF_WS6"
names(Qin)[14] <- "HBEF_WS7"
names(Qin)[15] <- "HBEF_WS8"
names(Qin)[16] <- "HBEF_WS9"
names(Qin)[17] <- "HJA_WS6"
names(Qin)[18] <- "HJA_WS7"
names(Qin)[19] <- "HJA_WS8"
names(Qin)[20] <- "HJA_WS9"
names(Qin)[21] <- "HJA_WS10"
names(Qin)[22] <- "LEF_QS"
names(Qin)[23] <- "LEF_RI"
names(Qin)[24] <- "LEF_MPR"
names(Qin)[25] <- "MEF_S2"
names(Qin)[26] <- "MEF_S4"
names(Qin)[27] <- "MEF_S5"
names(Qin)[28] <- "MEF_S6"
names(Qin)[29] <- "SEF_WS77"
names(Qin)[30] <- "SEF_WS80"
names(Qin)[31] <- "SLP_WS9"
names(Qin)[32] <- "TLW_C31"
names(Qin)[33] <- "TLW_C32"
names(Qin)[34] <- "TLW_C33"
names(Qin)[35] <- "TLW_C34"
names(Qin)[36] <- "TLW_C35"
names(Qin)[37] <- "TLW_C38"

# Add Site column to each list element
Qin <- purrr::imap(Qin, ~mutate(.x, Site = .y))

## Convert units to L s-1
## Sites already in L s-1 = CWT_WS7, CWT_WS18, ELA_EIF, ELA_NEIF, ELA_NWIF, HBEF_WS6, HBEF_WS7, HBEF_WS8, HBEF_WS9, SEF_WS77, SEF_WS80

Ls_sites <- c("CWT_WS7", "CWT_WS18", "ELA_EIF", "ELA_NEIF", "ELA_NWIF", "HBEF_WS6", "HBEF_WS7", "HBEF_WS8", "HBEF_WS9", "SEF_WS77", "SEF_WS80")
Q_Ls <- Qin[Ls_sites]

# Column for original Q units
Q_Ls <- mapply(cbind, Q_Ls, "discharge_units" = "L_s", SIMPLIFY = FALSE)

# Column for Q in L s-1
Q_Ls <- mapply(cbind, Q_Ls, "discharge_Ls" = NA, SIMPLIFY = FALSE)

Q_Ls <- lapply(Q_Ls, function(x) {
  x$discharge_Ls <- x[[2]]
  return(x)
})

## Sites in cfs = BBWM_EB, HJA_WS6, HJA_WS7, HJA_WS8, HJA_WS9, WJA_WS10, LEF_QS, LEF_RI
cfs_sites <- c("BBWM_EB", "HJA_WS6", "HJA_WS7", "HJA_WS8", "HJA_WS9", "HJA_WS10", "LEF_QS", "LEF_RI")
# Are HJA really in cfs?

Q_cfs <- Qin[cfs_sites]

# Column for original Q units
Q_cfs <- mapply(cbind, Q_cfs, "discharge_units" = "cfs", SIMPLIFY = FALSE)

# Column for Q in L s-1
Q_cfs <- mapply(cbind, Q_cfs, "discharge_Ls" = NA, SIMPLIFY = FALSE)

# Convert cfs to L s-1: 1 cf = 28.316847 L
Q_cfs <- lapply(Q_cfs, function(x) {
                       x$discharge_Ls <- x[[2]]*28.317
                       return(x)
})
  
## Sites in m3 s-1 = DOR_HP3, DOR_HP3A, DOR_HP5, DOR_HP6, DOR_HP6A
m3_sites <- c("DOR_HP3", "DOR_HP3A", "DOR_HP5", "DOR_HP6", "DOR_HP6A")

Q_m3 <- Qin[m3_sites]

# Column for original Q units
Q_m3 <- mapply(cbind, Q_m3, "discharge_units" = "m3s", SIMPLIFY = FALSE)

# Column for Q in L s-1
Q_m3 <- mapply(cbind, Q_m3, "discharge_Ls" = NA, SIMPLIFY = FALSE)

# Convert cfs to L s-1: 1 cf = 28.316847 L
Q_m3 <- lapply(Q_m3, function(x) {
  x$discharge_Ls <- x[[2]]*1000
  return(x)
})

## Sites in mm/d: LEF_MPR, SLP_WS9, TLW_C31, TLW_C32, TLW_C33, TLW_C34, TLW_C35, TLW_C38
mm_sites <- c("LEF_MPR", "SLP_WS9", "TLW_C31", "TLW_C32", "TLW_C33", "TLW_C34", "TLW_C35", "TLW_C38")

Q_mm <- Qin[mm_sites]

# Column for original Q units
Q_mm <- mapply(cbind, Q_mm, "discharge_units" = "mm_d", SIMPLIFY = FALSE)

# Column for Q in L s-1
Q_mm <- mapply(cbind, Q_mm, "discharge_Ls" = NA, SIMPLIFY = FALSE)

# Column for catchment area
Q_mm <- mapply(cbind, Q_mm, "catch_area_km2" = NA, SIMPLIFY = FALSE)

# Catchment areas (km2): 
  # LEF_QS = 2.6159
  # LEF_RI = 3.26
  # LEF_MPR = 17.8191
  # SLP_WS9 = 0.405
  # TLW_C31 = 0.0494
  # TLW_C32 = 0.0650
  # TLW_C33 = 0.2338
  # TLW_C34 = 0.6859
  # TLW_C35 = 0.0402
  # TLW_C38 = 0.0646
  # Note TLW watershed areas differ among publications. These values reported in Mengitsu HP 2013

# Populate catchment area column
# Breaking the list into individual dataframes (gave up on trying to assign values for individual catchments within list)
list2env(Q_mm, envir=.GlobalEnv)

LEF_MPR$catch_area_km2 <- 17.8191
SLP_WS9$catch_area_km2 <- 0.405
TLW_C31$catch_area_km2 <- 0.0494
TLW_C32$catch_area_km2 <- 0.0650
TLW_C33$catch_area_km2 <- 0.2338
TLW_C34$catch_area_km2 <- 0.6859
TLW_C35$catch_area_km2 <- 0.0402
TLW_C38$catch_area_km2 <- 0.0646

# Reformat LEF_MPR date
LEF_MPR$DATE <- as.Date(LEF_MPR$DATE, format = "%m/%d/%Y")

Q_mm <- list(LEF_MPR = LEF_MPR,
             SLP_WS9 = SLP_WS9, 
             TLW_C31 = TLW_C31, 
             TLW_C32 = TLW_C32, 
             TLW_C33 = TLW_C33, 
             TLW_C34 = TLW_C34, 
             TLW_C35 = TLW_C35, 
             TLW_C38 = TLW_C38)

# Convert mm/d to L s-1: mm/d * catchment area (km2) * 10^6 (convert to L) * 1/86500 (convert d to s)
Q_mm <- lapply(Q_mm, function(x) {
  x$discharge_Ls = x[[2]]*x[['catch_area_km2']]*10^6*(1/86400)
  return(x)
})

## Sites in cm/d: MEF_S2, MF_S4, MEF_S5, MEF_S6
cm_sites <- c("MEF_S2", "MEF_S4", "MEF_S5", "MEF_S6")
  
Q_cm <- Qin[cm_sites]
  
# Column for original Q units
Q_cm <- mapply(cbind, Q_cm, "discharge_units" = "cm_d", SIMPLIFY = FALSE)

# Column for Q in L s-1
Q_cm <- mapply(cbind, Q_cm, "discharge_Ls" = NA, SIMPLIFY = FALSE)

# Column for catchment area
Q_cm <- mapply(cbind, Q_cm, "catch_area_km2" = NA, SIMPLIFY = FALSE)

# Catchment areas (km2):
  # MEF_S2 = 0.097
  # MEF_S4 = 0.340
  # MEF_S5 = 0.526
  # MEF_S6 = 0.089

# Populate catchment area column
# Breaking the list into individual dataframes 
list2env(Q_cm, envir=.GlobalEnv)

MEF_S2$catch_area_km2 <- 0.097
MEF_S4$catch_area_km2 <- 0.340
MEF_S5$catch_area_km2 <- 0.526
MEF_S6$catch_area_km2 <- 0.089

Q_cm <- list(MEF_S2 = MEF_S2,
             MEF_S4 = MEF_S4,
             MEF_S5 = MEF_S5,
             MEF_S6 = MEF_S6)

# Convert cm/d to L s-1: cm/d * catchment area (km2) * 10^5 (convert to L) * 1/86500 (convert d to s)
Q_cm <- lapply(Q_cm, function(x) {
  x$discharge_Ls = x[[2]]*x[['catch_area_km2']]*10^7*(1/86400)
  return(x)
})

## Make a single data frame for output and plotting
Qall <- list(Q_Ls, Q_cfs, Q_m3, Q_mm, Q_cm)
Qall <- unlist(Qall, recursive = FALSE)

Qall <- lapply(Qall, function(x) {
  x$site <- x$Site
  return(x)
})

Qall <- lapply(Qall, separate, site, c("studyarea", "watershed"))

# Per Irena, drop LEF_MPR (also problems formatting date for this site)
#Qall <- within(Qall, rm(LEF_MPR)) 

## Collapse to a single dataframe for plotting
Qall.df <- bind_rows(Qall, .id = "SiteID")

Qall.df <- Qall.df %>% select(SiteID:watershed, DATE)

Qall.df$Date <- coalesce(Qall.df$Date, Qall.df$DATE)

# Plot
Qall.pl <- Qall.df %>% ggplot(aes(x = Date, y = discharge_Ls, color = watershed)) +
  geom_point() +
  geom_line() +
  facet_wrap(~SiteID, scales = "free_y")

# Data gaps: SEF 1980s, HJA_WS7 10/1/1987-9/30/1994

ggsave(path = "plots", filename = "discharge.pdf", width = 10, height = 10, units = "in")

write.csv(Qall.df, "Qall.df.csv", row.names = FALSE)
