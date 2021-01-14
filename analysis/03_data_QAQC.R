### Plots of raw data for QAQC ###
## Author: TKH (just don't want anyone to mistake this for an MS script)

### Packages
library(tidyverse)
library(here)

## path to data directory
data_dir <- here::here(paste0("data"))

## categories for sites
site_type <- c("managed", "unmanaged")

## read data files
dat_ann <- readr::read_csv(file.path(data_dir, "annual_data.csv"))
dat_mon <- readr::read_csv(file.path(data_dir, "monthly_data.csv"))

### Plots ###
## Annual mean concentration ##
# Calcium
Ca.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrCamgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# 3 points in error at SLP (should be <80 mg/L at Sleepers)? Same 3 years have elevated DOC

# DOC
DOC.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrDOCmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

#units for BBWM and LEF are incorrect...values too low

# NH4
NH4.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrNH4NmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Should apply a common LOQ filter
# SLP errant value in 2001?
# LEF: elevated values 1980s...? TKH can check with McDowell

# NO3
NO3.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrNO3NmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Should apply a common LOQ filter

# TDP
TDP.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrTDPmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Separate analyses for sites that report TDP and PO4/SRP
# Need to apply an LOQ filter
# early record CWT & HBEF high values

# SO4
SO4.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrSO4SmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Check units for DOR
# Apply a common LOQ filter

## Flow-weighted annual mean concentration ##
# Calcium
Ca.fwa <- dat_ann %>% ggplot(aes(x = WaterYear, y = FWACamgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# DOC
DOC.fwa <- dat_ann %>% ggplot(aes(x = WaterYear, y = FWADOCmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

#NH4
NH4.fwa <- dat_ann %>% ggplot(aes(x = WaterYear, y = FWANH4NmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

#NO3
NO3.fwa <- dat_ann %>% ggplot(aes(x = WaterYear, y = FWANO3NmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

#TDP
TDP.fwa <- dat_ann %>% ggplot(aes(x = WaterYear, y = FWATDPmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# SO4
SO4.fwa <- dat_ann %>% ggplot(aes(x = WaterYear, y = FWASO4SmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

## Monthly mean concentration ##
# Calcium
Ca.fwm <- dat_mon %>% filter(type == "unmanaged") %>%
  ggplot(aes(x = WaterYear, y = FWACamgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Slepers high outliers
# Why does BBWM have annual FWACa, but not monthly?
# Why does LEF have monthly FWACa, but not annual?
# Units for LEF Ca are incorrect

# DOC
DOC.fwm <- dat_mon %>% filter(type == "unmanaged") %>% 
  ggplot(aes(x = WaterYear, y = FWADOCmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Units for LEF DOC are incorrect
# No monthly DOC for BBWM?
# Check DOC outliers for HBEF

#NH4
NH4.fwm <- dat_mon %>% filter(type == "unmanaged") %>%
  ggplot(aes(x = WaterYear, y = FWANH4NmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Check NH4 units for LEF

#NO3
NO3.fwm <- dat_mon %>% filter(type == "unmanaged") %>%
  ggplot(aes(x = WaterYear, y = FWANO3NmgL, color = catchment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Units incorrect for LEF

#TDP
TDP.fwm <- dat_mon %>% filter(type == "unmanaged") %>% 
  ggplot(aes(x = WaterYear, y = FWATDPmgL, color = catchment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Erroneous values at beginning of CWT record

# SO4
SO4.fwm <- dat_mon %>% filter(type == "unmanaged") %>%
  ggplot(aes(x = WaterYear, y = FWASO4SmgL, color = catchment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

ggsave(path = "plots", filename = "SO4.fwm.pdf", width = 11, height = 8)

# Are the outlier values at SLP (~3 points) & TLW (~6 points) real?

########
# Are FWA and concentrations identical (JMH)
####


# FYI – Here’s how this traces to the data used in the MARSS analyses: 
# It looks like Mark’s MARSS script uses “tbl_solutes_unmanaged_mon.csv” which only has FWMCs. 
# That can be traced back to “tbl_solutes_unmanaged_mon.csv” which was created in “02_data_screening”,
# which can be traced back to “monthly_data.csv” which was created in “01_data_munging”.
# “monthly_data.csv” has both concentrations and FWMCs. 

df <- readr::read_csv(file.path(here::here("data"), "monthly_data.csv")) %>% 
  filter(type == "unmanaged") %>% 
  filter(site %in% c("HJA", "ELA", "MEF", "TLW", "DOR", "HBEF", "BBWM")) %>% 
  # don't know why there are NA's in Year/Month
  filter(!is.na(Year)) %>% 
  mutate(date = as.POSIXct(paste0(Year, "-", Month, "-01"), format ="%Y-%m-%d")) %>% 
  select(site, catchment, date, StrCamgL:StrSO4SmgL, FWACamgL:FWASO4SmgL) 
  
df %>% 
  split(.$catchment) %>% 
  map(summary)

# rearranging data
df.conc <- df %>% 
  select(site:StrSO4SmgL) %>% 
  pivot_longer(col = StrCamgL:StrSO4SmgL, values_to = "values", names_to = "vars") %>% 
  mutate(solute = str_remove(vars, "Str"),
         var = "Conc") %>% 
  select(-vars)

df.fwa <- df %>% 
  select(site:date, FWACamgL:FWASO4SmgL) %>% 
  pivot_longer(col = FWACamgL:FWASO4SmgL, values_to = "values", names_to = "vars") %>% 
  mutate(solute = str_remove(vars, "FWA"),
         var = "FWA") %>% 
  select(-vars)

# data in long form 
df2 <- rbind(df.conc, df.fwa) %>% 
  mutate(across(c("site", "catchment", "solute", "var"), factor))

# data in wide form with conc and fwa in columns
df2w <- df2 %>% 
  pivot_wider(names_from = var, values_from = values)
  # pivot_wider(id_cols = c(catchment, Year, Month, solute), names_from = "var", values_from = "values")

# scatterplots of FWA v. Conc
ConFWA.Ca <- ggplot(df2w %>% 
         filter(solute == "CamgL"), aes(y = FWA, x = Conc)) +
  geom_point() +
  facet_wrap(vars(site, catchment), scales = "free") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("CamgL")

ConFWA.DOC <- ggplot(df2w %>% 
         filter(solute == "DOCmgL"), aes(y = FWA, x = Conc)) +
  geom_point() +
  facet_wrap(vars(site, catchment), scales = "free") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("DOCmgL")

ConFWA.NH4 <- ggplot(df2w %>% 
         filter(solute == "NH4NmgL"), aes(y = FWA, x = Conc)) +
  geom_point() +
  facet_wrap(vars(site, catchment), scales = "free") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("NH4NmgL")

ConFWA.NO3 <- ggplot(df2w %>% 
         filter(solute == "NO3NmgL"), aes(y = FWA, x = Conc)) +
  geom_point() +
  facet_wrap(vars(site, catchment), scales = "free") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("NO3NmgL")

ConFWA.SO4 <- ggplot(df2w %>% 
         filter(solute == "SO4SmgL"), aes(y = FWA, x = Conc)) +
  geom_point() +
  facet_wrap(vars(site, catchment), scales = "free") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("SO4SmgL")

ConFWA.TDP <- ggplot(df2w %>% 
         filter(solute == "TDPmgL"), aes(y = FWA, x = Conc)) +
  geom_point() +
  facet_wrap(vars(site, catchment), scales = "free") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("TDPmgL")

pdf(file.path(here::here("plots"), "ConcVfwaPlots.pdf"), height = 8, width = 12)
ConFWA.Ca
ConFWA.DOC
ConFWA.NH4
ConFWA.NO3
ConFWA.SO4
ConFWA.TDP
dev.off()

# timeseries of FWA and conc
ConWTA.ts.Ca <- ggplot(df2 %>% 
         filter(solute == "CamgL"), aes(y = log(values), x = date, color = var)) +
  geom_line(alpha = 0.25) +
  facet_wrap(vars(site, catchment), scales = "free_y") +
  ggtitle("CamgL")

ConWTA.ts.Ca.C34 <- ggplot(df2 %>% 
                         filter(solute == "CamgL") %>% 
                        filter(catchment == "C34"), aes(y = log(values), x = date, color = var)) +
  geom_line(alpha = 0.25) +
  ggtitle("CamgL Catchment C34")

ConWTA.ts.DOC <- ggplot(df2 %>% 
         filter(solute == "DOCmgL"), aes(y = log(values), x = date, color = var)) +
  geom_line(alpha = 0.25) +
  facet_wrap(vars(site, catchment), scales = "free_y") +
  ggtitle("DOCmgL")


ConWTA.ts.NH4 <- ggplot(df2 %>% 
         filter(solute == "NH4NmgL"), aes(y = values, x = date, color = var)) +
  geom_line(alpha = 0.25) +
  facet_wrap(vars(site, catchment), scales = "free_y")+
  ggtitle("NH4NmgL")

# are the missing NH4 values real NA's: Yes 864 NAs of 936
EB.NH4 <- df2 %>% 
  filter(solute == "NH4NmgL") %>% 
  filter(catchment == "EB")

ConWTA.ts.NO3 <- ggplot(df2 %>% 
         filter(solute == "NO3NmgL"), aes(y = log(values), x = date, color = var)) +
  geom_line(alpha = 0.25) +
  facet_wrap(vars(site, catchment), scales = "free_y")+
  ggtitle("NO3NmgL")

ConWTA.ts.SO4 <- ggplot(df2 %>% 
                          filter(solute == "SO4SmgL"), aes(y = log(values), x = date, color = var)) +
  geom_line(alpha = 0.25) +
  facet_wrap(vars(site, catchment), scales = "free_y")+
  ggtitle("SO4SmgL")

ConWTA.ts.TDP <- ggplot(df2 %>% 
         filter(solute == "TDPmgL"), aes(y = log(values), x = date, color = var)) +
  geom_line(alpha = 0.25) +
  facet_wrap(vars(site, catchment), scales = "free_y")+
  ggtitle("TDPmgL")

pdf(file.path(here::here("plots"), "ConWTATimeSeries.pdf"), height = 8, width = 12)
ConWTA.ts.Ca
ConWTA.ts.DOC
ConWTA.ts.NH4
ConWTA.ts.NO3
ConWTA.ts.SO4
ConWTA.ts.TDP
dev.off()
