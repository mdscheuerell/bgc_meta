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

ggsave(path = "plots", filename = "Ca.an.pdf", width = 11, height = 8)

# DOC
DOC.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrDOCmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

#units for BBWM and LEF are incorrect...values too low

ggsave(path = "plots", filename = "DOC.an.pdf", width = 11, height = 8)

# NH4
NH4.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrNH4NmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Should apply a common LOQ filter
# SLP errant value in 2001?
# LEF: elevated values 1980s...? TKH can check with McDowell

ggsave(path = "plots", filename = "NH4.an.pdf", width = 11, height = 8)

# NO3
NO3.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrNO3NmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Should apply a common LOQ filter

ggsave(path = "plots", filename = "NO3.an.pdf", width = 11, height = 8)

# TDP
TDP.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrTDPmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Separate analyses for sites that report TDP and PO4/SRP
# Need to apply an LOQ filter
# early record CWT & HBEF high values

ggsave(path = "plots", filename = "TDP.an.pdf", width = 11, height = 8)

# SO4
SO4.an <- dat_ann %>% ggplot(aes(x = WaterYear, y = StrSO4SmgL, color = catchment, shape = type)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol = 3, scales = "free_y")

# Check units for DOR
# Apply a common LOQ filter

ggsave(path = "plots", filename = "SO4.an.pdf", width = 11, height = 8)

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
