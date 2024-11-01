# This script combines all Q and chem data for all unmanaged HJA sites
# JMH; 5 May 2021, 18 May 2021, 16 Sept 21, 27 sept 21

## required packages ----
library(here)
library(readr)
library(tidyverse)
library(readxl)
library(GGally)
library(openxlsx)

# Common time period ----
CTstart <- as.POSIXct(paste0("1985-10-31", format = "%Y-%m-%d"))
CTend <- as.POSIXct(paste0("2010-10-31", format = "%Y-%m-%d"))
# 9130 days

# DISCHARGE ----
# metadata: http://andlter.forestry.oregonstate.edu/data/attributes.aspx?dbcode=HF004&entnum=2
# Q in cfs

# daily
HJA_Q <-  read_csv(file.path(here::here("01_data"), 
                                    "HJA discharge daily.csv")) %>% 
            select(STCODE:MEAN_Q) %>% 
            # both 8 and 9 are undisturbed controls
            filter(SITECODE %in% c("GSWS08", "GSWS09")) %>% 
            mutate(Q_Ls = MEAN_Q * 28.31) %>% 
            select(Date = DATE, WS = SITECODE, Q_Ls)

ggplot(HJA_Q, aes(y = Q_Ls, x = Date)) +
  geom_point()+
  facet_wrap(vars(WS), scales = "free_y")


# CHEMISTRY ----
# metadata: http://andlter.forestry.oregonstate.edu/data/attributesdetail.aspx?dbcode=CF002&entnum=1
# CA mg/L; DOC: mg/L; NH4: mg/L; NO3: mg/L; TDP: mg/L; SO4: mg/L <- these data are flow-weighted means 
# new dataset has TDP as mg/L not ug/L - 27 Sept. 2021
# samples are collected roughly every 20 days (with little variation)
# DOC data starts much later, SO4 is pretty sparse before late 80's


HJA_chem <-  read_csv(file.path(here::here("01_data"), 
                            "HJA stream chemistry daily.csv")) %>% 
          select(WS = SITECODE, DateTime = DATE_TIME, Ca_mgL = CA, DOC_mgL = DOC, NH4_mgL = NH3N, NO3_mgL = NO3N, SRP_mgL = TDP, SO4_mgL = SO4S) %>% 
          filter(WS %in% c("GSWS08", "GSWS09")) %>% 
          mutate(Date = as.Date(DateTime, format = "%Y-%m-%d")) %>% 
          select(WS, Date, Ca_mgL, DOC_mgL, NH4_mgL, NO3_mgL, SRP_mgL, SO4_mgL)



ggplot(HJA_chem %>% 
         pivot_longer(Ca_mgL:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = log(conc+1), x = Date, color = WS)) +
  geom_point()+
  facet_wrap(vars(solute), scales = "free_y")


# COMBINE ----

HJA.f <- HJA_Q %>% 
          full_join(HJA_chem, by = c("Date", "WS")) %>% 
          mutate(WS = as.factor(WS),
                 Site = "HJA") %>% 
          select(Site, WS, Date, Q_Ls:SO4_mgL) 


ggplot(HJA.f %>% 
         pivot_longer(Q_Ls:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")

# ggpairs(HJA.f[,3:9])

# EXPORT DATAFRAME  ----
write_csv(HJA.f, file.path(here::here("03_generatedData"), "01_HJAcomb.csv"))


