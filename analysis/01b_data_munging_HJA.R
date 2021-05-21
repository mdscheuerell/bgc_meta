# This script combines all Q and chem data for all unmanaged HJA sites
# JMH; 5 May 2021, 18 May 2021




##-------------------
## required packages
##-------------------
library(here)
library(readr)
library(tidyverse)
library(readxl)
library(GGally)
library(openxlsx)

##-------------------
## Load data
##-------------------

# Common time period
CTstart <- as.POSIXct(paste0("1985-10-31", format = "%Y-%m-%d"))
CTend <- as.POSIXct(paste0("2010-10-31", format = "%Y-%m-%d"))
# 9130 days

############
# HJA
############

# DISCHARGE
# metadata: http://andlter.forestry.oregonstate.edu/data/attributes.aspx?dbcode=HF004&entnum=2
# Q in cfs

# daily
HJA_Q <-  read_csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/HJA"), 
                                    "HJA discharge daily.csv")) %>% 
            select(STCODE:MEAN_Q) %>% 
            # both 8 and 9 are undisturbed controls - checked since names were different
            filter(SITECODE %in% c("GSWS08", "GSWS09")) %>% 
            mutate(Q_Ls = MEAN_Q * 28.31) %>% 
            select(Date = DATE, WS = SITECODE, Q_Ls)

ggplot(HJA_Q, aes(y = log(MEAN_Q +1), x = DATE)) +
  geom_point()+
  facet_wrap(vars(SITECODE), scales = "free_y")


# CHEMISTRY
# metadata: http://andlter.forestry.oregonstate.edu/data/attributesdetail.aspx?dbcode=CF002&entnum=1
# CA mg/L; DOC: mg/L; NH4: mg/L; NO3: mg/L; TDP: ug/L; SO4: mg/L
# samples are collected roughly every 20 days (with little variation)
# DOC data starts much later, SO4 is pretty sparse before late 80's
# I"m not sure how MEAN_LPS was calculated

HJA_chem <-  read_csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/HJA"), 
                            "HJA stream chemistry daily.csv")) %>% 
          select(WS = SITECODE, DateTime = DATE_TIME, Ca_mgL = CA, DOC_mgL = DOC, NH4_mgL = NH3N, NO3_mgL = NO3N, SRP_ugL = TDP, SO4_mgL = SO4S) %>% 
          mutate(SRP_mgL = SRP_ugL * (31/94.97) /1000) %>% 
          filter(WS %in% c("GSWS08", "GSWS09")) %>% 
          mutate(Date = as.Date(DateTime, format = "%Y-%m-%d")) %>% 
          select(WS, Date, Ca_mgL, DOC_mgL, NH4_mgL, NO3_mgL, SRP_mgL, SO4_mgL)


# STOPPED ABOBVE THIS


ggplot(HJA_chem %>% 
         pivot_longer(Ca_mgL:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = log(conc+1), x = Date, color = WS)) +
  geom_point()+
  facet_wrap(vars(solute), scales = "free_y")

##########
# COMBINE INTO HJA DF
#########

HJA.f <- HJA_Q %>% 
          full_join(HJA_chem, by = c("Date", "WS")) %>% 
          mutate(WS = as.factor(WS),
                 Site = "HJA") %>% 
          select(Site, WS, Date, Q_Ls:SO4_mgL) %>% 
          filter(Date >= CTstart & Date <= CTend)


ggplot(HJA.f %>% 
         pivot_longer(Q_Ls:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")

ggpairs(HJA.f[,3:9])

##########
# EXPORT DATAFRAME
#########

write_csv(HJA.f, file.path(here::here("data/JMHnewMungedDat/"), "01_HJAcomb.csv"))


