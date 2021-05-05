# This script combines all Q and chem data for all unmanaged HJA sites
# JMH; 5 May 2021




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
            filter(SITECODE %in% c("GSWS08", "GSWS09"))

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
          select(SITECODE, DATE_TIME, CA, DOC, NH3N, NO3N, TDP, SO4S) %>% 
          filter(SITECODE %in% c("GSWS08", "GSWS09")) %>% 
          mutate(DATE = as.character(strftime(DATE_TIME, format = "%Y-%m-%d")))


# STOPPED ABOBVE THIS


ggplot(HJA_chem %>% 
         pivot_longer(CA:SO4S, names_to = "solute", values_to = "conc"), aes(y = log(conc+1), x = DATE_TIME, color = SITECODE)) +
  geom_point()+
  facet_wrap(vars(solute), scales = "free_y")

##########
# COMBINE INTO DOR DF
#########

# Think times in here are messing up the joins - sometimes
HJA_Q.j <-  HJA_Q %>% 
          mutate(DATE = as.character(DATE))

HJA.f <- HJA_Q.j %>% 
          full_join(HJA_chem, by = c("DATE", "SITECODE")) %>% 
          mutate(Date = as.POSIXct(DATE, format = "%Y-%m-%d"),
                 WS = as.factor(SITECODE)) %>% 
          select(SITECODE, Date, MEAN_Q, CA:SO4S) %>% 
          filter(Date >= CTstart & Date <= CTend)


ggplot(HJA.f %>% 
         pivot_longer(MEAN_Q:SO4S, names_to = "solute", values_to = "conc"), aes(y = log(conc+1), x = Date, color = SITECODE)) +
  geom_point()+
  facet_wrap(vars(solute), scales = "free_y")

ggpairs(HJA.f[,3:9])

##########
# EXPORT DATAFRAME
#########

write_csv(HJA.f, file.path(here::here("data/JMHnewMungedDat/"), "01_HJAcomb.csv"))


