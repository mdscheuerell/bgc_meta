# This script combines all Q and chem data for all unmanaged SLP sites
# JMH; 18 May 21




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
# SLP
############

# DISCHARGE
# Q in cfs

# All data is here combined, using the "DA" version, which seems to have been "corrected" by data provider?

SLPall <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/SLP"), 
                                       "HB_qwdata_sleepersW9_1991-2019_WeeklyPPT_InstQ_DA.xlsx"),
                             sheet = "stream", 
                             range = "A1:AT1476") 

SLPall2 <- SLPall %>% 
            mutate(WS = "S5") %>% 
            select("Date" = "Collection date",
                   # not sure what difference is between Discharge and corrected discharge
                   "Q_cfs" = "Corrected Discharge,cfs",
                   # 156 of these are listed as below
                   "Ca_mgL" = "Ca, water, filtered, mg/L", "Ca_code" = "...14",
                   "DOC_mgL" = "DOC, water, filtered, mg/L", "DOC_code" = "...8",
                   "NH4_mgL" = "NH4, water, filtered, mg N /L", "NH4_code" = "...34",
                   # note: as NO3, not NO3-N
                   "NO3_mgL" = "NO3, water, filtered, mg/L as NO3", "NO3_code" = "...38",
                   # not sure if this is digested or not
                   # not 100% sure about units on this.
                   "SRP_mgL" = "P, water, filtered,  mg/L", "P_code" = "...26", 
                   # no code for this
                   "SO4_mgL" = "SO4, water, filtered, mg/L") %>% 
            mutate_at(vars(contains("code")), funs(factor(.))) %>% 
            mutate_at(vars(Q_cfs, Ca_mgL, DOC_mgL, NH4_mgL, NO3_mgL, SRP_mgL, SO4_mgL), funs(as.numeric(.))) %>% 
            # units
            mutate(NO3_mgL = NO3_mgL * 14/62,
                   SO4_mgL = SO4_mgL* 32.07/96.06,
                   Q_Ls = Q_cfs * 28.32, 
                   Site = "SLP",
                   WS = "W9") %>% 
            select(Site, WS, Date, Q_Ls, Ca_mgL, DOC_mgL, NH4_mgL, NO3_mgL, SRP_mgL, SO4_mgL)
  


ggplot(SLPall2 %>% 
         pivot_longer(cols = c("Q_Ls":"SO4_mgL"), names_to = "solute", values_to = "conc"), aes(y = conc, x = Date)) +
            geom_point() +
            facet_wrap(vars(solute), scale = "free_y")

# - NEED TO FIGURE OUT COLUMNS AND WHAT THE COLUMNS NEXT TO THE DATA MEAN



##########
# EXPORT DATAFRAME
#########

write_csv(SLPall2, file.path(here::here("data/JMHnewMungedDat/"), "01_SLPcomb.csv"))


