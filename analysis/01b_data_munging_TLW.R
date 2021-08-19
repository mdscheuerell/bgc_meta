# This script calculates flow weighted means for all unmanaged sites at TLW
# JMH started 18 May 21




##-------------------
## required packages
##-------------------
library(here)
library(readr)
library(tidyverse)
library(readxl)
library(GGally)

##-------------------
## Load data
##-------------------

# Common time period
CTstart <- as.POSIXct(paste0("1985-10-31", format = "%Y-%m-%d"))
CTend <- as.POSIXct(paste0("2010-10-31", format = "%Y-%m-%d"))
# 9130 days

############
# TLW
############

# units on Q are m3/s
# units for solutes variable in ELA stream chemistry.xls::Units
# no info on qualifiers

############
# GET CONC DATA
############

# TLW - had to change file type from xlsb to xlsx
# just keeping the unmanaged watersheds: 32, 35, and 38
# Q in mm


TLW_Q <-  readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/TLW"), 
                                          "TLW all data - new compiled Oct 2020.xlsx"), sheet = "Q", range = "A1:P11689") %>% 
                # Q in mm is in col E:I, but is derived from Q in L/s in col K:P (no calc there), so using L/s which is what I want.
                select(Date, "C32" = "32 L/s", "C35" = "35 L/s", "C38" = "38 L/s") %>% 
                mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
                pivot_longer(cols = c(C32:C38), names_to = "WS", values_to = "Q_Ls") %>% 
                mutate(WS = as.factor(WS)) %>% 
                # Q_Ls is negative - 7 cases: forcing to zero
                mutate(Q_Ls = ifelse(Q_Ls < 0, 0, Q_Ls))
  
  

ggplot(TLW_Q, aes(y = Q_Ls, Date)) +
  geom_point() +
  facet_wrap(vars(WS), scales = "free_y")


############
# SOLUTE DATA
############
# david says TP = TDP

# C32 chem
# got this from David today after writing rest of script.
# The read me indicates taht these are mg/L/day, but they are really mg/L
C32chem <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/TLW"), 
                                       "TLW data - daily chemistry updated20200518.xlsx"), sheet = 2, range = "V1:AO1256") %>% 
              select(Date,
                     "Ca_mgL" = "c32 Ca",
                     "DOC_mgL" = "c32 DOC",
                     "NH4_mgL" = "c32 NH4-N",
                     "NO3_mgL" = "c32 NO3-N",
                     "SRP_mgL" = "c32 TP",
                     "SO4_mgL" = "c32 SO4-S") %>% 
              mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
                       WS = "C32")%>% 
              # filter(Date >= CTstart & Date <= CTend) %>% 
              droplevels()

#35
C35chem <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/TLW"), 
                                       "TLW data - daily chemistry updated20200518.xlsx"), sheet = 2, range = "CG1:CZ1391") %>% 
              select(Date,
                     "Ca_mgL" = "c35 Ca",
                     "DOC_mgL" = "c35 DOC",
                     "NH4_mgL" = "c35 NH4-N",
                     "NO3_mgL" = "c35 NO3-N",
                     "SRP_mgL" = "c35 TP",
                     "SO4_mgL" = "c35 SO4-S") %>% 
              mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
                     WS = "C35")%>% 
              # filter(Date >= CTstart & Date <= CTend) %>% 
              droplevels()

#38
C38chem <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/TLW"), 
                                       "TLW data - daily chemistry updated20200518.xlsx"), sheet = 2, range = "DB1:DU1148") %>% 
              select(Date,
                     "Ca_mgL" = "c38 Ca",
                     "DOC_mgL" = "c38 DOC",
                     "NH4_mgL" = "c38 NH4-N",
                     "NO3_mgL" = "c38 NO3-N",
                     "SRP_mgL" = "c38 TP",
                     "SO4_mgL" = "c38 SO4-S") %>% 
              mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
                     WS = "C38")%>% 
              # filter(Date >= CTstart & Date <= CTend) %>% 
              droplevels()





TLW_chem <- rbind(C32chem, C35chem, C38chem)  %>% 
            mutate(SRP_mgL = as.numeric(SRP_mgL)) %>% 
            select(WS, Date, Ca_mgL:SO4_mgL)





##########
# EXPORT DATAFRAME
#########

TLW <- TLW_Q %>% 
  full_join(TLW_chem, by = c("Date", "WS")) %>% 
  # filter(Date >= CTstart & Date <= CTend) %>% 
  mutate(Site = "TLW") %>% 
  select(Site, WS, Date, Q_Ls:SO4_mgL)


ggplot(TLW %>% 
         pivot_longer(cols = c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc"), aes(y = conc, x = Date)) +
  geom_point() +
  facet_grid(solute ~ WS, scales = "free_y")


write_csv(TLW, file.path(here::here("data/JMHnewMungedDat/"), "01_TLWcomb.csv"))











                            