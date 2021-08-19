# This script combines all Q and chem data for all unmanaged SEF sites
# JMH; 5 May 2021, updated 18 May 21




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
# MEF
############

# DISCHARGE
# Q in cfs

# Q data is here
# file name suggests it is L/s

# lot's of NULL in the Q column indicates missing data
SEF_1980_1999 <- read_csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/SEF"), 
                             "SEF_WS80_Q_ls_1980-1999.csv")) %>% 
        select(WS = Location, Date = "Date_", "Q_Ls" = "Dailyflow_") %>% 
        mutate(Q_Ls = as.numeric(Q_Ls)) %>%  # this removes the nulls
        mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y"))

ggplot(SEF_1980_1999, aes(y = Q_Ls, x = Date)) +
  geom_point()

# UNITS HERE UNCLEAR - avg flow is about 1/100 of daily flow, not sure what the difference is or the units!!!!!!!!!!
# Daily flow is very different from what was in the previous file 1/100 times smaller
# going to focus on average
SEF_2003_2017 <- read_csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/SEF"), 
                             "SEF_WS80_Q_ls_2003-2017.csv")) %>% 
  select(WS = Location, Date = "Date_", "Q_Ls" = "Avg_FlowRa") %>% 
  mutate(Q_Ls = as.numeric(Q_Ls)) %>%  # this removes the nulls
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y"))

ggplot(SEF_2003_2017, aes(y = Q_Ls, x = Date)) +
  geom_point()

SEF_Q <- rbind(SEF_1980_1999, SEF_2003_2017)

ggplot(SEF_Q, aes(y = Q_Ls, x = Date)) +
  geom_point()


# CHEMISTRY
# no DOC
# only Po4 here, no TDP
SEFchem_1976_1994 <- read_csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/SEF"), 
                                    "SEF_WS80_WQ_mgl_1976-1994.csv")) %>% 
  mutate_at(vars(pH:Temp_C), as.numeric) %>%  # this removes the nulls
  rename(Date = "Date_") %>% 
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>% 
  select(Location, Date, NO3_NO2_N_mgL, NH4_N_mgL, PO4_P_mgL, Ca_mgL, SO4_S_mgL) %>% 
  mutate(DOC_mgL = as.numeric("NA")) 

# here there's TP, "P", and PO4
# This has DOC
SEFchem_2004_2017 <- read_csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/SEF"), 
                                        "SEF_WS80_WQ_mgl_2004-2017.csv")) %>% 
  mutate_at(vars(TN_mgL:DO_per_sat), as.numeric) %>%  # this removes the nulls
  # rename(Date = "Date_") %>% 
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>% 
  select(Location, Date, NO3_NO2_N_mgL, NH4_N_mgL, PO4_P_mgL = "PO4_mgL", Ca_mgL, SO4_S_mgL =  "SO4_mgL", DOC_mgL) 

SEFchem <- rbind(SEFchem_1976_1994, SEFchem_2004_2017) %>% 
          select("Date",
                 "Ca_mgL",
                 "DOC_mgL",
                 "NH4_mgL" = "NH4_N_mgL",
                 "NO3_mgL" = "NO3_NO2_N_mgL",
                 "SRP_mgL" = "PO4_P_mgL",
                 "SO4_mgL" = "SO4_S_mgL")

SEF_final <- SEF_Q %>% 
  full_join(SEFchem, by = "Date") %>% 
  filter(Date >= CTstart & Date <= CTend) %>% 
  mutate(Site = "SEF") %>% 
  select(Site, WS, Date, Q_Ls:SO4_mgL)

ggplot(SEF_final %>% 
         pivot_longer(c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc"),
       aes(y = log(conc + 1), x = Date)) +
  geom_point() +
  facet_wrap(vars(solute))


##########
# EXPORT DATAFRAME
#########

write_csv(SEF_final, file.path(here::here("data/JMHnewMungedDat/"), "01_SEFcomb.csv"))


