# This script combines all Q and chem data for all unmanaged HBEF sites
# JMH; 12 May 2021, updated 18 May 2021




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
# HBEF
############
# NOTE: WE have monthly discharge and solute data compiled by John Campbell. 
# I thought it would be better to compile everything the same way (after a lot of thought).


# DISCHARGE

# daily
HBEF_Q <-  read_csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/HBEF"), 
                                    "HBEF_DailyStreamflow_1956-2020.csv")) %>% 
            mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"),
                   WS = as.factor(paste0("WS",WS))) %>% 
            # both 8 and 9 are undisturbed controls - checked since names were different
            filter(WS %in% c("WS6", "WS7", "WS8", "WS9")) %>% 
            droplevels() %>% 
            # add watershed area in ha. Values from "hbef_discharge_1963_2020_compiled" from Irena
            mutate(WA_m2 = (ifelse(WS == "WS6", 13.2,
                                           ifelse(WS == "WS7", 77.38,
                                                  ifelse(WS == "WS8", 59.4,
                                                         ifelse(WS == "WS9", 68.4, as.numeric('NA'))))) * 10000)) %>% # converting from ha to m2
            # 1 mm = 1 L/m2
            mutate(Q_Ls = Streamflow * WA_m2 *(1/86400))

# flags apply after our window.
ggplot(HBEF_Q, aes(y = log(Q_Ls +1), x = DATE, color = Flag)) +
  geom_point()+
  facet_wrap(vars(WS), scales = "free_y")


# CHEMISTRY
# all values are in mg/L - not sure if it is N or NH4 - metadata say NH4 and NO3 concentration, not NH4-N...
# samples before 2013 were not filtered or frozen prior to analysis
# couldn't get DOC and PO4 to be numbers with read_csv
HBEF_chem <-  read.csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/HBEF"), 
                            "HubbardBrook_weekly_stream_chemistry_1963-2019.csv"), na = c("", "NA")) %>% 
          select(site, Date = date, Ca_mgL = Ca, DOC_mgL = DOC, NH4_mgL = NH4, NO3_mgL = NO3, SRP_mgL = PO4, SO4_mgL = SO4) %>% 
          mutate(NH4_mgL = NH4_mgL *  (14/18.039),
                 NO3_mgL = NO3_mgL * (14/62.0049),
                 SRP_mgL = SRP_mgL * (31/94.97),
                 SO4_mgL = SO4_mgL * (32.065/90.06)) %>% 
          filter(site %in% c("W6", "W7", "W8", "W9")) %>% 
          mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
          mutate(WS = ifelse(site == "W6", "WS6",
                        ifelse(site == "W7", "WS7",
                          ifelse(site == "W8", "WS8",
                            ifelse(site == "W9", "WS9","blah")))),
                 WS = as.factor(WS)) %>% 
          select(Date, WS, Ca_mgL:SO4_mgL)



ggplot(HBEF_chem %>% 
         pivot_longer(Ca_mgL:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")

##########
# COMBINE 
#########

HBEF <- HBEF_Q %>% 
  rename(Date = "DATE") %>% 
  full_join(HBEF_chem, by = c("Date", "WS")) %>% 
  # loose the DOC data here
  filter(Date >= CTstart & Date <= CTend) %>%
  mutate(Site = "HBEF") %>% 
  select(Site, WS, Date, Q_Ls:SO4_mgL)


ggplot(HBEF %>% 
         pivot_longer(Q_Ls:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")



##########
# EXPORT DATAFRAME
#########

write_csv(HBEF, file.path(here::here("data/JMHnewMungedDat/"), "01_HBEFcomb.csv"))


