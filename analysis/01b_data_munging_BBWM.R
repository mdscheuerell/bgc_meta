# This script combines all Q and chem data for the one BBWM unmanaged site
# JMH; 17 May 2021, updated 18 May 21




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
# BBWM
############
# NOTE: WE have monthly discharge and solute data compiled by John Campbell. 
# I thought it would be better to compile everything the same way (after a lot of thought).


# DISCHARGE

# daily
# cubic feet per second - east bear brook, Beddington, MAine
BBWM_Q.1 <-  read.table(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/BBWM"), 
                                    "daily_dischargeBBWMEAST.txt"), skip = 29, header = F) 

names(BBWM_Q.1) <- c("Agency", "Site_no", "Datetime", "Q_cfs", "Flag")

BBWM_Q <-  BBWM_Q.1 %>% 
            mutate(Date = as.Date(Datetime, format = "%Y-%m-%d"),
                   Site = "BBWM",
                   WS = "EB", 
                   Q_Ls = Q_cfs*28.32) %>% 
            select(Date, Site, WS, Q_Ls, Flag)

# flags apply after our window.
#A= approved, e = estimated - some values ahve been estimated.
ggplot(BBWM_Q, aes(y = log(Q_Ls +1), x = Date, color = Flag)) +
  geom_point()


# CHEMISTRY
# samples before 2013 were not filtered or frozen prior to analysis
# couldn't get DOC and PO4 to be numbers with read_csv
BBWM_chem <-  read.csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/BBWM"), 
                            "bbwm_stream_chemistry_1986_2015.csv")) %>% 
                mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>% 
                select(-Year, -Month, -Day, -Hour, -Mg_ueq_L) %>% 
                mutate(Watershed = as.factor(Watershed),
                       Record_Type = as.factor(Record_Type)) %>% 
                filter(Watershed == "EB") %>% 
                mutate(Ca_mgL = Ca_ueq_L*(1/2)*40.078*0.001, #mg Ca/L
                       NO3_mgL = NO3_ueq_L*(1/1)*62.0049*0.001*(14/62.0049), #NO3-N mg N/L
                       SO4_mgL = SO4_ueq_L*(1/2)*90.06*0.001*(32.065/90.06), #SO4-S mg S/L
                       NH4_mgL = NH4_ueq_L*(1/1)*18.039*0.001*(14/18.039),
                       SRP_mgL = as.numeric("NA")) %>% # no data
                select(Date, Watershed, Ca_mgL, DOC_mgL = DOC_mg_L, NH4_mgL, NO3_mgL, SRP_mgL, SO4_mgL)




          
ggplot(BBWM_chem %>% 
         pivot_longer(Ca_mgL:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = Watershed)) +
  geom_point()+
  facet_grid(solute ~ Watershed, scales = "free_y")

##########
# COMBINE 
#########

# weird. ~2 years with chem data but no Q
# USGS only  has data since 1995, so what they downloaded is more extensive.
BBWM <- BBWM_Q %>% 
  full_join(BBWM_chem, by = c("Date")) %>% 
  #74 rows with missing Site and WS info due to join - Q on date without chem
  mutate(Site = ifelse(is.na(Site), "BBWM", Site),
         WS = ifelse(is.na(WS), "EB", WS)) %>% 
  mutate(Site = as.factor(Site),
         WS = as.factor(WS)) %>%
    # loose the DOC data here
  filter(Date >= CTstart & Date <= CTend) %>% 
  select(-Watershed, -Flag) %>% 
  select(Site, WS, Date, Q_Ls:SO4_mgL)



ggplot(BBWM %>% 
         pivot_longer(Q_Ls:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")



##########
# EXPORT DATAFRAME
#########

write_csv(BBWM, file.path(here::here("data/JMHnewMungedDat/"), "01_BBWMcomb.csv"))


