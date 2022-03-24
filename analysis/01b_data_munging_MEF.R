############
# MEF
############

# This script combines all Q and chem data for all unmanaged MEF sites
# JMH; 5 May 2021, 18 May 2021, 2 June 2021, 14 Jul 21 (dropping SRP), 23 Mar 22 (new MEF S2 data)


# required packages ----

library(here)
library(readr)
library(tidyverse)
library(readxl)
# library(GGally)
# library(openxlsx)

# Load data ----

## DISCHARGE ----
 # downloaded from: https://www.fs.usda.gov/rds/archive/Catalog/RDS-2018-0009
MEF_Q <- read.csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/MEF"), 
                              "Daily_streamflowDOWNLOAD.csv")) %>% 
            select(Date, WS = Watershed, Q_Ls = Flow..L.s.)%>% 
            mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d")) %>% 
            filter(WS %in% c("S2", "S5"))

## CHEMISTRY ----
# DAvid indicated that NO3 and NH4 can't be trusted
# TS says to use TC.IC before mid-2010 and NPOC after,
# TP: David indicates this is TDP
# dropping TDP: they do not filter or preserve their samples


### S2 ----
# New MEF data from SS 3/22 
MEFin <- read.csv(here("data", "NewDataFromIrena20210130", "New MAR Data", "Raw Data Files", "MEF", "2022.03.04_S2LaggPool_ToHarmsT.csv")) %>%                 select(Site, NAME, Date = DATE.TIME, 
                        SO4_mgL = SO4, Ca_mgL = CA, NPOC, TC.IC)


# TC.IC is higher than NPOC
ggplot(MEFin, aes(y = TC.IC, x = NPOC)) +geom_point() + geom_abline(intercept = 0, slope = 1)
summary(lm(TC.IC~NPOC, MEFin))

# munge df
  MEFS2 <- MEFin %>% 
                mutate(Date = as.POSIXct(Date, format = "%m/%d/%y %H:%M")) %>% 
                mutate_at(vars(Site,NAME), factor) %>% 
                # ASSUME I WANT THE LAGG POOL - NEED TO CHECK META DATA
                filter(NAME == "S2 lagg pool") %>% 
                # SS says to use TC.IC before mid-2010 and NPOC after,
                # wasn't sure what mid 2010 was, so started in June 2010
                mutate(DOC_mgL = ifelse(Date < as.POSIXct("2010-06-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"),
                                         TC.IC,
                                         NPOC),
                       NO3_mgL = as.numeric("NA"), #no data
                       NH4_mgL = as.numeric("NA"), #no data
                       SRP_mgL = as.numeric("NA")) %>% #no data
                select(WS = Site, Date, Ca_mgL, DOC_mgL, NH4_mgL, NO3_mgL, SRP_mgL, SO4_mgL)


### S5 ----
MEF_chemNames <- c("Site", "Date", "TN_mgL", "NO3_mgL", "NH4_mgL", "TP_mgL", "Ca_mgL", "SO4_mgL", "TOC_NPOC_mgL", "TOC_TCIC_mgL")

MEF_chem <-  readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/MEF"), 
                                         "MEF raw data.xlsx"),
                               range = "A5:J1879",
                               col_names = MEF_chemNames) %>% 
              select(Site, Date , NO3_mgL:SO4_mgL, TOC_NPOC_mgL,  TOC_TCIC_mgL)

# chem columns have "<" in there. 
# In NO3 column these are not always the same less than first < 0.01 then < 0.02
MEFS5 <- MEF_chem %>%  
          # deleted NO3 and NH4 data 
          # deleted TP
          mutate(NO3_mgL = as.numeric("NA"),
                 NH4_mgL = as.numeric("NA"),
                 TP_mgL = as.numeric("NA"),
                 Ca_mgL = as.numeric(str_remove(Ca_mgL, "<.")),
                 SO4_mgL = as.numeric(str_remove(SO4_mgL, "<.")),
                 Site = as.factor(Site)) %>% 
          # only S5
          filter(Site %in% c("S5")) %>% 
          mutate(DOC_mgL = ifelse(Date < as.POSIXct("2010-06-01 00:00:00", format = "%Y-%m-%d %H:%M:%S"),
                                  TOC_TCIC_mgL,
                                  TOC_NPOC_mgL)) %>% 
          droplevels() %>% 
          rename("WS" = "Site") %>% 
          # just reorganizing
          select(WS, Date, Ca_mgL, DOC_mgL, NH4_mgL, NO3_mgL, SRP_mgL = TP_mgL, SO4_mgL)


# combine two watersheds ----
MEF_S2S5 <- rbind(MEFS2, MEFS5)

ggplot(MEF_S2S5 %>% 
         pivot_longer(Ca_mgL:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")

# Combine chem and Q ----
MEF.f <- MEF_Q %>% 
          full_join(MEF_S2S5, by = c("Date", "WS")) %>% 
          mutate(WS = as.factor(WS),
                 Site = "MEF") %>% 
          select(Site, WS, Date, Q_Ls, Ca_mgL:SO4_mgL)


ggplot(MEF.f %>% 
         pivot_longer(c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")

# export df & save ----
write_csv(MEF.f, file.path(here::here("data/JMHnewMungedDat/"), "01_MEFcomb.csv"))


