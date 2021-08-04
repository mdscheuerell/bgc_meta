# This script combines all Q and chem data for all unmanaged MEF sites
# JMH; 5 May 2021, 18 May 2021, 2 June 2021, 14 Jul 21 (dropping SRP)




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
# Going to take the L/s column
MEFnames <- c("Date", "Q_Ls")

MEF_Qs2 <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/MEF"), 
                                     "MEF raw data compiled.xlsx"),
                           sheet = "run-S2", 
                           range = "A2:B20455",
                           col_names = MEFnames) %>% 
            mutate(WS = "S2")

MEF_Qs5 <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/MEF"), 
                                       "MEF raw data compiled.xlsx"),
                             sheet = "run-S5", 
                             range = "A2:B20455",
                             col_names = MEFnames) %>% 
            mutate(WS = "S5")
  
  
MEF_Q <- rbind(MEF_Qs2, MEF_Qs5)

# compare data used to data paper
# downloaded from: https://www.fs.usda.gov/rds/archive/Catalog/RDS-2018-0009
MEF_Qdd <- read.csv(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/MEF"), 
                              "Daily_streamflowDOWNLOAD.csv")) 

names(MEF_Qdd) <- c("Date", "WS", "FlowLsDD", "FlowCFS", "FlowCmDay")

Qtest <- MEF_Q %>% 
  mutate(Date = as.character(Date)) %>% 
  left_join(MEF_Qdd, by = c("Date", "WS"))%>% 
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"))


ggplot(Qtest, aes(y = FlowLsDD, x = Q_Ls)) +
  geom_point()


ggplot(MEF_Q, aes(y = log(Q_Ls +1), x = Date)) +
  geom_point()+
  facet_wrap(vars(WS), scales = "free_y")


# CHEMISTRY
# !!!!!!!!!!!!!!!!!!NO3 SAYS uM IN D1 AND MG/L IN D3!!!!!!!!!!!!!!!!!! <- DAvid indicated that NO3 and NH4 can't be trusted
# THERE ARE 2 KINDS OF TOC HERE, NTO SURE WHICH ONE TO USE- SEEM TO ALTERNATE <- data provider indicates that these are exchangeable
# TP: David indicates this is TDP
# dropping TDP: they do not filter or preserve their samples

MEF_chemNames <- c("Site", "Date", "TN_mgL", "NO3_mgL", "NH4_mgL", "TP_mgL", "Ca_mgL", "SO4_mgL", "TOC_NPOC_mgL", "TOC_TCIC_mgL")

MEF_chem <-  readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/MEF"), 
                                         "MEF raw data.xlsx"),
                               range = "A5:J1879",
                               col_names = MEF_chemNames) %>% 
              select(Site, Date , NO3_mgL:SO4_mgL, TOC_NPOC_mgL,  TOC_TCIC_mgL)

# chem columns have "<" in there. In NO3 column these are not always the same less than first < 0.01 then < 0.02
# going to replace "< x" with "x"
MEF_chem2 <- MEF_chem %>%  
          # deleted NO3 and NH4 data not trustworthy
          # deleted TP, also not trustworthy
          mutate(NO3_mgL = as.numeric("NA"),
                 NH4_mgL = as.numeric("NA"),
                 TP_mgL = as.numeric("NA"),
                 Ca_mgL = as.numeric(str_remove(Ca_mgL, "<.")),
                 SO4_mgL = as.numeric(str_remove(SO4_mgL, "<.")),
                 Site = as.factor(Site)) %>% 
          filter(Site %in% c("S2", "S5")) %>% 
          rowwise() %>% 
          # data provider has evidence that these are the same
          mutate(DOC_mgL = mean(c(TOC_NPOC_mgL, TOC_TCIC_mgL), na.rm = TRUE)) %>% 
          filter(Date >= CTstart & Date <= CTend) %>% 
          droplevels() %>% 
          rename("WS" = "Site") %>% 
          # just reorganizing
          select(WS, Date, Ca_mgL, DOC_mgL, NH4_mgL, NO3_mgL, SRP_mgL = TP_mgL, SO4_mgL)



ggplot(MEF_chem2 %>% 
         pivot_longer(Ca_mgL:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")

##########
# COMBINE INTO DOR DF
#########

# Think times in here are messing up the joins - sometimes

MEF.f <- MEF_Q %>% 
          full_join(MEF_chem2, by = c("Date", "WS")) %>% 
          filter(Date >= CTstart & Date <= CTend) %>% 
          mutate(WS = as.factor(WS),
                 Site = "MEF") %>% 
          select(Site, WS, Date, Q_Ls, Ca_mgL:SO4_mgL)


ggplot(MEF.f %>% 
         pivot_longer(c(Q_Ls:SO4_mgL), names_to = "solute", values_to = "conc"), aes(y = conc, x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")



##########
# EXPORT DATAFRAME
#########

write_csv(MEF.f, file.path(here::here("data/JMHnewMungedDat/"), "01_MEFcomb.csv"))


