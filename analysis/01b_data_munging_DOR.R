# This script combines all Q and chem data for all unmanaged DOR sites
# JMH; 4 May 2021, updated 18 May 21, updated 2 June 21




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
# DOR
############

# DISCHARGE
# no clear header rows
# Q in m3/s; precip mm/day
# only keeping Q and adding in names
DORnames <- c("Y", "M", "D", "Q_HP3", "Q_HP3A", "Q_HP4", "Q_HP5", "Q_HP6", "Q_HP6A")

# data sheet says it goes to 2014, but stops in 2007
DOR_Q <-  readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/DOR"), 
                                    "DOR discharge temperature precipitation 1984-2014.xlsx"),1, 
                          range = "A6:I11114",
                          col_names = DORnames) %>% 
        # Y,M,D columns are all dates with only Y,M,D showing; Y coming out weird but M and D the same using D
        select(D:Q_HP6A) %>% 
        rename(Date = "D") #%>% 
        # filter(Date >= CTstart & Date <= CTend)

ggplot(DOR_Q %>% 
         pivot_longer(Q_HP3:Q_HP6A, names_to = "station", values_to = "Q"), aes(y = Q, x = Date)) +
  geom_point()+
  facet_wrap(vars(station), scales = "free_y")

DOR_Q.l <- DOR_Q %>% 
  pivot_longer(Q_HP3:Q_HP6A, names_to = "ColName", values_to = "Q_m3s") %>% 
  separate(ColName, sep = "_", into = c("ColName", "WS")) %>% 
  mutate(WS = as.factor(WS),
         Q_Ls = Q_m3s * 1000) %>% 
  select(Date, WS, Q_Ls)

# CHEMISTRY
# using TP here instead of TDP because there's no TDP data
# CA mg/L; DOC: mg/L; NH4: ug/L; NO3: ug/L; TP: ug/L; SO4: mg/L
# these spreadsheets consist of data Irena pulled together A1:G12055 - I'll pull this
# Then presumably the data she was given in columns H:Z
# Each watershed is in a sheet
# I had to change from xlsb to xlsx

DOR_chem_names <- c("Date", "Ca_mgL", "DOC_mgL", "NH4_mgNL", "NO3_mgNL", "TP_mgPL", "SO4_mgSL")
DOR_chem_HP3 <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/DOR"), 
                                            "DOR stream chemistry - compiled by Creed.xlsx"),
                                  sheet = "HP3 raw",
                                  range = "A2:G12055",
                                  col_names = DOR_chem_names) %>% 
                  mutate(WS = "HP3")

DOR_chem_HP3A <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/DOR"), 
                                            "DOR stream chemistry - compiled by Creed.xlsx"),
                                  sheet = "HP3A raw",
                                  range = "A2:G12055",
                                  col_names = DOR_chem_names)  %>% 
                  mutate(WS = "HP3A")

DOR_chem_HP4 <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/DOR"), 
                                             "DOR stream chemistry - compiled by Creed.xlsx"),
                                   sheet = "HP4 raw",
                                   range = "A2:G12055",
                                   col_names = DOR_chem_names)  %>% 
                  mutate(WS = "HP4")

DOR_chem_HP5 <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/DOR"), 
                                            "DOR stream chemistry - compiled by Creed.xlsx"),
                                  sheet = "HP5 raw",
                                  range = "A2:G12055",
                                  col_names = DOR_chem_names)  %>% 
                  mutate(WS = "HP5")

DOR_chem_HP6 <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/DOR"), 
                                            "DOR stream chemistry - compiled by Creed.xlsx"),
                                  sheet = "HP6 raw",
                                  range = "A2:G12055",
                                  col_names = DOR_chem_names)  %>% 
                  mutate(WS = "HP6")

DOR_chem_HP6A <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/DOR"), 
                                            "DOR stream chemistry - compiled by Creed.xlsx"),
                                  sheet = "HP6A raw",
                                  range = "A2:G12055",
                                  col_names = DOR_chem_names)  %>% 
                  mutate(WS = "HP6A")

DOR_chem <- rbind(DOR_chem_HP3, DOR_chem_HP3A, DOR_chem_HP4, DOR_chem_HP5, DOR_chem_HP6, DOR_chem_HP6A) %>% 
            select(WS, Date, Ca_mgL, DOC_mgL, NH4_mgL = "NH4_mgNL", NO3_mgL = "NO3_mgNL", SRP_mgL = TP_mgPL, SO4_mgL = SO4_mgSL) %>% 
            # mistake in "raw data" excel sheet. They /1000 but didn't change the units
            mutate(SO4_mgL = SO4_mgL*1000)


ggplot(DOR_chem %>% 
         pivot_longer(Ca_mgL:SO4_mgL, names_to = "solute", values_to = "conc"), aes(y = log(conc+1), x = Date, color = WS)) +
  geom_point()+
  facet_grid(solute ~ WS, scales = "free_y")

##########
# COMBINE INTO DOR DF
#########

# Think times in here are messing up the joins - sometimes
DOR_Q.l.j <-  DOR_Q.l %>% 
          mutate(Date = as.character(Date))

DOR_chem.j <- DOR_chem %>% 
          mutate(Date = as.character(Date))

DOR.f <- DOR_Q.l.j %>% 
          full_join(DOR_chem.j, by = c("Date", "WS")) %>% 
          mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d"),
                 WS = as.factor(WS),
                 Site = "DOR") %>% 
          # filter(Date >= CTstart & Date <= CTend) %>% 
          select(Site, WS, Date, Q_Ls:SO4_mgL)
          



##########
# EXPORT DATAFRAME
#########

write_csv(DOR.f, file.path(here::here("data/JMHnewMungedDat/"), "01_DORcomb.csv"))


