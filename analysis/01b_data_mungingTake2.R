# This script calculates flow weighted means for all unmanaged sites
# JMH started 30 Jan 2021




##-------------------
## required packages
##-------------------
library(here)
library(readr)
library(tidyverse)
library(readxl)


##-------------------
## Load data
##-------------------

# Common time period
CTstart <- as.POSIXct(paste0("1985-10-31", format = "%Y-%m-%d"))
CTend <- as.POSIXct(paste0("2010-10-31", format = "%Y-%m-%d"))
# 9130 days

############
# ELA
############

# units on Q are m3/s
# units for solutes variable in ELA stream chemistry.xls::Units
# no info on qualifiers

############
# GET CONC DATA
############

# ELA EIF
ELA.eif <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                          "ELA EIF discharge.xlsx"),1) %>% 
        mutate_at(vars(data_set:station,qualifier), factor) %>% 
  mutate(mean_date = as.POSIXct(mean_date, format = "%Y-%m-%d")) %>% 
  # for some reason I can only get the start date if I have >= the day before i start
  filter(mean_date >= CTstart & mean_date <= CTend) %>% 
  mutate(dateC = as.character(as.Date(mean_date))) %>% 
  rename("Q_m3s" = "mean_daily_discharge")

ggplot(ELA.eif, aes(y = Q_m3s, mean_date)) +
  geom_point()

# ELA NEIF
ELA.neif <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                       "ELA NEIF discharge.xlsx"),1) %>% 
  mutate_at(vars(data_set:station,qualifier), factor) %>% 
  mutate(mean_date = as.POSIXct(mean_date, format = "%Y-%m-%d")) %>% 
  filter(mean_date >= CTstart & mean_date <= CTend) %>% 
  mutate(dateC = as.character(as.Date(mean_date)))%>% 
  rename("Q_m3s" = "mean_daily_discharge")

ggplot(ELA.neif, aes(y = Q_m3s, mean_date)) +
  geom_point()

# ELA NWIF
ELA.nwif <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                        "ELA NWIF discharge.xlsx"),1) %>% 
  mutate_at(vars(data_set:station,qualifier), factor) %>% 
  mutate(mean_date = as.POSIXct(mean_date, format = "%Y-%m-%d")) %>% 
  filter(mean_date >= CTstart & mean_date <= CTend)%>% 
  mutate(dateC = as.character(as.Date(mean_date)))%>% 
  rename("Q_m3s" = "mean_daily_discharge")

ggplot(ELA.nwif, aes(y = Q_m3s, mean_date)) +
  geom_point()
############
# SOLUTE DATA
############

# ELA chemistry
# 1970-04-07 to 2017-03-23
# codes in solute columns
    # -1111 analysis not conducted
    # -1 below minimum detection
    # 0 analysis not requested
    # -200 no sample available
    # other wise not dealing with detection limits ------ WAS THIS THE GROUP DECISION?
# Variable NAs ~ 40-90 of 5163
# --------- WHAT ARE THE SUBLOC AND DO I WANT ANY

BadCodeNAfun <- function(x){
  x <- ifelse(x <= 0, as.numeric("NA"), x)
  x
}


ELA.chem <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                        "ELA stream chemistry.xlsx"),2)%>% 
  mutate(SAMPLED = as.POSIXct(SAMPLED, format = "%Y-%m-%d")) %>% 
  filter(SAMPLED >= CTstart & SAMPLED <= CTend) %>%
  mutate_at(vars(TYPE:STATION), factor) %>% 
  # select solutes of interest
  #NH4 was labeled as NH3 in excel sheet:units
  select("SAMPLED", "SUBLOC", "STATION","CA","DOC","NH4","NO3","TDP","SO4") %>% 
  rename(CA_mgL = "CA", DOC_uM = "DOC", NH4_ugL = "NH4",NO3_ugL ="NO3",TDP_ugL = "TDP",SO4_mgL = "SO4") %>% 
  # select focal catchments
  filter(STATION %in% c("EIF", "NWIF", "NEIF")) %>% 
  # remove solute values that are codes (all below zero)
  mutate(across(CA_mgL:SO4_mgL, BadCodeNAfun)) %>% #ifelse(. <= 0, as.numeric("NA"),.)
  mutate(dateC = as.character(as.Date(SAMPLED))) %>% 
  # more than one sample on some days - average them
  group_by(dateC, STATION) %>% 
  summarize(across(c(CA_mgL:SO4_mgL), mean, na.rm = TRUE)) %>% 
  mutate(mean_date = as.POSIXct(dateC, format = "%Y-%m-%d"))

ggplot(ELA.chem %>% 
         pivot_longer(CA_mgL:SO4_mgL, names_to = "solutes", values_to = "values"), aes(y = values, x = mean_date, color = STATION)) +
  geom_point() +
  facet_wrap(vars(solutes), scales = "free_y")

ELA.chemL <- ELA.chem %>% 
  pivot_longer(cols = CA_mgL:SO4_mgL, names_to = "solute", values_to = "conc") %>% 
  mutate(solute = as.factor(solute))

############
# COMBINE Q AND CHEM
# CALCULATE FWMC
############

#EIF
ELA.eif2 <- ELA.eif %>% 
  left_join(ELA.chemL %>% 
              filter(STATION == "EIF"), by = "dateC") %>% 
  select(dateC, date = "mean_date.x", Q_m3s, solute, conc) %>% 
  mutate(station = "EIF",
         Ym = as.character(strftime(date, format = "%Y-%m")), 
         # using eq here: https://ncwqr.files.wordpress.com/2017/06/d-time-weighted-and-flow-weighted-mean-concentrations.pdf
        FWMCtop = conc * (Q_m3s*60*60*24) * 1, #[conc * (flow converted to d) * 1day]
         FWMCbottom = (Q_m3s*60*60*24) * 1,
         # don't want to divide "top" by a larger number of days of bottom
         FWMCbottomUse = ifelse(!is.na(FWMCtop), FWMCbottom, as.numeric("NA"))) %>% # [(flow conv to d) * 1 day]
  # AGGREGATE TO MONTHLY - summing
  group_by(Ym, solute, station) %>% 
  summarize(across(c(FWMCtop, FWMCbottomUse), sum, na.rm = T)) %>% 
  mutate(FWMC = FWMCtop/FWMCbottomUse) %>% 
  mutate(date = as.POSIXct(paste0(Ym,"-01"), format = "%Y-%m-%d")) %>% 
  #remove 301 NA's in solute
  filter(solute != "NA")

ggplot(ELA.eif2, aes(y = FWMC, x = date, color = solute)) +
  geom_point() +
  facet_wrap(vars(solute), scales = "free_y")

#NEIF
ELA.neif2 <- ELA.neif %>% 
  full_join(ELA.chemL %>% 
              filter(STATION == "NEIF"), by = "dateC")%>% 
  select(dateC, date = "mean_date.x", Q_m3s, solute, conc) %>% 
  mutate(station = "NEIF",
         Ym = as.character(strftime(date, format = "%Y-%m")), 
         # using eq here: https://ncwqr.files.wordpress.com/2017/06/d-time-weighted-and-flow-weighted-mean-concentrations.pdf
         FWMCtop = conc * (Q_m3s*60*60*24) * 1, #[conc * (flow converted to d) * 1day]
         FWMCbottom = (Q_m3s*60*60*24) * 1,
         # don't want to divide "top" by a larger number of days of bottom
         FWMCbottomUse = ifelse(!is.na(FWMCtop), FWMCbottom, as.numeric("NA"))) %>% # [(flow conv to d) * 1 day]
  group_by(Ym, solute, station) %>% 
  summarize(across(c(FWMCtop, FWMCbottomUse), sum, na.rm = T)) %>% 
  mutate(FWMC = FWMCtop/FWMCbottomUse) %>% 
  mutate(date = as.POSIXct(paste0(Ym,"-01"), format = "%Y-%m-%d")) %>% 
  #remove NA's in solute
  filter(solute != "NA")

ggplot(ELA.neif2, aes(y = FWMC, x = date, color = solute)) +
  geom_point() +
  facet_wrap(vars(solute), scales = "free_y")

ELA.nwif2 <- ELA.nwif %>% 
  full_join(ELA.chemL %>% 
              filter(STATION == "NWIF"), by = "dateC")%>% 
  select(dateC, date = "mean_date.x", Q_m3s, solute, conc) %>% 
  mutate(station = "NWIF",
         Ym = as.character(strftime(date, format = "%Y-%m")), 
         # using eq here: https://ncwqr.files.wordpress.com/2017/06/d-time-weighted-and-flow-weighted-mean-concentrations.pdf
         FWMCtop = conc * (Q_m3s*60*60*24) * 1, #[conc * (flow converted to d) * 1day]
         FWMCbottom = (Q_m3s*60*60*24) * 1,
         # don't want to divide "top" by a larger number of days of bottom
         FWMCbottomUse = ifelse(!is.na(FWMCtop), FWMCbottom, as.numeric("NA"))) %>% # [(flow conv to d) * 1 day]
  group_by(Ym, solute, station) %>% 
  summarize(across(c(FWMCtop, FWMCbottomUse), sum, na.rm = T)) %>% 
  mutate(FWMC = FWMCtop/FWMCbottomUse) %>% 
  mutate(date = as.POSIXct(paste0(Ym,"-01"), format = "%Y-%m-%d")) %>% 
  #remove NA's in solute
  filter(solute != "NA")

ggplot(ELA.nwif2, aes(y = FWMC, x = date, color = solute)) +
  geom_point() +
  facet_wrap(vars(solute), scales = "free_y")

########## NEED TO CHECK THIS #########

                            