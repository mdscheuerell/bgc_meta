# This script calculates flow weighted means for all unmanaged sites
# JMH started 30 Jan 2021, updated 18 May 21, 2 June 2021




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
  # filter(mean_date >= CTstart & mean_date <= CTend) %>% 
  mutate(dateC = as.character(as.Date(mean_date))) %>% 
  rename("Q_m3s" = "mean_daily_discharge")

ggplot(ELA.eif, aes(y = Q_m3s, mean_date)) +
  geom_point()

# ELA NEIF
ELA.neif <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                       "ELA NEIF discharge.xlsx"),1) %>% 
  mutate_at(vars(data_set:station,qualifier), factor) %>% 
  mutate(mean_date = as.POSIXct(mean_date, format = "%Y-%m-%d")) %>% 
  # filter(mean_date >= CTstart & mean_date <= CTend) %>% 
  mutate(dateC = as.character(as.Date(mean_date)))%>% 
  rename("Q_m3s" = "mean_daily_discharge")

ggplot(ELA.neif, aes(y = Q_m3s, mean_date)) +
  geom_point()

# ELA NWIF
ELA.nwif <- readxl::read_xlsx(file.path(here::here("data/NewDataFromIrena20210130/New MAR Data/Raw Data Files/ELA"), 
                                        "ELA NWIF discharge.xlsx"),1) %>% 
  mutate_at(vars(data_set:station,qualifier), factor) %>% 
  mutate(mean_date = as.POSIXct(mean_date, format = "%Y-%m-%d")) %>% 
  # filter(mean_date >= CTstart & mean_date <= CTend)%>% 
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
  # filter(SAMPLED >= CTstart & SAMPLED <= CTend) %>%
  mutate_at(vars(TYPE:STATION), factor) %>% 
  # select solutes of interest
  #NH4 was labeled as NH3 in excel sheet:units
  select("SAMPLED", "SUBLOC", "STATION","CA","DOC","NH4","NO3","TDP","SO4") %>% 
  rename(Ca_mgL = "CA", DOC_uM = "DOC", NH4_ugL = "NH4",NO3_ugL ="NO3",TDP_ugL = "TDP",SO4_mgL = "SO4") %>% 
  mutate(DOC_mgL = DOC_uM * 12.01 /1000,
         NH4_mgL = NH4_ugL / 1000,
         NO3_mgL = NO3_ugL / 1000,
         SRP_mgL = TDP_ugL / 1000,
         SO4_mgL = SO4_mgL * (32.065/90.06)) %>% 
  # select focal solutes
  select("SAMPLED", "SUBLOC", "STATION", "Ca_mgL", DOC_mgL, NH4_mgL, NO3_mgL, SRP_mgL, SO4_mgL) %>% 
  # select focal catchments
  filter(STATION %in% c("EIF", "NWIF", "NEIF")) %>% 
  # remove solute values that are codes (all below zero)
  mutate(across(Ca_mgL:SO4_mgL, BadCodeNAfun)) %>% #ifelse(. <= 0, as.numeric("NA"),.)
  mutate(dateC = as.character(as.Date(SAMPLED))) %>% 
  # more than one sample on some days - average them
  group_by(dateC, STATION) %>% 
  summarize(across(c(Ca_mgL:SO4_mgL), mean, na.rm = TRUE)) %>% 
  mutate(Date = as.POSIXct(dateC, format = "%Y-%m-%d")) %>% 
  ungroup() %>% 
  select(Date, WS = "STATION", Ca_mgL:SO4_mgL)

ggplot(ELA.chem %>% 
         pivot_longer(Ca_mgL:SO4_mgL, names_to = "solutes", values_to = "values"), aes(y = values, x = Date, color = WS)) +
  geom_point() +
  facet_grid(solutes ~ WS, scales = "free_y")

ggpairs(ELA.chem[,3:8])

# ELA.chemL <- ELA.chem %>% 
#   pivot_longer(cols = CA_mgL:SO4_mgL, names_to = "solute", values_to = "conc") %>% 
#   mutate(solute = as.factor(solute))


##########
# COMBINE INTO ELA DF
#########
ELA.Q <- rbind(ELA.eif, ELA.neif, ELA.nwif) %>% 
  mutate(subloc2 = as.factor(ifelse(sublocation == "ET","EIF",
                            ifelse(sublocation == "NET", "NEIF",
                              ifelse(sublocation == "NWT", "NWIF","blah")))),
         Site = as.factor("ELA"),
         Q_Ls = Q_m3s * 1000,
         mean_date = as.Date(mean_date)) %>% 
  select(Site, WS = "subloc2", Date = "mean_date", Q_Ls)

ELA.f <- ELA.Q %>% 
  left_join(ELA.chem, by = c("Date", "WS"))

ggplot(ELA.f %>% 
         pivot_longer(cols = c(Q_Ls:SO4_mgL), names_to = "meas", values_to = "value"), aes(y = value, x = Date)) +
  geom_point() +
  facet_grid(meas ~ WS,  scales = "free_y")

# ggpairs(ELA.f[,c(4,7:12)])

##########
# EXPORT DATAFRAME
#########

write_csv(ELA.f, file.path(here::here("data/JMHnewMungedDat/"), "01_ELAcomb.csv"))










